const std = @import("std");
const testing = @import("testing.zig");
const tokens = @import("tokens.zig");

const Token = tokens.Token;
const TokenType = tokens.TokenType;
const LiteralIndex = tokens.LiteralIndex;

pub fn isIdentChar(c: u8) bool {
    return c == '_' or std.ascii.isAlphanumeric(c);
}

pub fn isDigit(c: u8) bool {
    return '0' <= c and c <= '9';
}

pub const LexerError = error{
    INVALID_CHAR,
    SYNTAX_ERROR,
    INCOMPLETE_STRING_LITERAL,
};

pub const Lexer = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    /// A store of all literal values read from the source.
    /// All stored slices must be free'd using the Lexer's allocator, not the ArrayList's.
    literals: *std.ArrayList([]const u8),
    literal_set: std.StringHashMap(LiteralIndex),
    input: std.io.AnyReader,
    cur: ?u8,
    next: ?u8,
    literal_buf: std.ArrayList(u8),

    pub fn init(allocator: std.mem.Allocator, literals: *std.ArrayList([]const u8), reader: std.io.AnyReader) Lexer {
        var lexer = Lexer{
            .allocator = allocator,
            .literals = literals,
            .literal_set = std.StringHashMap(LiteralIndex).init(allocator),
            .input = reader,
            .cur = undefined,
            .next = undefined,
            .literal_buf = std.ArrayList(u8).init(allocator),
        };

        lexer.cur = (reader.readByte() catch null);
        lexer.next = (reader.readByte() catch null);

        return lexer;
    }

    pub fn deinit(self: *Self) void {
        self.literal_buf.deinit();
        self.literal_set.deinit();
    }

    /// Reads one character from input.
    /// Stores next into cur, and read input into next.
    fn readNextChar(self: *Self) void {
        self.cur = self.next;
        // if (self.next == null) return;
        self.next = self.input.readByte() catch null;
    }

    pub fn nextToken(self: *Self) anyerror!Token {
        var tok: Token = Token{.tokentype = TokenType.ILLEGAL};
        const eof = Token{.tokentype = .EOF};
        self.skipWhitespace();
        while (self.cur == '-' and self.next == '-') {
            self.readNextChar();
            self.readNextChar();
            try self.skipComment();
            self.skipWhitespace();
        }
        const c = self.cur orelse return eof;
        switch (c) {
            '~' => tok = .{.tokentype=TokenType.NIL},
            '=' => if (self.next == '=') {
                tok = .{.tokentype=TokenType.EQ};
                self.readNextChar();
            } else {
                tok = .{.tokentype=TokenType.ASSIGN};
            },
            '+' => tok = .{.tokentype=TokenType.PLUS},
            '-' => if(self.next == '-') {
                    // all comments should be handled
                    unreachable;
                } else {
                    tok = .{.tokentype=TokenType.MINUS};
                },
            '*' => tok = .{.tokentype=TokenType.ASTERISK},
            '/' => tok = .{.tokentype=TokenType.SLASH},
            '%' => tok = .{.tokentype=TokenType.MOD},
            '^' => if(self.next == '^') {
                    tok = .{.tokentype=TokenType.BREAK};
                    self.readNextChar();
                } else {
                    tok = .{.tokentype=TokenType.CARAT};
                },
            '<' => if (self.next == '=') {
                    tok = .{.tokentype=TokenType.LE};
                    self.readNextChar();
                } else {
                    tok = .{.tokentype=TokenType.LT};
                },
            '>' => if (self.next == '=') {
                    tok = .{.tokentype=TokenType.GE};
                    self.readNextChar();
                } else {
                    tok = .{.tokentype=TokenType.GT};
                },
            '&' => tok = .{.tokentype = TokenType.AND},
            '|' => tok = .{.tokentype = TokenType.OR},
            '!' => if (self.next == '=') {
                    tok = .{.tokentype = TokenType.NEQ};
                    self.readNextChar();
                } else {
                    tok = .{.tokentype = TokenType.NOT};
                },
            '?' => tok = .{.tokentype=TokenType.QUESTION},
            '#' => tok = .{.tokentype=TokenType.HASH},
            '@' => tok = .{.tokentype=TokenType.AT},
            '.' => if (self.next == '.') {
                    self.readNextChar();
                    if (self.next == '.') {
                        self.readNextChar();
                        tok = .{.tokentype = TokenType.ELLIPSIS};
                    } else {
                        tok = .{.tokentype = TokenType.CONCAT};
                    }
                } else if (self.next != null and isDigit(self.next.?)) {
                    return .{.tokentype=TokenType.NUMBER, .literal_index = @truncate(try readNumber(self))};
                } else {
                    tok = .{.tokentype = TokenType.DOT};
                },
            ':' => switch(self.next orelse '\x00') {
                    ':' => {tok = .{.tokentype=TokenType.META}; self.readNextChar();},
                    '=' => {tok = .{.tokentype=TokenType.DEFINE}; self.readNextChar();},
                    else => tok = .{.tokentype=TokenType.COLON},
                },
            ';' => tok = .{.tokentype=TokenType.SEMICOLON},
            ',' => tok = .{.tokentype=TokenType.COMMA},
            '(' => tok = .{.tokentype=TokenType.LPAREN},
            ')' => tok = .{.tokentype=TokenType.RPAREN},
            '{' => tok = .{.tokentype=TokenType.LBRACE},
            '}' => tok = .{.tokentype=TokenType.RBRACE},
            '[' => if (self.next == '[' or self.next == '=') {
                    return .{.tokentype=TokenType.STRING, .literal_index=try readStringLiteral(self)};
                } else {
                    tok = .{.tokentype=TokenType.LBRACKET};
                },
            ']' => tok = .{.tokentype=TokenType.RBRACKET},
            '"' => return .{.tokentype=TokenType.STRING, .literal_index=try readStringLiteral(self)},
            '\'' => return .{.tokentype=TokenType.STRING, .literal_index=try readStringLiteral(self)},
            else => if (!isDigit(c) and isIdentChar(c)) {
                return .{.tokentype=TokenType.IDENT, .literal_index=try readIdentifier(self)};
            } else if (isDigit(c)) {
                // does not end on last character, so return early
                return .{.tokentype=TokenType.NUMBER, .literal_index=try readNumber(self)};
            }
        }
        self.readNextChar();

        return tok;
    }

    pub fn skipWhitespace(self: *Self) void {
        while (std.ascii.isWhitespace(self.cur orelse return)) {
            self.readNextChar();
        }
    }

    pub fn skipComment(self: *Self) !void {
        var level: usize = 0;
        var block_comment = false;
        if ((self.cur orelse return) == '[') {
            self.readNextChar();
            while ((self.cur orelse return) == '=') {
                level += 1;
                self.readNextChar();
            }
            if ((self.cur orelse return) == '[') {
                block_comment = true;
                self.readNextChar();
            }
        }
        if (block_comment) {
            try self.collectLongString(level);
            self.literal_buf.items.len = 0; // clear read comment
        } else {
            while (true) {
                const c = self.cur orelse return;
                if (c == '\n') {
                    self.readNextChar();
                    return;
                } else {
                    self.readNextChar();
                }
            }
        }
    }

    /// Register a literal contained within literal_buf.
    /// If the literal is already registered, the original will be returned.
    /// In both cases, the literal_buf will be cleared, but not free'd (so the allocated capacity can be reused).
    fn registerLiteral(self: *Self) std.mem.Allocator.Error!LiteralIndex {
        // Prevent overflow errors
        if (self.literals.items.len == std.math.maxInt(LiteralIndex)) return std.mem.Allocator.Error.OutOfMemory;
        if (self.literal_set.get(self.literal_buf.items)) |index| {
            self.literal_buf.items.len = 0;
            return index;
        }
        const string = try self.literal_buf.toOwnedSlice();
        try self.literals.append(string);
        const index: LiteralIndex = @truncate(self.literals.items.len - 1);
        try self.literal_set.put(string, index);
        return index;
    }

    /// Expects self.cur to be one after the starting brackets. Callers are expected to measure the level.
    fn collectLongString(self: *Self, level: usize) !void {
        var buf = &self.literal_buf;
        buf.items.len = 0; // clear buffer
        if (self.cur == '\n') self.readNextChar(); // skip first newline
        while (true) {
            const c = self.cur orelse return;
            if (c == ']') {
                try buf.append(c);
                self.readNextChar();
                var endlevel: usize = 0;
                while((self.cur orelse return) == '=') {
                    endlevel += 1;
                    try buf.append(self.cur.?);
                    self.readNextChar();
                }
                if ((self.cur orelse return) == ']') {
                    if (endlevel == level) {
                        buf.items.len -= endlevel+2; // string ended, remove the ']', '=', and ']'
                        self.readNextChar();
                        return;
                    }
                } else try buf.append(self.cur.?);
                // false alarm, keep the read characters, including the just-read non-']'
            } else {
                try buf.append(self.cur orelse return);
                self.readNextChar();
            }
        }
    }

    /// Reads a string literal and returns its index in the ArrayList of literals.
    pub fn readStringLiteral(self: *Self) !LiteralIndex {
        var buf = &self.literal_buf;
        buf.items.len = 0; // clear without freeing
        const start = self.cur orelse return LexerError.INCOMPLETE_STRING_LITERAL;
        std.debug.assert(start == '"' or start == '\'' or start == '[');
        var level: usize = 0;
        var block_comment = false;
        if (start == '[') {
            self.readNextChar();
            while ((self.cur orelse return LexerError.INCOMPLETE_STRING_LITERAL) == '=') {
                level += 1;
                self.readNextChar();
            }
            if ((self.cur orelse return LexerError.INCOMPLETE_STRING_LITERAL) == '[') {
                block_comment = true;
                self.readNextChar();
            } else return LexerError.INCOMPLETE_STRING_LITERAL;
        }
        if (block_comment) {
            try self.collectLongString(level);
        } else {
            while (true) {
                self.readNextChar();
                const c = self.cur orelse return LexerError.INCOMPLETE_STRING_LITERAL;
                if (c == '\\') {
                    self.readNextChar();
                    const c2 = switch (self.cur orelse return LexerError.INCOMPLETE_STRING_LITERAL) {
                        'a' => 0x07,
                        'b' => 0x08,
                        'f' => 0x0C,
                        'n' => 0x0A,
                        'r' => 0x0D,
                        't' => 0x09,
                        'v' => 0x0B,
                        '\\' => '\\',
                        '\'' => '\'',
                        '"' => '"',
                        '[' => '[',
                        ']' => ']',
                        '0' => 0,
                        else => if (isDigit(self.cur.?)) blk: {
                            var cbuf = [3:0]u8{0, 0, 0};
                            var i: u8 = 0;
                            while (i < cbuf.len and self.cur != null and isDigit(self.cur.?)) {
                                cbuf[i] = self.cur.?;
                                i += 1;
                            }
                            const cslice: [*:0]const u8 = cbuf[0..];
                            break :blk std.fmt.parseUnsigned(u8, std.mem.span(cslice), 10) catch unreachable;
                        } else self.cur.?
                    };
                    try buf.append(c2);
                } else if (c == start) {
                    self.readNextChar();
                    break;
                } else {
                    try buf.append(c);
                }
            }
        }
        return self.registerLiteral();
    }

    pub fn readIdentifier(self: *Self) !LiteralIndex {
        var buf = &self.literal_buf;
        buf.items.len = 0; // clear without freeing
        while (self.cur != null and isIdentChar(self.cur.?)) {
            try buf.append(self.cur.?);
            self.readNextChar();
        }

        return self.registerLiteral();
    }

    pub fn readNumber(self: *Self) !LiteralIndex {
        var buf = &self.literal_buf;
        buf.items.len = 0; // clear without freeing
        // First decimal part
        while (self.cur != null and (self.cur == '.' or isDigit(self.cur.?))) {
            try buf.append(self.cur.?);
            self.readNextChar();
        }
        // Optional `E'
        if (self.cur == 'E' or self.cur == 'e') {
            try buf.append(self.cur.?);
            self.readNextChar();
            // Optional exponent sign
            if (self.cur == '+' or self.cur == '-') {
                try buf.append(self.cur.?);
                self.readNextChar();
            }
        }
        // Second decimal part, or hex/binary/octal characters
        while (self.cur != null and std.ascii.isAlphanumeric(self.cur.?)) {
            try buf.append(self.cur.?);
            self.readNextChar();
        }

        return self.registerLiteral();
    }
};

// TESTS

const assert = testing.assert;
const assert_errmsg = testing.assert_errmsg;

test "sizeof Token" {
    std.debug.print("sizeof(Token) = {d}\n", .{@sizeOf(Token)});
}

test "parse tokens" {
    const source = 
    \\five=5;
    \\addition = 1 + 0.31416e1;
    \\dot_prefix = .2;
    \\?(fn(); i=i+1; i<=10 & ^);
    \\stringlit = "Hello, world!";
    ;

    const expected_tokens = [_]struct{tt: TokenType, lit: []const u8}{
        .{.tt=TokenType.IDENT, .lit="five"},
        .{.tt=TokenType.ASSIGN, .lit="="},
        .{.tt=TokenType.NUMBER, .lit="5"},
        .{.tt=TokenType.SEMICOLON, .lit=";"},

        .{.tt=TokenType.IDENT, .lit="addition"},
        .{.tt=TokenType.ASSIGN, .lit="="},
        .{.tt=TokenType.NUMBER, .lit="1"},
        .{.tt=TokenType.PLUS, .lit="+"},
        .{.tt=TokenType.NUMBER, .lit="0.31416e1"},
        .{.tt=TokenType.SEMICOLON, .lit=";"},

        .{.tt=TokenType.IDENT, .lit="dot_prefix"},
        .{.tt=TokenType.ASSIGN, .lit="="},
        .{.tt=TokenType.NUMBER, .lit=".2"},
        .{.tt=TokenType.SEMICOLON, .lit=";"},
        
        .{.tt=TokenType.QUESTION, .lit="?"},
        .{.tt=TokenType.LPAREN, .lit="("},
        .{.tt=TokenType.IDENT, .lit="fn"},
        .{.tt=TokenType.LPAREN, .lit="("},
        .{.tt=TokenType.RPAREN, .lit=")"},
        .{.tt=TokenType.SEMICOLON, .lit=";"},
        .{.tt=TokenType.IDENT, .lit="i"},
        .{.tt=TokenType.ASSIGN, .lit="="},
        .{.tt=TokenType.IDENT, .lit="i"},
        .{.tt=TokenType.PLUS, .lit="+"},
        .{.tt=TokenType.NUMBER, .lit="1"},
        .{.tt=TokenType.SEMICOLON, .lit=";"},
        .{.tt=TokenType.IDENT, .lit="i"},
        .{.tt=TokenType.LE, .lit="<="},
        .{.tt=TokenType.NUMBER, .lit="10"},
        .{.tt=TokenType.AND, .lit="&"},
        .{.tt=TokenType.CARAT, .lit="^"},
        .{.tt=TokenType.RPAREN, .lit=")"},
        .{.tt=TokenType.SEMICOLON, .lit=";"},

        .{.tt=TokenType.IDENT, .lit="stringlit"},
        .{.tt=TokenType.ASSIGN, .lit="="},
        .{.tt=TokenType.STRING, .lit="Hello, world!"},
        .{.tt=TokenType.SEMICOLON, .lit=";"},

        .{.tt=TokenType.EOF, .lit=""},
    };

    const alloc = std.testing.allocator;
    var literals = std.ArrayList([]const u8).init(alloc);
    defer literals.deinit();
    var input = std.fifo.LinearFifo(u8, .Dynamic).init(alloc);
    defer input.deinit();
    try input.write(source);
    
    var lexer = Lexer.init(alloc, &literals, input.reader().any());
    defer lexer.deinit();
    
    for (expected_tokens) |expected| {
        const token = try lexer.nextToken();

        // const val: []const u8 = if (token.has_value()) literals.items[token.literal.index] else std.mem.span(token.literal.static);
        // std.debug.print("Read {s}: {s}\n", .{@tagName(token.tokentype), val } );
        
        assert_errmsg(token.tokentype == expected.tt,
            "Type mismatch: {s} != {s}\n", .{@tagName(expected.tt), @tagName(token.tokentype)});

        if (token.has_value()) {
            assert_errmsg(std.mem.eql(u8, expected.lit, literals.items[token.literal_index]),
                "Value mismatch: {s} != {s}\n", .{expected.lit, literals.items[token.literal_index]});
        } else {
            
            assert_errmsg(std.mem.eql(u8, expected.lit, tokens.token_type_str(token.tokentype)),
                "Value mismatch: {s} != {s}\n", .{expected.lit, tokens.token_type_str(token.tokentype)});
        }
    }

    for (literals.items) |value| {
        alloc.free(value);
    }
}