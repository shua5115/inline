const std = @import("std");
const testing = @import("testing.zig");
const tokens = @import("tokens.zig");

const Token = tokens.Token;
const TokenType = tokens.TokenType;
const TokenLiteral = tokens.TokenLiteral;

const FiFo = std.fifo.LinearFifo(u8, .{.Dynamic});

pub fn isIdentChar(c: u8) bool {
    return c == '_' or std.ascii.isAlphanumeric(c);
}

pub fn isDigit(c: u8) bool {
    return '0' <= c and c <= '9';
}

pub const LexerError = error{
    INVALID_CHAR,
    SYNTAX_ERROR,
};

pub const Lexer = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    /// A store of all literal values read from the source.
    /// All stored slices must be free'd using the Lexer's allocator, not the ArrayList's.
    literals: *std.ArrayList([]const u8),
    input: std.io.AnyReader,
    cur: u8,
    next: u8,

    pub fn init(allocator: std.mem.Allocator, literals: *std.ArrayList([]const u8), reader: std.io.AnyReader) Lexer {
        var lexer = Lexer{
            .allocator = allocator,
            .literals = literals,
            .input = reader,
            .cur = undefined,
            .next = undefined,
        };

        lexer.cur = (reader.readByte() catch 0);
        lexer.next = (reader.readByte() catch 0);

        return lexer;
    }

    /// Reads one character from input.
    /// Stores next into cur, and read input into next.
    pub fn readNextChar(self: *Self) anyerror!void {
        self.cur = self.next;
        // if (self.next == 0) return;
        // var buf: [1]u8 = .{0};
        // const nread = self.input.read(buf[0..1]) catch 0;
        // self.next = if (nread == 0) 0 else buf[0];
        self.next = self.input.readByte() catch 0;
    }

    pub fn nextToken(self: *Self) anyerror!Token {
        var tok: Token = Token{.tokentype = TokenType.ILLEGAL, .literal = .{.static=""}};

        try self.skipWhitespace();
        const c = self.cur;
        switch (c) {
            '~' => tok = .{.tokentype=TokenType.NIL, .literal=.{.static="~"}},
            '=' => if (self.next == '=') {
                tok = .{.tokentype=TokenType.EQ, .literal=.{.static="=="}};
                try self.readNextChar();
            } else {
                tok = .{.tokentype=TokenType.ASSIGN, .literal=.{.static="="}};
            },
            '+' => tok = .{.tokentype=TokenType.PLUS, .literal=.{.static="+"}},
            '-' => tok = .{.tokentype=TokenType.MINUS, .literal=.{.static="-"}},
            '*' => tok = .{.tokentype=TokenType.ASTERISK, .literal=.{.static="*"}},
            '/' => tok = .{.tokentype=TokenType.SLASH, .literal=.{.static="/"}},
            '%' => tok = .{.tokentype=TokenType.MOD, .literal=.{.static="%"}},
            '^' => if(self.next == '^') {
                    tok = .{.tokentype=TokenType.RETURN, .literal=.{.static="^^"}};
                    try self.readNextChar();
                } else {
                    tok = .{.tokentype=TokenType.CARAT, .literal=.{.static="^"}};
                },
            '<' => if (self.next == '=') {
                    tok = .{.tokentype=TokenType.LE, .literal=.{.static="<="}};
                    try self.readNextChar();
                } else {
                    tok = .{.tokentype=TokenType.LT, .literal=.{.static="<"}};
                },
            '>' => if (self.next == '=') {
                    tok = .{.tokentype=TokenType.GE, .literal=.{.static=">="}};
                    try self.readNextChar();
                } else {
                    tok = .{.tokentype=TokenType.GT, .literal=.{.static=">"}};
                },
            '&' => tok = .{.tokentype = TokenType.AND, .literal = .{.static = "&"}},
            '|' => tok = .{.tokentype = TokenType.OR, .literal = .{.static = "|"}},
            '!' => if (self.next == '=') {
                    tok = .{.tokentype = TokenType.NEQ, .literal = .{.static = "!="}};
                    try self.readNextChar();
                } else {
                    tok = .{.tokentype = TokenType.NOT, .literal = .{.static = "!"}};
                },
            '?' => tok = .{.tokentype=TokenType.QUESTION, .literal=.{.static="?"}},
            '#' => tok = .{.tokentype=TokenType.HASH, .literal=.{.static="#"}},
            '@' => tok = .{.tokentype=TokenType.AT, .literal=.{.static="@"}},
            // Note to self: numeric literals beginnning with '.' must be corrected in the ast.
            '.' => if (self.next == '.') {
                    try self.readNextChar();
                    if (self.next == '.') {
                        try self.readNextChar();
                        tok = .{.tokentype = TokenType.ELLIPSIS, .literal = .{.static = "..."}};
                    } else {
                        tok = .{.tokentype = TokenType.CONCAT, .literal = .{.static=".."}};
                    }
                } else if (isDigit(self.next)) {
                    return .{.tokentype=TokenType.NUMBER, .literal=.{.index=try readNumber(self)}};
                } else {
                    tok = .{.tokentype = TokenType.DOT, .literal = .{.static = "."}};
                },
            ':' => if (self.next == ':') {
                    tok = .{.tokentype=TokenType.META, .literal=.{.static="::"}};
                    try self.readNextChar();
                } else {
                    tok = .{.tokentype=TokenType.COLON, .literal=.{.static=":"}};
                },
            ';' => tok = .{.tokentype=TokenType.SEMICOLON, .literal=.{.static=";"}},
            ',' => tok = .{.tokentype=TokenType.COMMA, .literal=.{.static=";"}},
            '(' => tok = .{.tokentype=TokenType.LPAREN, .literal=.{.static="("}},
            ')' => tok = .{.tokentype=TokenType.RPAREN, .literal=.{.static=")"}},
            '{' => tok = .{.tokentype=TokenType.LBRACE, .literal=.{.static="{"}},
            '}' => tok = .{.tokentype=TokenType.RBRACE, .literal=.{.static="}"}},
            '[' => tok = .{.tokentype=TokenType.LBRACKET, .literal=.{.static="["}},
            ']' => tok = .{.tokentype=TokenType.RBRACKET, .literal=.{.static="]"}},
            '"' => return .{.tokentype=TokenType.STRING, .literal=.{.index =try readStringLiteral(self)}},
            '\'' => return .{.tokentype=TokenType.STRING, .literal=.{.index=try readStringLiteral(self)}},
            0 => tok = .{.tokentype=TokenType.EOF, .literal=.{.static=""}},
            else => if (c == '_' or std.ascii.isAlphabetic(c)) {
                return .{.tokentype=TokenType.IDENT, .literal=.{.index=try readIdentifier(self)}};
            } else if (isDigit(c)) {
                // does not end on last character, so return early
                return .{.tokentype=TokenType.NUMBER, .literal=.{.index=try readNumber(self)}};
            }
        }
        try self.readNextChar();

        return tok;
    }

    pub fn skipWhitespace(self: *Self) !void {
        while (self.cur != 0 and std.ascii.isWhitespace(self.cur)) {
            try self.readNextChar();
        }
    }

    pub fn registerLiteral(self: *Self, string: []const u8) std.mem.Allocator.Error!usize {
        try self.literals.append(string);
        return self.literals.items.len-1;
    }

    /// Reads a string literal and returns its index in the ArrayList of literals.
    pub fn readStringLiteral(self: *Self) !usize {
        var buf = std.ArrayList(u8).init(self.allocator);
        defer buf.deinit();
        const start = self.cur;
        
        std.debug.assert(start == '"' or start == '\'');

        while (true) {
            try self.readNextChar();
            const c = self.cur;
            if (c == '\\') {
                try self.readNextChar();
                const c2 = switch (self.cur) {
                    'a' => 0x07,
                    'b' => 0x08,
                    'e' => 0x1B,
                    'f' => 0x0C,
                    'n' => 0x0A,
                    'r' => 0x0D,
                    't' => 0x09,
                    'v' => 0x0B,
                    '\\' => '\\',
                    '\'' => '\'',
                    '"' => '"',
                    '?' => 0x3F,
                    else => self.cur
                };
                try buf.append(c2);
            } else if (c == 0 or c == start) {
                try self.readNextChar();
                break;
            } else {
                try buf.append(c);
            }
        }
        const s: []const u8 = try buf.toOwnedSlice();
        return registerLiteral(self, s);
    }

    pub fn readIdentifier(self: *Self) !usize {
        var buf = std.ArrayList(u8).init(self.allocator);
        defer buf.deinit();
        while (isIdentChar(self.cur)) {
            try buf.append(self.cur);
            try self.readNextChar();
        }

        const s: []const u8 = try buf.toOwnedSlice();
        return registerLiteral(self, s);
    }

    pub fn readNumber(self: *Self) !usize {
        var buf = std.ArrayList(u8).init(self.allocator);
        defer buf.deinit();
        // First decimal part
        while (isDigit(self.cur) or self.cur == '.') {
            try buf.append(self.cur);
            try self.readNextChar();
        }
        // Optional `E'
        if (self.cur == 'E' or self.cur == 'e') {
            try buf.append(self.cur);
            try self.readNextChar();
            // Optional exponent sign
            if (self.cur == '+' or self.cur == '-') {
                try buf.append(self.cur);
                try self.readNextChar();
            }
        }
        // Second decimal part, or hex/binary/octal characters
        while (std.ascii.isAlphanumeric(self.cur)) {
            try buf.append(self.cur);
            try self.readNextChar();
        }

        const s: []const u8 = try buf.toOwnedSlice();
        return registerLiteral(self, s);
    }
};

// TESTS

const assert = testing.assert;
const assert_errmsg = testing.assert_errmsg;

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
    
    for (expected_tokens) |expected| {
        const token = try lexer.nextToken();

        // const val: []const u8 = if (token.has_value()) literals.items[token.literal.index] else std.mem.span(token.literal.static);
        // std.debug.print("Read {s}: {s}\n", .{@tagName(token.tokentype), val } );
        
        assert_errmsg(token.tokentype == expected.tt,
            "Type mismatch: {s} != {s}\n", .{@tagName(expected.tt), @tagName(token.tokentype)});

        if (token.has_value()) {
            assert_errmsg(std.mem.eql(u8, expected.lit, literals.items[token.literal.index]),
                "Value mismatch: {s} != {s}\n", .{expected.lit, literals.items[token.literal.index]});
        } else {
            assert_errmsg(std.mem.eql(u8, expected.lit, std.mem.span(token.literal.static)),
                "Value mismatch: {s} != {s}\n", .{expected.lit, std.mem.span(token.literal.static)});
        }
    }

    for (literals.items) |value| {
        alloc.free(value);
    }
}