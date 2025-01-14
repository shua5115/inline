const std = @import("std");
const tokens = @import("tokens.zig");
const lexing = @import("lexing.zig");
const ast = @import("ast.zig");

const Token = tokens.Token;
const TokenType = tokens.TokenType;
const TokenLiteral = tokens.TokenLiteral;
const Lexer = lexing.Lexer;

const OpPriority = enum(u8) {
    LOWEST = 0,
    COMMA,
    ASSIGN,
    OR,
    AND,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    MOD,
    POW,
    CONCAT,
    PREFIX,
    CALL,
    INDEX,
};

fn op_priority(tt: TokenType) OpPriority {
    return switch(tt) {
        .COMMA => OpPriority.COMMA,
        .ASSIGN => OpPriority.ASSIGN,
        .OR => OpPriority.OR,
        .AND => OpPriority.AND,
        .EQ => OpPriority.EQUALS,
        .NEQ => OpPriority.EQUALS,
        .LT => OpPriority.LESSGREATER,
        .GT => OpPriority.LESSGREATER,
        .LE => OpPriority.LESSGREATER,
        .GE => OpPriority.LESSGREATER,
        .PLUS => OpPriority.SUM,
        .MINUS => OpPriority.SUM,
        .ASTERISK => OpPriority.PRODUCT,
        .SLASH => OpPriority.PRODUCT,
        .MOD => OpPriority.MOD,
        .CARAT => OpPriority.POW,
        .CONCAT => OpPriority.CONCAT,
        .NOT => OpPriority.PREFIX,
        .HASH => OpPriority.PREFIX,
        .META => OpPriority.PREFIX,
        .COLON => OpPriority.PREFIX,
        .AT => OpPriority.PREFIX,
        .LPAREN => OpPriority.CALL,
        .DOT => OpPriority.INDEX,
        else => OpPriority.LOWEST
    };
}

pub const ParseError = error {
    SYNTAX_ERROR,
    TYPE_MISMATCH,
    INVALID_PREFIX,
    INVALID_INFIX,
    MALFORMED_NUMBER,
    INVALID_ARGLIST,
    MISSING_FN_BODY,
};

pub const Parser = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    lex: *Lexer,
    literals: *std.ArrayList([]const u8),
    nodes: *std.ArrayList(ast.Node),
    errors: std.ArrayList(ParseError),

    prev: Token,
    cur: Token,
    next: Token,

    pub fn init(
        alloc: std.mem.Allocator,
        lex: *Lexer,
        literals: *std.ArrayList([]const u8),
        nodes: *std.ArrayList(ast.Node)
    ) !Parser {
        const invalid_token = Token{.tokentype=TokenType.ILLEGAL, .literal=.{.static=""}};
        var p = Parser{
            .allocator = alloc,
            .lex = lex,
            .literals = literals,
            .nodes = nodes,
            .errors = std.ArrayList(ParseError).init(alloc),
            .prev = invalid_token,
            .cur = invalid_token,
            .next = invalid_token,
        };

        try p.nextToken();
        try p.nextToken();

        return p;
    }

    pub fn deinit(self: *Self) void {
        self.errors.deinit();
    }

    /// Advances the cursor to the next token.
    fn nextToken(self: *Self) !void {
        self.prev = self.cur;
        self.cur = self.next;
        self.next = try self.lex.nextToken();
    }

    /// If the next token is of type tt, then the cursor will advance
    /// to make tt the current token.
    fn nextTokenOfType(self: *Self, tt: TokenType) !bool {
        if (self.next.tokentype == tt) {
            try self.nextToken();
            return true;
        }
        return false;
    }

    /// Stores a node, returning its unique index.
    fn pushNode(self: *Self, node: ast.Node) !ast.AstIndex {
        // try self.nodes.append(node);
        const ptr = try self.nodes.addOne();
        ptr.* = node;
        return @enumFromInt(self.nodes.items.len-1);
    }

    pub fn getNode(self: *Self, node_index: ast.AstIndex) ?*ast.Node {
        const i: usize = @intFromEnum(node_index);
        if (i >= self.nodes.items.len) return null;
        return &self.nodes.items[i];
    }

    /// Errors if the index is error or FINAL
    fn expect(index: anyerror!ast.AstIndex) anyerror!ast.AstIndex {
        if ((try index) == .FINAL) return ParseError.SYNTAX_ERROR;
        return index;
    }

    // PARSERS
    // Convention:
    // Parser functions start with cur pointing to the token that begins the statement.
    // For example:
    // Block Statement => (); => cur starts at '('
    // Not Expression => !a => cur starts at '!'
    // Parser functions end with cur pointing after the last token in the group.
    // For example:
    // Block Statement => (); => cur one past ';' on end
    // Not Expression => !a => cur one past 'a' on end

    // Chains of statements:
    // Each ast node represents an expression.
    // The distinction between statements and expressions only exists during parsing.
    // Each ast node contains a field "next_index", which allows nodes to form singly linked lists.
    // "next_index" is an unbounded enum, which has a FINAL value as a sentinel
    // When a node contains points to a chain of statements, it only stores the "head" of the chain's linked list.
    // The parser for the node which contains the list is responsible for ensuring proper linkage.

    /// Parses until the input reaches EOF, returning the root index of the AST.
    pub fn parseProgram(self: *Self) !ast.AstIndex {
        var node = ast.Node{.expr = .{.Block = .{.start_index = .FINAL}}};
        
        if (self.cur.tokentype == .EOF) {
            return self.pushNode(node);
        }

        var cur_index = try expect(self.parseStatement());
        node.expr.Block.start_index = cur_index;

        while (self.cur.tokentype != .EOF) {
            const next_index = try expect(self.parseStatement());
            const cur_stmt = self.getNode(cur_index) orelse return ParseError.SYNTAX_ERROR;
            cur_stmt.next_index = next_index;
            cur_index = next_index;
        }
        
        return self.pushNode(node);
    }

    // STATEMENT PARSERS

    fn parseStatement(self: *Self) !ast.AstIndex {
        return try switch (self.cur.tokentype) {
            TokenType.CARAT => self.parseBreakStatement(),
            TokenType.RETURN => self.parseReturnStatement(),
            else => self.parseExpressionStatement()
        };
    }

    fn parseBreakStatement(self: *Self) !ast.AstIndex {
        std.debug.assert(self.cur.tokentype == .CARAT);
        try self.nextToken();

        const node = ast.Node{.expr = .{ .Break = .{
            .rhs_index = switch(self.next.tokentype) {
                TokenType.RPAREN, TokenType.SEMICOLON, TokenType.EOF => ast.AstIndex.FINAL,
                else => try self.parseExpression(OpPriority.LOWEST)
            }
        }}};

        if (self.cur.tokentype == .SEMICOLON) {
            try self.nextToken();
        }
        return try self.pushNode(node);
    }

    fn parseReturnStatement(self: *Self) !ast.AstIndex {
        std.debug.assert(self.cur.tokentype == .RETURN);
        try self.nextToken();

        const node = ast.Node{.expr = .{ .Return = .{
            .rhs_index = switch(self.next.tokentype) {
                TokenType.RPAREN, TokenType.SEMICOLON, TokenType.EOF => ast.AstIndex.FINAL,
                else => try self.parseExpression(OpPriority.LOWEST)
            }
        }}};

        if (self.cur.tokentype == .SEMICOLON) {
            try self.nextToken();
        }
        return try self.pushNode(node);
    }

    fn parseExpressionStatement(self: *Self) !ast.AstIndex {
        const node = try self.parseExpression(OpPriority.LOWEST);
        if (self.cur.tokentype == .SEMICOLON) {
            try self.nextToken();
        }
        return node;
    }

    // EXPRESSION PARSERS

    fn prefixParseFn(tt: TokenType) ?*const fn(*Self)anyerror!ast.AstIndex {
        return switch(tt) {
            TokenType.IDENT => parseIdentifier,
            TokenType.NUMBER => parseNumberLiteral,
            TokenType.DOT => parseNumberLiteral,
            TokenType.NOT, 
            TokenType.PLUS,
            TokenType.MINUS,
            TokenType.HASH,
            TokenType.META,
            TokenType.COLON => parsePrefixExpression,
            TokenType.AT => parseGlobal,
            TokenType.LPAREN => parseBlock,
            TokenType.LBRACKET => parseFunctionLiteral,
            TokenType.STRING => parseStringLiteral,
            TokenType.LBRACE => parseTableLiteral,
            else => null
        };
    }

    fn infixParseFn(tt: TokenType) ?*const fn(*Self, ast.AstIndex)anyerror!ast.AstIndex {
        return switch (tt) {
            TokenType.ASSIGN,
            TokenType.OR,
            TokenType.AND,
            TokenType.EQ,
            TokenType.NEQ,
            TokenType.LT,
            TokenType.GT,
            TokenType.LE,
            TokenType.GE,
            TokenType.PLUS,
            TokenType.MINUS,
            TokenType.ASTERISK,
            TokenType.SLASH,
            TokenType.MOD,
            TokenType.CARAT,
            TokenType.CONCAT,
            TokenType.DOT => parseInfixExpression,
            TokenType.LPAREN => parseCallExpression,
            else => null
        };
    }

    fn parseExpression(self: *Self, precedence: OpPriority) !ast.AstIndex {
        const prefixParser = prefixParseFn(self.cur.tokentype) orelse {
            std.debug.print("INVALID PREFIX: {s}\n", .{@tagName(self.cur.tokentype)});
            return ParseError.INVALID_PREFIX;
        };
        var left: ast.AstIndex = try expect(prefixParser(self));

        while (self.cur.tokentype != TokenType.SEMICOLON and @intFromEnum(precedence) < @intFromEnum(op_priority(self.cur.tokentype))) {
            const infixParser = infixParseFn(self.cur.tokentype) orelse return left;
            // leftmost expression becomes one with the highest precedence
            left = try expect(infixParser(self, left));
        }

        return left;
    }

    // PREFIX EXPRESSIONS

    fn parseIdentifier(self: *Self) !ast.AstIndex {
        std.debug.assert(self.cur.tokentype == .IDENT);
        const lit_index = self.cur.literal.index;
        try self.nextToken();
        return self.pushNode(ast.Node{.expr = .{ .Identifier = 
            .{ .literal_index=lit_index }
        }});
    }

    fn parseNumberLiteral(self: *Self) !ast.AstIndex {
        std.debug.assert(self.cur.tokentype == .NUMBER);
        const literal_index = self.cur.literal.index;
        const buf = self.literals.items[literal_index];
        const val = std.fmt.parseFloat(f64, buf) catch blk: {
            break :blk @as(f64, @floatFromInt(std.fmt.parseInt(i64, buf, 0) catch {
                return ParseError.MALFORMED_NUMBER;
            }));
        };
        try self.nextToken();

        return self.pushNode(ast.Node{.expr = .{
            .NumberLiteral = .{.value=val, .literal_index=literal_index}
        }});
    }

    fn parsePrefixExpression(self: *Self) !ast.AstIndex {
        const kind = self.cur.tokentype;
        try self.nextToken();
        const node = ast.Node{.expr=.{.PrefixExpression = .{
            .op = kind,
            .rhs_index = try self.parseExpression(.PREFIX),
        }}};
        return self.pushNode(node);
    }

    fn parseGlobal(self: *Self) !ast.AstIndex {
        std.debug.assert(self.cur.tokentype == .AT);
        try self.nextToken();
        return self.pushNode(ast.Node{.expr = .{
            .Global = .{.rhs_index = try self.parseExpression(.LOWEST)}
        }});
    }

    fn parseBlock(self: *Self) !ast.AstIndex {
        std.debug.assert(self.cur.tokentype == .LPAREN);
        var node = ast.Node{.expr = .{ .Block = .{
            .start_index = .FINAL
        }}};
        try self.nextToken();
        if (self.cur.tokentype == .RPAREN) {
            try self.nextToken();
            return self.pushNode(node);
        }

        var cur_index = try self.parseStatement();
        node.expr.Block.start_index = cur_index;

        while (self.cur.tokentype != .RPAREN and self.cur.tokentype != .EOF) {
            const next_index = try self.parseStatement();
            // getNode must be after parseStatement b/c node creation invalidates node pointers.
            const cur_stmt = self.getNode(cur_index).?;
            cur_stmt.next_index = next_index;
            cur_index = next_index;
        }
        try self.nextToken();
        
        return self.pushNode(node);
    }

    fn parseFunctionLiteral(self: *Self) !ast.AstIndex {
        std.debug.assert(self.cur.tokentype == .LBRACKET);
        var node = ast.Node{.expr = .{ .FunctionLiteral = .{
            .params_index = .FINAL,
            .body_index = .FINAL,
        }}};

        try self.nextToken();
        // Parse Args
        {
            var prev_node: ?*ast.Node = null;
            while (self.cur.tokentype != .RBRACKET and self.cur.tokentype != .EOF) {
                const ident_node: ast.Node = switch(self.cur.tokentype) {
                    .IDENT => ast.Node{.expr = .{ .Identifier = .{
                        .literal_index = self.cur.literal.index
                    }}},
                    .ELLIPSIS => ast.Node{.expr = .{ .Ellipsis = .{} }},
                    else => return ParseError.INVALID_ARGLIST
                };
                const is_ellipsis = (ident_node.expr == .Ellipsis);
                const cur_index = try self.pushNode(ident_node);
                if(node.expr.FunctionLiteral.params_index == .FINAL) node.expr.FunctionLiteral.params_index = cur_index;
                if (prev_node) |n| {
                    n.next_index = cur_index;
                }
                prev_node = self.getNode(cur_index).?;
                try self.nextToken();
                switch (self.cur.tokentype) {
                    .COMMA => if (is_ellipsis) { return ParseError.INVALID_ARGLIST; } else { try self.nextToken(); },
                    .RBRACKET => {},
                    else => return ParseError.INVALID_ARGLIST
                }
            }
        }

        try self.nextToken();
        if (self.cur.tokentype != .LPAREN) return ParseError.MISSING_FN_BODY;

        try self.nextToken();
        if (self.cur.tokentype == .RPAREN) {
            // empty function body
            try self.nextToken();
            return self.pushNode(node);
        }

        // Read function body
        var cur_index = try self.parseStatement();
        node.expr.FunctionLiteral.body_index = cur_index;

        while (self.cur.tokentype != .RPAREN and self.cur.tokentype != .EOF) {
            const next_index = try self.parseStatement();
            // getNode must be after parseStatement b/c node creation invalidates node pointers.
            const cur_stmt = self.getNode(cur_index).?;
            cur_stmt.next_index = next_index;
            cur_index = next_index;
        }
        // go one past ')'
        try self.nextToken();

        return self.pushNode(node);
    }

    fn parseStringLiteral(self: *Self) !ast.AstIndex {
        std.debug.assert(self.cur.tokentype == .STRING);
        const lit_index = self.cur.literal.index;
        try self.nextToken();
        return self.pushNode(ast.Node{.expr = .{
            .StringLiteral = .{.literal_index = lit_index}
        }});
    }

    fn parseTableLiteral(self: *Self) !ast.AstIndex {
        _ = self;
        unreachable;
    }

    // INFIX EXPRESSIONS

    fn parseInfixExpression(self: *Self, left: ast.AstIndex) !ast.AstIndex {
        var node = ast.Node{.expr = .{ .InfixExpression = .{
            .lhs_index = left,
            .op = self.cur.tokentype,
            .rhs_index = undefined,
        }}};
        const precedence = op_priority(self.cur.tokentype);
        try self.nextToken();
        node.expr.InfixExpression.rhs_index = try self.parseExpression(precedence);

        return self.pushNode(node);
    }

    fn parseCallExpression(self: *Self, left: ast.AstIndex) !ast.AstIndex {
        var node = ast.Node{.expr = .{ .CallExpression = .{
            .lhs_index = left,
            .args_index = .FINAL,
        }}};
        
        try self.nextToken();
        if (self.cur.tokentype == .RPAREN) {
            try self.nextToken();
            return self.pushNode(node);
        }

        var cur_index = try self.parseExpression(.LOWEST);
        node.expr.CallExpression.args_index = cur_index;

        while (self.cur.tokentype != .RPAREN and self.cur.tokentype != .EOF) {
            if (self.cur.tokentype != .COMMA) return ParseError.INVALID_ARGLIST;
            try self.nextToken();
            const next_index = try self.parseExpression(.LOWEST);
            // getNode must be after parseStatement b/c node creation invalidates node pointers.
            const cur_stmt = self.getNode(cur_index).?;
            cur_stmt.next_index = next_index;
            cur_index = next_index;
        }
        try self.nextToken();
        
        return self.pushNode(node);
    }
};