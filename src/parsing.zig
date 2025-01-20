const std = @import("std");
const tokens = @import("tokens.zig");
const lexing = @import("lexing.zig");
const ast = @import("ast.zig");
const luavm = @import("luavm.zig");

const Token = tokens.Token;
const TokenType = tokens.TokenType;
const Lexer = lexing.Lexer;

const OpPriority = enum(u8) {
    LOWEST = 0,
    ASSIGN,
    COMMA,
    OR,
    AND,
    EQUALS,
    LESSGREATER,
    CONCAT,
    SUM,
    PRODUCT,
    MOD,
    LOOP,
    PREFIX,
    POW,
    CALL,
    INDEX,
};

fn op_priority(tt: TokenType) OpPriority {
    return switch(tt) {
        .ASSIGN => OpPriority.ASSIGN,
        .DEFINE => OpPriority.ASSIGN,
        .COMMA => OpPriority.COMMA,
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
        .CONCAT => OpPriority.CONCAT,
        .QUESTION => OpPriority.LOOP,
        .NOT => OpPriority.PREFIX,
        .HASH => OpPriority.PREFIX,
        .META => OpPriority.PREFIX,
        .COLON => OpPriority.PREFIX,
        .AT => OpPriority.PREFIX,
        .CARAT => OpPriority.POW,
        .LPAREN => OpPriority.CALL,
        .DOT => OpPriority.INDEX,
        else => OpPriority.LOWEST
    };
}

pub const ParseError = error {
    SYNTAX_ERROR,
    TYPE_MISMATCH,
    INVALID_DEFINE,
    INVALID_PREFIX,
    INVALID_INFIX,
    MALFORMED_NUMBER,
    MALFORMED_TABLE_LITERAL,
    INVALID_ARGLIST,
    MISSING_FN_BODY,
    INVALID_LOOP_ARGUMENT,
    MISSING_LOOP_BODY,
};

pub const Parser = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    lexer: Lexer,
    nodes: std.ArrayList(ast.Node), // potential optimization: use std.MultiArrayList
    lines: std.ArrayList(c_int), // map from node index to line number
    errors: std.ArrayList(ParseError),

    curline: c_int,
    cur: Token,
    next: Token,

    pub fn init(
        alloc: std.mem.Allocator,
        reader: std.io.AnyReader,
    ) !Parser {
        const invalid_token = Token{.tokentype=TokenType.ILLEGAL};
        var p = Parser{
            .allocator = alloc,
            .lexer = Lexer.init(alloc, reader),
            .nodes = std.ArrayList(ast.Node).init(alloc),
            .lines = std.ArrayList(c_int).init(alloc),
            .errors = std.ArrayList(ParseError).init(alloc),
            .curline = 0,
            .cur = invalid_token,
            .next = invalid_token,
        };

        try p.nextToken();
        try p.nextToken();

        return p;
    }

    pub fn deinit(self: *Self) void {
        self.errors.deinit();
        self.lines.deinit();
        self.nodes.deinit();
        self.lexer.deinit();
    }

    /// Advances the cursor to the next token.
    fn nextToken(self: *Self) !void {
        // self.prev = self.cur;
        self.cur = self.next;
        self.curline = self.lexer.curline;
        self.next = try self.lexer.nextToken();
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
        try self.nodes.append(node);
        try self.lines.append(self.curline);
        return @enumFromInt(self.nodes.items.len-1);
    }

    pub fn getNode(self: *Self, node_index: ast.AstIndex) ?*ast.Node {
        const i: usize = @intFromEnum(node_index);
        if (node_index == .FINAL or i >= self.nodes.items.len) return null;
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
    // Parser functions push the current node before the node's children.

    // Chains of statements:
    // Each ast node represents an expression, but can be a statement depending on context.
    // Blocks and function bodies both expect to contain a chain of statements.
    // Each ast node contains a field "next_index", which allows nodes to form singly linked lists.
    // "next_index" is an unbounded enum, which has a FINAL value as a sentinel
    // When a node contains a chain of statements, it only stores the "head" of the chain's linked list.
    // The parser for the node which contains the list is responsible for ensuring proper linkage.

    /// Parses until the input reaches EOF, returning the root index of the AST.
    pub fn parseProgram(self: *Self) !ast.AstIndex {
        const node_index = try self.pushNode(ast.Node{.expr = .{
            .Block = .{.start_index = .FINAL}
        }});
        
        if (self.cur.tokentype == .EOF) {
            return node_index;
        }
        
        var cur_index = try expect(self.parseStatement());
        self.getNode(node_index).?.expr.Block.start_index = cur_index;

        while (self.cur.tokentype != .EOF) {
            const next_index = try expect(self.parseStatement());
            const cur_stmt = self.getNode(cur_index).?;
            cur_stmt.next_index = next_index;
            cur_index = next_index;
        }
        
        return node_index;
    }

    // STATEMENT PARSERS

    fn parseStatement(self: *Self) !ast.AstIndex {
        const node = try self.parseExpression(OpPriority.LOWEST);
        if (self.cur.tokentype == .SEMICOLON) {
            try self.nextToken();
        }
        return node;
    }

    // EXPRESSION PARSERS

    fn prefixParseFn(tt: TokenType) ?*const fn(*Self)anyerror!ast.AstIndex {
        return switch(tt) {
            TokenType.CARAT => parseReturn,
            TokenType.BREAK => parseBreak,
            TokenType.NIL => parseNil,
            TokenType.ELLIPSIS => parseEllipsis,
            TokenType.IDENT => parseIdentifier,
            TokenType.NUMBER => parseNumberLiteral,
            TokenType.DOT => parseNumberLiteral,
            TokenType.NOT => parsePrefixNot,
            TokenType.PLUS => parsePrefixUnp,
            TokenType.MINUS => parsePrefixUnm,
            TokenType.HASH => parsePrefixLen,
            TokenType.META => parsePrefixMeta,
            TokenType.COLON => parsePrefixType,
            TokenType.AT => parseGlobal,
            TokenType.LPAREN => parseBlock,
            TokenType.LBRACKET => parseFunctionLiteral,
            TokenType.STRING => parseStringLiteral,
            TokenType.LBRACE => parseTableLiteral,
            else => null
        };
    }

    fn parseBreak(self: *Self) !ast.AstIndex {
        std.debug.assert(self.cur.tokentype == .BREAK);
        try self.nextToken();
        const node_index = try self.pushNode(ast.Node{.expr = .{ .Break = .{
            .rhs_index = .FINAL
        }}});
        
        const rhs_index = switch(self.next.tokentype) {
            TokenType.RPAREN, TokenType.SEMICOLON, TokenType.EOF => ast.AstIndex.FINAL,
            else => try self.parseExpression(OpPriority.LOWEST)
        };
        self.getNode(node_index).?.expr.Break.rhs_index = rhs_index;
        return node_index;
    }

    fn parseReturn(self: *Self) !ast.AstIndex {
        std.debug.assert(self.cur.tokentype == .CARAT);
        try self.nextToken();
        const node_index = try self.pushNode(ast.Node{.expr = .{ .Return = .{
            .rhs_index = .FINAL
        }}});
        
        const rhs_index = switch(self.next.tokentype) {
            TokenType.RPAREN, TokenType.SEMICOLON, TokenType.EOF => ast.AstIndex.FINAL,
            else => try self.parseExpression(OpPriority.LOWEST)
        };
        self.getNode(node_index).?.expr.Return.rhs_index = rhs_index;
        return node_index;
    }

    fn infixParseFn(tt: TokenType) ?*const fn(*Self, ast.AstIndex)anyerror!ast.AstIndex {
        return switch (tt) {
            TokenType.COMMA => parseInfixComma,
            TokenType.ASSIGN => parseInfixAssign,
            TokenType.DEFINE => parseInfixDefine,
            TokenType.OR => parseInfixOr,
            TokenType.AND => parseInfixAnd,
            TokenType.EQ => parseInfixEq,
            TokenType.NEQ => parseInfixNeq,
            TokenType.LT => parseInfixLT,
            TokenType.GT => parseInfixGT,
            TokenType.LE => parseInfixLE,
            TokenType.GE => parseInfixGE,
            TokenType.PLUS => parseInfixPlus,
            TokenType.MINUS => parseInfixMinus,
            TokenType.ASTERISK => parseInfixMul,
            TokenType.SLASH => parseInfixDiv,
            TokenType.MOD => parseInfixMod,
            TokenType.CARAT => parseInfixPow,
            TokenType.CONCAT => parseInfixConcat,
            TokenType.DOT => parseInfixIndex,
            TokenType.QUESTION => parseInfixLoop,
            TokenType.LPAREN => parseCallExpression,
            else => null
        };
    }

    fn parseExpression(self: *Self, precedence: OpPriority) !ast.AstIndex {
        const prefix = self.cur.tokentype;
        const prefixParser = prefixParseFn(self.cur.tokentype) orelse {
            std.debug.print("INVALID PREFIX: {s}\n", .{@tagName(self.cur.tokentype)});
            return ParseError.INVALID_PREFIX;
        };
        var left: ast.AstIndex = try expect(prefixParser(self));
        // These prefix expressions will never be the left argument to an infix expression.
        switch(prefix) {
            .CARAT => return left,
            .BREAK => return left,
            else => {}
        }
        while (self.cur.tokentype != TokenType.SEMICOLON and @intFromEnum(precedence) < @intFromEnum(op_priority(self.cur.tokentype))) {
            const infixParser = infixParseFn(self.cur.tokentype) orelse return left;
            // leftmost expression becomes one with the highest precedence
            left = try expect(infixParser(self, left));
        }

        return left;
    }

    // PREFIX EXPRESSIONS

    fn parseNil(self: *Self) !ast.AstIndex {
        std.debug.assert(self.cur.tokentype == .NIL);
        try self.nextToken();
        return self.pushNode(ast.Node{.expr = .{ .Nil = .{} }});
    }

    fn parseEllipsis(self: *Self) !ast.AstIndex {
        std.debug.assert(self.cur.tokentype == .ELLIPSIS);
        try self.nextToken();
        return self.pushNode(ast.Node{.expr = .{ .Ellipsis = .{} }});
    }

    fn parseIdentifier(self: *Self) !ast.AstIndex {
        std.debug.assert(self.cur.tokentype == .IDENT);
        const lit_index = self.cur.literal_index;
        try self.nextToken();
        return self.pushNode(ast.Node{.expr = .{ .Identifier = 
            .{ .literal_index=lit_index }
        }});
    }

    fn parseNumberLiteral(self: *Self) !ast.AstIndex {
        std.debug.assert(self.cur.tokentype == .NUMBER);
        const literal_index = self.cur.literal_index;
        const buf = self.lexer.literals.items[literal_index];
        const val: luavm.lua_Number = switch(@typeInfo(luavm.lua_Number)) {
            .Float => std.fmt.parseFloat(luavm.lua_Number, buf) catch (
                @as(luavm.lua_Number, @floatFromInt(std.fmt.parseInt(i64, buf, 0) catch {
                    return ParseError.MALFORMED_NUMBER;
                }))),
            .Int => blk: {
                const f = std.fmt.parseFloat(f32, buf);
                if (f catch null) |v| {
                    break :blk @as(luavm.lua_Number, @intFromFloat(v));
                }
                break :blk std.fmt.parseInt(luavm.lua_Number, buf, 0) catch {
                    return ParseError.MALFORMED_NUMBER;
                };
            },
            else => @compileError("lua_Number must be of type int or float!")
        };
        try self.nextToken();

        return self.pushNode(ast.Node{.expr = .{
            .NumberLiteral = .{.value=val, .literal_index=literal_index}
        }});
    }

    fn parsePrefixNot(self: *Self) !ast.AstIndex {
        try self.nextToken();
        const node_index = try self.pushNode(ast.Node{.expr=.{.Not=.{.rhs_index=.FINAL}}});
        const rhs_index = try self.parseExpression(.PREFIX);
        self.getNode(node_index).?.expr.Not.rhs_index = rhs_index;
        return node_index;
    }

    fn parsePrefixUnp(self: *Self) !ast.AstIndex {
        try self.nextToken();
        return self.parseExpression(.PREFIX);
    }

    fn parsePrefixUnm(self: *Self) !ast.AstIndex {
        try self.nextToken();
        const node_index = try self.pushNode(ast.Node{.expr=.{.Unm=.{.rhs_index=.FINAL}}});
        const rhs_index = try self.parseExpression(.PREFIX);
        self.getNode(node_index).?.expr.Unm.rhs_index = rhs_index;
        return node_index;
    }

    fn parsePrefixLen(self: *Self) !ast.AstIndex {
        try self.nextToken();
        const node_index = try self.pushNode(ast.Node{.expr=.{.Len=.{.rhs_index=.FINAL}}});
        const rhs_index = try self.parseExpression(.PREFIX);
        self.getNode(node_index).?.expr.Len.rhs_index = rhs_index;
        return node_index;
    }

    fn parsePrefixType(self: *Self) !ast.AstIndex {
        try self.nextToken();
        const node_index = try self.pushNode(ast.Node{.expr=.{.Type=.{.rhs_index=.FINAL}}});
        const rhs_index = try self.parseExpression(.PREFIX);
        self.getNode(node_index).?.expr.Type.rhs_index = rhs_index;
        return node_index;
    }

    fn parsePrefixMeta(self: *Self) !ast.AstIndex {
        try self.nextToken();
        const node_index = try self.pushNode(ast.Node{.expr=.{.Meta=.{.rhs_index=.FINAL}}});
        const rhs_index = try self.parseExpression(.PREFIX);
        self.getNode(node_index).?.expr.Meta.rhs_index = rhs_index;
        return node_index;
    }

    fn parseGlobal(self: *Self) !ast.AstIndex {
        std.debug.assert(self.cur.tokentype == .AT);
        try self.nextToken();
        const node_index = try self.pushNode(ast.Node{.expr = .{
            .Global = .{.rhs_index = .FINAL }
        }});
        const rhs_index = try self.parseExpression(.PREFIX);
        self.getNode(node_index).?.expr.Global.rhs_index = rhs_index;
        return node_index;
    }

    fn parseBlock(self: *Self) !ast.AstIndex {
        std.debug.assert(self.cur.tokentype == .LPAREN);
        const node_index = try self.pushNode(ast.Node{.expr = .{ .Block = .{
            .start_index = .FINAL
        }}});
        try self.nextToken();
        if (self.cur.tokentype == .RPAREN) {
            try self.nextToken();
            return node_index;
        }

        var cur_index = try self.parseExpression(.LOWEST);
        var ended_with_semicolon: bool = undefined;
        if (self.cur.tokentype == .SEMICOLON) {
            try self.nextToken();
            ended_with_semicolon = true;
        } else ended_with_semicolon = false;
        self.getNode(node_index).?.expr.Block.start_index = cur_index;

        while (self.cur.tokentype != .RPAREN and self.cur.tokentype != .EOF) {
            const next_index = try self.parseExpression(OpPriority.LOWEST);
            if (self.cur.tokentype == .SEMICOLON) {
                try self.nextToken();
                ended_with_semicolon = true;
            } else ended_with_semicolon = false;
            const cur_stmt = self.getNode(cur_index).?;
            cur_stmt.next_index = next_index;
            cur_index = next_index;
        }
        try self.nextToken();

        if (ended_with_semicolon) {
            self.getNode(cur_index).?.next_index = try self.pushNode(ast.Node{.expr = .{ .Nil = .{} }});
        } else if (self.getNode(node_index)) |n| {
            if (n.expr.Block.start_index == cur_index) {
                // then the block only contains one node, so just get rid of the block and return the node itself.
                return cur_index;
            }

        }
        
        return node_index;
    }

    fn parseFunctionLiteral(self: *Self) !ast.AstIndex {
        std.debug.assert(self.cur.tokentype == .LBRACKET);
        const node_index = try self.pushNode(ast.Node{.expr = .{ .FunctionLiteral = .{
            .params_start = .FINAL,
            .body_start = .FINAL,
        }}});

        try self.nextToken();
        // Parse Args
        {
            var prev_index = ast.AstIndex.FINAL;
            while (self.cur.tokentype != .RBRACKET and self.cur.tokentype != .EOF) {
                const ident_node: ast.Node = switch(self.cur.tokentype) {
                    .IDENT => ast.Node{.expr = .{ .Identifier = .{
                        .literal_index = self.cur.literal_index
                    }}},
                    .ELLIPSIS => ast.Node{.expr = .{ .Ellipsis = .{} }},
                    else => return ParseError.INVALID_ARGLIST
                };
                const is_ellipsis = (ident_node.expr == .Ellipsis);
                const cur_index = try self.pushNode(ident_node);
                
                if (prev_index == .FINAL) {
                    // then this is the first, so set the param list head here
                    self.getNode(node_index).?.expr.FunctionLiteral.params_start = cur_index;
                } else {
                    // link the previous node to here
                    self.getNode(prev_index).?.next_index = cur_index;
                }
                prev_index = cur_index;
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
            return node_index;
        }

        // Read function body
        var cur_index = try self.parseStatement();
        self.getNode(node_index).?.expr.FunctionLiteral.body_start = cur_index;

        while (self.cur.tokentype != .RPAREN and self.cur.tokentype != .EOF) {
            const next_index = try self.parseStatement();
            // getNode must be after parseStatement b/c node creation invalidates node pointers.
            const cur_stmt = self.getNode(cur_index).?;
            cur_stmt.next_index = next_index;
            cur_index = next_index;
        }
        // go one past ')'
        try self.nextToken();

        return node_index;
    }

    fn parseStringLiteral(self: *Self) !ast.AstIndex {
        std.debug.assert(self.cur.tokentype == .STRING);
        const lit_index = self.cur.literal_index;
        try self.nextToken();
        return self.pushNode(ast.Node{.expr = .{
            .StringLiteral = .{.literal_index = lit_index}
        }});
    }

    fn parseTableLiteral(self: *Self) !ast.AstIndex {
        std.debug.assert(self.cur.tokentype == .LBRACE);
        const node_index = try self.pushNode(ast.Node{.expr = .{ .TableLiteral = .{
            .start_index = .FINAL
        }}});

        try self.nextToken();
        // check for empty table
        if (self.cur.tokentype == .RBRACE) {
            try self.nextToken();
            return node_index;
        }
        
        var prev_index = ast.AstIndex.FINAL;
        while (self.cur.tokentype != .RBRACE and self.cur.tokentype != .EOF) {
            const cur_index = try self.parseExpression(.COMMA);
            if (prev_index == .FINAL) {
                self.getNode(node_index).?.expr.TableLiteral.start_index = cur_index;
            } else {
                self.getNode(prev_index).?.next_index = cur_index;
            }
            prev_index = cur_index;
            
            switch (self.cur.tokentype) {
                .COMMA => try self.nextToken(),
                .RBRACE => {},
                else => return ParseError.MALFORMED_TABLE_LITERAL
            }
        }
        // pass '}'
        try self.nextToken();
        return node_index;
    }

    // INFIX EXPRESSIONS

    fn parseInfixComma(self: *Self, left: ast.AstIndex) !ast.AstIndex {
        const node_index = try self.pushNode(ast.Node{.expr=.{
            .Comma=.{ .lhs_index = left, .rhs_index = .FINAL }
        }});
        try self.nextToken();
        const rhs_index = try self.parseExpression(
            (@enumFromInt( @intFromEnum(op_priority(TokenType.COMMA)) - 1))
        );
        self.getNode(node_index).?.expr.Comma.rhs_index = rhs_index;
        return node_index;
    }

    fn parseInfixAssign(self: *Self, left: ast.AstIndex) !ast.AstIndex {
        const node_index = try self.pushNode(ast.Node{.expr=.{
            .Assign=.{ .lhs_index = left, .rhs_index = .FINAL }
        }});
        try self.nextToken();
        const rhs_index = try self.parseExpression(.LOWEST); // right associative, use lower priority
        self.getNode(node_index).?.expr.Assign.rhs_index = rhs_index;
        return node_index;
    }

    fn parseInfixDefine(self: *Self, left: ast.AstIndex) !ast.AstIndex {
        switch ((self.getNode(left) orelse return ParseError.INVALID_DEFINE).expr) {
            .Identifier => {},
            .Comma => {},
            else => return ParseError.INVALID_DEFINE
        }
        const node_index = try self.pushNode(ast.Node{.expr=.{
            .Define=.{ .lhs_index = left, .rhs_index = .FINAL }
        }});
        try self.nextToken();
        const rhs_index = try self.parseExpression(.LOWEST); // right associative, use lower priority
        self.getNode(node_index).?.expr.Define.rhs_index = rhs_index;
        return node_index;
    }

    fn parseInfixOr(self: *Self, left: ast.AstIndex) !ast.AstIndex {
        const node_index = try self.pushNode(ast.Node{.expr=.{
            .Or=.{ .lhs_index = left, .rhs_index = .FINAL }
        }});
        try self.nextToken();
        const rhs_index = try self.parseExpression(comptime op_priority(TokenType.OR));
        self.getNode(node_index).?.expr.Or.rhs_index = rhs_index;
        return node_index;
    }

    fn parseInfixAnd(self: *Self, left: ast.AstIndex) !ast.AstIndex {
        const node_index = try self.pushNode(ast.Node{.expr=.{
            .And=.{ .lhs_index = left, .rhs_index = .FINAL }
        }});
        try self.nextToken();
        const rhs_index = try self.parseExpression(comptime op_priority(TokenType.AND));
        self.getNode(node_index).?.expr.And.rhs_index = rhs_index;
        return node_index;
    }

    fn parseInfixEq(self: *Self, left: ast.AstIndex) !ast.AstIndex {
        const node_index = try self.pushNode(ast.Node{.expr=.{
            .Eq=.{ .lhs_index = left, .rhs_index = .FINAL }
        }});
        try self.nextToken();
        const rhs_index = try self.parseExpression(comptime op_priority(TokenType.EQ));
        self.getNode(node_index).?.expr.Eq.rhs_index = rhs_index;
        return node_index;
    }

    fn parseInfixNeq(self: *Self, left: ast.AstIndex) !ast.AstIndex {
        const node_index = try self.pushNode(ast.Node{.expr=.{
            .Neq=.{ .lhs_index = left, .rhs_index = .FINAL }
        }});
        try self.nextToken();
        const rhs_index = try self.parseExpression(comptime op_priority(TokenType.NEQ));
        self.getNode(node_index).?.expr.Neq.rhs_index = rhs_index;
        return node_index;
    }

    fn parseInfixLT(self: *Self, left: ast.AstIndex) !ast.AstIndex {
        const node_index = try self.pushNode(ast.Node{.expr=.{
            .LT=.{ .lhs_index = left, .rhs_index = .FINAL }
        }});
        try self.nextToken();
        const rhs_index = try self.parseExpression(comptime op_priority(TokenType.LT));
        self.getNode(node_index).?.expr.LT.rhs_index = rhs_index;
        return node_index;
    }

    fn parseInfixGT(self: *Self, left: ast.AstIndex) !ast.AstIndex {
        const node_index = try self.pushNode(ast.Node{.expr=.{
            .GT=.{ .lhs_index = left, .rhs_index = .FINAL }
        }});
        try self.nextToken();
        const rhs_index = try self.parseExpression(comptime op_priority(TokenType.GT));
        self.getNode(node_index).?.expr.GT.rhs_index = rhs_index;
        return node_index;
    }

    fn parseInfixLE(self: *Self, left: ast.AstIndex) !ast.AstIndex {
        const node_index = try self.pushNode(ast.Node{.expr=.{
            .LE=.{ .lhs_index = left, .rhs_index = .FINAL }
        }});
        try self.nextToken();
        const rhs_index = try self.parseExpression(comptime op_priority(TokenType.LE));
        self.getNode(node_index).?.expr.LE.rhs_index = rhs_index;
        return node_index;
    }

    fn parseInfixGE(self: *Self, left: ast.AstIndex) !ast.AstIndex {
        const node_index = try self.pushNode(ast.Node{.expr=.{
            .GE=.{ .lhs_index = left, .rhs_index = .FINAL }
        }});
        try self.nextToken();
        const rhs_index = try self.parseExpression(comptime op_priority(TokenType.GE));
        self.getNode(node_index).?.expr.GE.rhs_index = rhs_index;
        return node_index;
    }

    fn parseInfixPlus(self: *Self, left: ast.AstIndex) !ast.AstIndex {
        const node_index = try self.pushNode(ast.Node{.expr=.{
            .Plus=.{ .lhs_index = left, .rhs_index = .FINAL }
        }});
        try self.nextToken();
        const rhs_index = try self.parseExpression(comptime op_priority(TokenType.PLUS));
        self.getNode(node_index).?.expr.Plus.rhs_index = rhs_index;
        return node_index;
    }

    fn parseInfixMinus(self: *Self, left: ast.AstIndex) !ast.AstIndex {
        const node_index = try self.pushNode(ast.Node{.expr=.{
            .Minus =.{ .lhs_index = left, .rhs_index = .FINAL }
        }});
        try self.nextToken();
        const rhs_index = try self.parseExpression(comptime op_priority(TokenType.MINUS));
        self.getNode(node_index).?.expr.Minus.rhs_index = rhs_index;
        return node_index;
    }

    fn parseInfixMul(self: *Self, left: ast.AstIndex) !ast.AstIndex {
        const node_index = try self.pushNode(ast.Node{.expr=.{
            .Mul=.{ .lhs_index = left, .rhs_index = .FINAL }
        }});
        try self.nextToken();
        const rhs_index = try self.parseExpression(comptime op_priority(TokenType.ASTERISK));
        self.getNode(node_index).?.expr.Mul.rhs_index = rhs_index;
        return node_index;
    }

    fn parseInfixDiv(self: *Self, left: ast.AstIndex) !ast.AstIndex {
        const node_index = try self.pushNode(ast.Node{.expr=.{
            .Div=.{ .lhs_index = left, .rhs_index = .FINAL }
        }});
        try self.nextToken();
        const rhs_index = try self.parseExpression(comptime op_priority(TokenType.SLASH));
        self.getNode(node_index).?.expr.Div.rhs_index = rhs_index;
        return node_index;
    }

    fn parseInfixMod(self: *Self, left: ast.AstIndex) !ast.AstIndex {
        const node_index = try self.pushNode(ast.Node{.expr=.{
            .Mod=.{ .lhs_index = left, .rhs_index = .FINAL }
        }});
        try self.nextToken();
        const rhs_index = try self.parseExpression(comptime op_priority(TokenType.MOD));
        self.getNode(node_index).?.expr.Mod.rhs_index = rhs_index;
        return node_index;
    }

    fn parseInfixPow(self: *Self, left: ast.AstIndex) !ast.AstIndex {
        const node_index = try self.pushNode(ast.Node{.expr=.{
            .Pow=.{ .lhs_index = left, .rhs_index = .FINAL }
        }});
        try self.nextToken();
        // right-associative, so decrease priority by 1
        const rhs_index = try self.parseExpression(
            comptime (@enumFromInt( @intFromEnum(op_priority(TokenType.CARAT)) - 1))
        );
        self.getNode(node_index).?.expr.Pow.rhs_index = rhs_index;
        return node_index;
    }

    fn parseInfixConcat(self: *Self, left: ast.AstIndex) !ast.AstIndex {
        const node_index = try self.pushNode(ast.Node{.expr=.{
            .Concat=.{ .lhs_index = left, .rhs_index = .FINAL }
        }});
        try self.nextToken();
        // right-associative, so decrease priority by 1
        const rhs_index = try self.parseExpression(
            comptime (@enumFromInt( @intFromEnum(op_priority(TokenType.CONCAT)) - 1))
        );
        self.getNode(node_index).?.expr.Concat.rhs_index = rhs_index;
        return node_index;
    }

    fn parseInfixIndex(self: *Self, left: ast.AstIndex) !ast.AstIndex {
        const node_index = try self.pushNode(ast.Node{.expr=.{
            .Index=.{ .lhs_index = left, .rhs_index = .FINAL }
        }});
        try self.nextToken();
        const rhs_index = try self.parseExpression(comptime op_priority(TokenType.DOT));
        self.getNode(node_index).?.expr.Index.rhs_index = rhs_index;
        return node_index;
    }

    fn parseInfixLoop(self: *Self, left: ast.AstIndex) !ast.AstIndex {
        std.debug.assert(self.cur.tokentype == .QUESTION);
        if (left == .FINAL) return ParseError.INVALID_LOOP_ARGUMENT;
        const node_index = try self.pushNode(ast.Node{.expr=.{.Loop = .{
            .cond_index = left,
            .body_index = .FINAL,
        }}});
        try self.nextToken();
        // if (self.cur.tokentype != .LPAREN) return ParseError.MISSING_LOOP_BODY;
        const rhs_index = try self.parseExpression(comptime op_priority(TokenType.QUESTION));
        self.getNode(node_index).?.expr.Loop.body_index = rhs_index;
        return node_index;
    }

    fn parseCallExpression(self: *Self, left: ast.AstIndex) !ast.AstIndex {
        const node_index = try self.pushNode(ast.Node{.expr = .{ .Call = .{
            .lhs_index = left,
            .args_index = .FINAL,
        }}});
        
        try self.nextToken();
        if (self.cur.tokentype == .RPAREN) {
            try self.nextToken();
            return node_index;
        }

        var cur_index = try self.parseExpression(.LOWEST);
        self.getNode(node_index).?.expr.Call.args_index = cur_index;

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
        
        return node_index;
    }
};