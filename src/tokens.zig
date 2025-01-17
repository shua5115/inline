const std = @import("std");
// Naming convention:
// if the symbol always has the same meaning, then its token name is that meaning.
// if the symbol's meaning changes meaning with context, then its token name is the symbol's name.
// though, this is a loose convention.

pub const TokenType = enum(u8) {
    ILLEGAL,
    EOF,
    NIL, // ~
    ELLIPSIS, // ...
    // Values (have attached text)
    IDENT,
    NUMBER,
    STRING,
    // Operators
    DEFINE, // :=
    ASSIGN, // =
    DOT, // .
    PLUS, // +
    MINUS, // -
    ASTERISK, // *
    SLASH, // /
    MOD, // %
    CARAT, // ^
    BREAK, // ^^
    EQ, // ==
    NEQ, // !=
    LT, // <
    GT, // >
    LE, // <=
    GE, // >=
    AND, // &
    OR, // |
    NOT, // !
    HASH, // #
    AT, // @
    COLON, // :
    META, // ::
    CONCAT, // ..
    // Structure
    SEMICOLON, // ;
    COMMA, // ,
    LPAREN, // (
    RPAREN, // )
    LBRACE, // {
    RBRACE, // }
    LBRACKET, // [
    RBRACKET, // ]
    QUESTION, // ?
};

pub const LiteralIndex = u56;

pub const Token = packed struct {
    const Self = @This();

    tokentype: TokenType,
    literal_index: LiteralIndex = 0,

    pub fn has_value(self: Self) bool {
        return switch(self.tokentype) {
            TokenType.IDENT => true,
            TokenType.STRING => true,
            TokenType.NUMBER => true,
            else => false
        };
    }
};

pub fn is_infix_right_associative(tt: TokenType) bool {
    return switch (tt) {
        .CARAT,
        .CONCAT => true,
        else => false
    };
}

pub fn token_type_str(tt: TokenType) []const u8 {
    return switch (tt) {
        .ILLEGAL => "<ILLEGAL>",
        .EOF => "",
        .NIL => "~",
        .IDENT => "<IDENT>",
        .STRING => "<STRING>",
        .NUMBER => "<NUMBER>",
        .ELLIPSIS => "...",
        .ASSIGN => "=",
        .DEFINE => ":=", 
        .DOT => ".",
        .PLUS => "+",
        .MINUS => "-",
        .ASTERISK => "*",
        .SLASH => "/",
        .MOD => "%",
        .CARAT => "^",
        .BREAK => "^^",
        .EQ => "==",
        .NEQ => "!=",
        .LT => "<",
        .GT => ">",
        .LE => "<=",
        .GE => ">=",
        .AND => "&",
        .OR => "|",
        .NOT => "!",
        .HASH => "#",
        .AT => "@",
        .COLON => ":",
        .META => "::",
        .CONCAT => "..",
        .SEMICOLON => ";",
        .COMMA => ",",
        .LPAREN => "(",
        .RPAREN => ")",
        .LBRACE => "{",
        .RBRACE => "}",
        .LBRACKET => "[",
        .RBRACKET => "]",
        .QUESTION => "?",
    };
}