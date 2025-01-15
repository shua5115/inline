// Naming convention:
// if the symbol always has the same meaning, then its token name is that meaning.
// if the symbol's meaning changes meaning with context, then its token name is the symbol's name.
// though, this is a loose convention.

pub const TokenType = enum {
    ILLEGAL,
    EOF,
    NIL, // ~
    ELLIPSIS, // ...
    // Values (have attached text)
    IDENT,
    NUMBER,
    STRING,
    // Operators
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

pub const TokenLiteral = union {
    static: [*:0]const u8,
    index: usize, // index into strings arraylist
};

pub const Token = struct {
    const Self = @This();

    tokentype: TokenType,
    // will be a string if the value varies between token instances
    literal: TokenLiteral,

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

pub fn token_type_str(tt: TokenType) ?[]const u8 {
    return switch (tt) {
        .ILLEGAL => "<ILLEGAL>", // illegal
        .EOF => "", // emptystr
        .NIL => "~", // ~
        .ELLIPSIS => "...", // ...
        .ASSIGN => "=", // =
        .DOT => ".", // .
        .PLUS => "+", // +
        .MINUS => "-", // -
        .ASTERISK => "*", // *
        .SLASH => "/", // /
        .MOD => "%", // %
        .CARAT => "^", // ^
        .BREAK => "^^", // ^^
        .EQ => "==", // ==
        .NEQ => "!=", // !=
        .LT => "<", // <
        .GT => ">", // >
        .LE => "<=", // <=
        .GE => ">=", // >=
        .AND => "&", // &
        .OR => "|", // |
        .NOT => "!", // !
        .HASH => "#", // #
        .AT => "@", // @
        .COLON => ":", // :
        .META => "::", // ::
        .CONCAT => "..", // ..
        .SEMICOLON => ";", // ;
        .COMMA => ",", // ,
        .LPAREN => "(", // (
        .RPAREN => ")", // )
        .LBRACE => "{", // {
        .RBRACE => "}", // }
        .LBRACKET => "[", // [
        .RBRACKET => "]", // ]
        .QUESTION => "?", // ?
        else => null
    };
}