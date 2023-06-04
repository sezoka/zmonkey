pub const Token_Kind = enum {
    illegal,
    eof,

    // identifiers + literals
    ident,
    int,

    // operators
    assign, // =
    plus, // +
    minus, // -
    bang, // !
    asterisk, // *
    slash, // /

    lt, // <
    gt, // >

    eq, // ==
    not_eq, // !=

    // delimiters
    comma, // ,
    semicolon, // ;

    lparen, // (
    rparen, // )
    lbrace, // {
    rbrace, // }

    // keywords
    function, // fn
    let, // let
    true_, // true
    false_, // false
    if_, // if
    else_, // else
    return_, // return
};

pub const Token = struct {
    kind: Token_Kind,
    literal: []const u8,
};
