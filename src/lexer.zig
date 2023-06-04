const std = @import("std");
const token_mod = @import("token.zig");
const Token = token_mod.Token;
const Token_Kind = token_mod.Token_Kind;

const Lexer = struct {
    input: []const u8,
    position: usize,
    read_position: usize,
};

const keywords = std.ComptimeStringMap(Token_Kind, .{
    .{ "fn", .function },
    .{ "let", .let },
    .{ "true", .true_ },
    .{ "false", .false_ },
    .{ "if", .if_ },
    .{ "else", .else_ },
    .{ "return", .return_ },
});

pub fn init_lexer(input: []const u8) Lexer {
    var l = Lexer{
        .input = input,
        .position = 0,
        .read_position = 0,
    };

    read_char(&l);

    return l;
}

fn next_token(l: *Lexer) Token {
    if (is_at_end(l)) {
        return new_token(.eof, "");
    }

    skip_whitespace(l);

    const ch_slice = get_literal(l);
    const ch = peek(l);

    const tok = switch (ch) {
        '=' => blk: {
            if (peek_next(l) == '=') {
                advance(l);
                const literal = get_literal(l);
                break :blk new_token(.eq, literal);
            } else {
                break :blk new_token(.assign, ch_slice);
            }
        },
        '+' => new_token(.plus, ch_slice),
        '-' => new_token(.minus, ch_slice),
        '!' => blk: {
            if (peek_next(l) == '=') {
                advance(l);
                const literal = get_literal(l);
                break :blk new_token(.not_eq, literal);
            } else {
                break :blk new_token(.bang, ch_slice);
            }
        },
        '/' => new_token(.slash, ch_slice),
        '*' => new_token(.asterisk, ch_slice),
        '<' => new_token(.lt, ch_slice),
        '>' => new_token(.gt, ch_slice),
        ';' => new_token(.semicolon, ch_slice),
        ',' => new_token(.comma, ch_slice),
        '(' => new_token(.lparen, ch_slice),
        ')' => new_token(.rparen, ch_slice),
        '{' => new_token(.lbrace, ch_slice),
        '}' => new_token(.rbrace, ch_slice),
        else => {
            if (is_letter(ch)) {
                const literal = read_identifier(l);
                const kind = lookup_ident(literal);
                return new_token(kind, literal);
            } else if (is_digit(ch)) {
                const literal = read_number(l);
                return new_token(.int, literal);
            } else {
                return new_token(.illegal, "");
            }
        },
    };

    read_char(l);

    return tok;
}

fn read_char(l: *Lexer) void {
    l.position = l.read_position;
    l.read_position += 1;
}

fn advance(l: *Lexer) void {
    l.read_position += 1;
}

fn get_literal(l: *Lexer) []const u8 {
    return l.input[l.position..l.read_position];
}

fn new_token(
    kind: Token_Kind,
    literal: []const u8,
) Token {
    return .{ .kind = kind, .literal = literal };
}

fn read_identifier(l: *Lexer) []const u8 {
    const position = l.position;
    while (is_letter(peek(l))) {
        read_char(l);
    }
    return l.input[position..l.position];
}

fn is_letter(ch: u8) bool {
    return ('a' <= ch and ch <= 'z') or ('A' <= ch and ch <= 'Z') or (ch == '_');
}

fn lookup_ident(ident: []const u8) Token_Kind {
    if (keywords.get(ident)) |tok| {
        return tok;
    } else {
        return .ident;
    }
}

fn skip_whitespace(l: *Lexer) void {
    while (true) {
        switch (peek(l)) {
            ' ', '\t', '\n', '\r' => read_char(l),
            else => break,
        }
    }
}

fn read_number(l: *Lexer) []const u8 {
    const position = l.position;
    var ch = peek(l);
    while (is_digit(ch)) {
        read_char(l);
        ch = peek(l);
    }
    return l.input[position..l.position];
}

fn is_digit(ch: u8) bool {
    return '0' <= ch and ch <= '9';
}

fn is_at_end(l: *Lexer) bool {
    return l.input.len <= l.position;
}

fn peek(l: *Lexer) u8 {
    if (is_at_end(l)) {
        return 0;
    } else {
        return l.input[l.position];
    }
}

fn peek_next(l: *Lexer) u8 {
    if (l.input.len <= l.position + 1) {
        return 0;
    } else {
        return l.input[l.position + 1];
    }
}

test "next token" {
    const Expected_Token = struct {
        expected_kind: Token_Kind,
        expected_literal: []const u8,

        const Self = @This();

        fn init(expected_kind: Token_Kind, expected_literal: []const u8) Self {
            return .{ .expected_kind = expected_kind, .expected_literal = expected_literal };
        }
    };

    const input =
        \\ let five = 5;
        \\ let ten = 10;
        \\  
        \\ let add = fn(x, y) {
        \\     x + y;
        \\ };
        \\
        \\ let result = add(five, ten);
        \\ !-/*5;
        \\ 5 < 10 > 5;
        \\
        \\ if (5 < 10) {
        \\     return true;
        \\ } else {
        \\     return false;
        \\ }
        \\
        \\ 10 == 10;
        \\ 10 != 9;
    ;

    const tests = [_]Expected_Token{
        Expected_Token.init(.let, "let"),
        Expected_Token.init(.ident, "five"),
        Expected_Token.init(.assign, "="),
        Expected_Token.init(.int, "5"),
        Expected_Token.init(.semicolon, ";"),
        Expected_Token.init(.let, "let"),
        Expected_Token.init(.ident, "ten"),
        Expected_Token.init(.assign, "="),
        Expected_Token.init(.int, "10"),
        Expected_Token.init(.semicolon, ";"),
        Expected_Token.init(.let, "let"),
        Expected_Token.init(.ident, "add"),
        Expected_Token.init(.assign, "="),
        Expected_Token.init(.function, "fn"),
        Expected_Token.init(.lparen, "("),
        Expected_Token.init(.ident, "x"),
        Expected_Token.init(.comma, ","),
        Expected_Token.init(.ident, "y"),
        Expected_Token.init(.rparen, ")"),
        Expected_Token.init(.lbrace, "{"),
        Expected_Token.init(.ident, "x"),
        Expected_Token.init(.plus, "+"),
        Expected_Token.init(.ident, "y"),
        Expected_Token.init(.semicolon, ";"),
        Expected_Token.init(.rbrace, "}"),
        Expected_Token.init(.semicolon, ";"),
        Expected_Token.init(.let, "let"),
        Expected_Token.init(.ident, "result"),
        Expected_Token.init(.assign, "="),
        Expected_Token.init(.ident, "add"),
        Expected_Token.init(.lparen, "("),
        Expected_Token.init(.ident, "five"),
        Expected_Token.init(.comma, ","),
        Expected_Token.init(.ident, "ten"),
        Expected_Token.init(.rparen, ")"),
        Expected_Token.init(.semicolon, ";"),
        Expected_Token.init(.bang, "!"),
        Expected_Token.init(.minus, "-"),
        Expected_Token.init(.slash, "/"),
        Expected_Token.init(.asterisk, "*"),
        Expected_Token.init(.int, "5"),
        Expected_Token.init(.semicolon, ";"),
        Expected_Token.init(.int, "5"),
        Expected_Token.init(.lt, "<"),
        Expected_Token.init(.int, "10"),
        Expected_Token.init(.gt, ">"),
        Expected_Token.init(.int, "5"),
        Expected_Token.init(.semicolon, ";"),
        Expected_Token.init(.if_, "if"),
        Expected_Token.init(.lparen, "("),
        Expected_Token.init(.int, "5"),
        Expected_Token.init(.lt, "<"),
        Expected_Token.init(.int, "10"),
        Expected_Token.init(.rparen, ")"),
        Expected_Token.init(.lbrace, "{"),
        Expected_Token.init(.return_, "return"),
        Expected_Token.init(.true_, "true"),
        Expected_Token.init(.semicolon, ";"),
        Expected_Token.init(.rbrace, "}"),
        Expected_Token.init(.else_, "else"),
        Expected_Token.init(.lbrace, "{"),
        Expected_Token.init(.return_, "return"),
        Expected_Token.init(.false_, "false"),
        Expected_Token.init(.semicolon, ";"),
        Expected_Token.init(.rbrace, "}"),
        Expected_Token.init(.int, "10"),
        Expected_Token.init(.eq, "=="),
        Expected_Token.init(.int, "10"),
        Expected_Token.init(.semicolon, ";"),
        Expected_Token.init(.int, "10"),
        Expected_Token.init(.not_eq, "!="),
        Expected_Token.init(.int, "9"),
        Expected_Token.init(.semicolon, ";"),
        Expected_Token.init(.eof, ""),
    };

    var l = init_lexer(input);
    for (tests, 0..) |tt, i| {
        const tok = next_token(&l);

        if (tok.kind != tt.expected_kind) {
            std.debug.print("tests[{d}] - tokenkind wrong. expected='{}', got='{}'\n", .{ i, tt.expected_kind, tok.kind });
            return error.TokenKindWrong;
        }

        if (!std.mem.eql(u8, tok.literal, tt.expected_literal)) {
            std.debug.print("tests[{d}] - literal wrong. expected='{s}', got='{s}'\n", .{ i, tt.expected_literal, tok.literal });
            return error.TokenLiteralWrong;
        }
    }
}
