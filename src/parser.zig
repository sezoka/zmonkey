const std = @import("std");
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const token = @import("token.zig");

pub const Parser = struct {
    lexer: *lexer.Lexer,
    alloc: std.mem.Allocator,
    errors: std.ArrayList([]const u8),
    cur_token: token.Token,
    peek_token: token.Token,
};

pub fn init_parser(alloc: std.mem.Allocator, l: *lexer.Lexer) Parser {
    var p = Parser{
        .lexer = l,
        .alloc = alloc,
        .errors = std.ArrayList([]const u8).init(alloc),
        .cur_token = undefined,
        .peek_token = undefined,
    };

    next_token(&p);
    next_token(&p);

    return p;
}

pub fn deinit_parser(p: *Parser) void {
    p.errors.deinit();
}

pub fn deinit_program(alloc: std.mem.Allocator, p: *ast.Program) void {
    for (p.statements) |stmt| {
        deinit_statement(alloc, stmt);
    }
    alloc.free(p.statements);
}

pub fn deinit_statement(alloc: std.mem.Allocator, s: ast.Statement) void {
    switch (s) {
        .let => |ptr| {
            alloc.destroy(ptr);
        },
        .program => |ptr| {
            deinit_program(alloc, ptr);
            alloc.destroy(ptr);
        },
        .return_ => |ptr| {
            deinit_expression(alloc, ptr.return_value);
            alloc.destroy(ptr);
        },
    }
}

pub fn deinit_expression(alloc: std.mem.Allocator, e: ast.Expression) void {
    switch (e) {
        .identifier => |ptr| {
            _ = ptr;
            _ = alloc;
            // alloc.destroy(ptr);
        },
    }
}

fn get_errors(p: *Parser) [][]const u8 {
    return p.errors.items;
}

fn peek_error(p: *Parser, k: token.Token_Kind) !void {
    const msg = try std.fmt.allocPrint(p.alloc, "expected next token to be {}, got {} istead", .{ k, p.peek_token.kind });
    try p.errors.append(msg);
}

fn next_token(p: *Parser) void {
    p.cur_token = p.peek_token;
    p.peek_token = lexer.next_token(p.lexer);
}

pub fn parse_program(p: *Parser) !?ast.Program {
    var program: ast.Program = undefined;

    var statements_arr = std.ArrayList(ast.Statement).init(p.alloc);
    errdefer statements_arr.deinit();

    while (p.cur_token.kind != .eof) {
        const maybe_stmt = try parse_statement(p);
        if (maybe_stmt) |stmt| {
            try statements_arr.append(stmt);
        }
        next_token(p);
    }

    program.statements = try statements_arr.toOwnedSlice();

    return program;
}

fn parse_statement(p: *Parser) !?ast.Statement {
    return switch (p.cur_token.kind) {
        .let => try parse_let_statement(p),
        .return_ => try parse_return_statement(p),
        else => return null,
    };
}

fn parse_return_statement(p: *Parser) !?ast.Statement {
    const stmt_ptr = try p.alloc.create(ast.Return_Statement);
    stmt_ptr.* = ast.Return_Statement{ .token = p.cur_token, .return_value = undefined };

    next_token(p);

    while (!cur_token_is(p, .semicolon)) {
        next_token(p);
    }

    return .{ .return_ = stmt_ptr };
}

fn parse_let_statement(p: *Parser) !?ast.Statement {
    const stmt_token = p.cur_token;

    if (!try expect_peek(p, .ident)) return null;

    const stmt_name = .{ .token = p.cur_token, .value = p.cur_token.literal };

    if (!try expect_peek(p, .assign)) return null;

    while (!cur_token_is(p, .semicolon)) {
        next_token(p);
    }

    const stmt_ptr = try p.alloc.create(ast.Let_Statement);
    stmt_ptr.token = stmt_token;
    stmt_ptr.name = stmt_name;

    return .{ .let = stmt_ptr };
}

fn cur_token_is(p: *Parser, k: token.Token_Kind) bool {
    return p.cur_token.kind == k;
}

fn peek_token_is(p: *Parser, k: token.Token_Kind) bool {
    return p.peek_token.kind == k;
}

fn expect_peek(p: *Parser, k: token.Token_Kind) !bool {
    if (peek_token_is(p, k)) {
        next_token(p);
        return true;
    } else {
        try peek_error(p, k);
        return false;
    }
}

test "let statement" {
    const helper = struct {
        pub fn test_let_statement(s: ast.Statement, name: []const u8) !void {
            if (!std.mem.eql(u8, ast.statement_token_literal(s), "let")) {
                std.debug.print("statement_token_literal(s) not 'let'. got '{s}'\n", .{ast.statement_token_literal(s)});
                return error.StatementLiteralNotLet;
            }

            if (s != .let) {
                std.debug.print("s not *ast.Let_Statement. got='{any}'\n", .{s});
                return error.SNotLetStatement;
            }

            const let_stmt = s.let;

            if (!std.mem.eql(u8, let_stmt.name.value, name)) {
                std.debug.print("let_stmt.name.value not '{s}'. got='{s}'\n", .{ name, let_stmt.name.value });
                return error.LetStmtNameNotMathing;
            }
        }
    };

    const input =
        \\ let x = 5;
        \\ let y = 10;
        \\ let foobar = 838383;
    ;

    var l = lexer.init_lexer(input);
    var p = init_parser(std.testing.allocator, &l);
    defer deinit_parser(&p);

    var program = (try parse_program(&p)) orelse {
        try check_parse_errors(&p);

        std.debug.print("parse_program() returned null\n", .{});
        return error.parseProgramNull;
    };
    defer deinit_program(std.testing.allocator, &program);

    if (program.statements.len != 3) {
        std.debug.print("program.statements does not contain 3 statements. got='{d}'\n", .{program.statements.len});
        return error.invalidStatementsCnt;
    }

    const tests = [_](struct {
        expected_ident: []const u8,
    }){
        .{ .expected_ident = "x" },
        .{ .expected_ident = "y" },
        .{ .expected_ident = "foobar" },
    };

    for (tests, 0..) |tt, i| {
        const stmt = program.statements[i];
        try helper.test_let_statement(stmt, tt.expected_ident);
    }
}

test "return statement" {
    const input =
        \\ return 5;
        \\ return 10;
        \\ return 993322;
    ;

    var l = lexer.init_lexer(input);
    var p = init_parser(std.testing.allocator, &l);

    var program = (try parse_program(&p)) orelse {
        std.debug.print("parse_program() returned null\n", .{});
        return error.parseProgramNull;
    };
    defer deinit_program(std.testing.allocator, &program);

    try check_parse_errors(&p);

    if (program.statements.len != 3) {
        std.debug.print("program.statements does not contain 3 statements. got='{d}'\n", .{program.statements.len});
        return error.invalidStatementsCnt;
    }

    for (program.statements) |stmt| {
        if (stmt != .return_) {
            std.debug.print("stmt not *ast.Return_Statement. got={}\n", .{stmt});
            continue;
        }

        if (!std.mem.eql(u8, ast.statement_token_literal(stmt), "return")) {
            const literal = ast.statement_token_literal(stmt);
            std.debug.print("statement_token_literal(return_stmt) is not 'return', got {s}\n", .{literal});
        }
    }
}

fn check_parse_errors(p: *Parser) !void {
    const errors = get_errors(p);
    if (errors.len == 0) {
        return;
    }

    std.debug.print("parser has {d} errors\n", .{errors.len});
    for (errors) |msg| {
        std.debug.print("parser error: {s}\n", .{msg});
    }
    return error.FoundParseErrors;
}
