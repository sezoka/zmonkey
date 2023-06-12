const std = @import("std");
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const token = @import("token.zig");

const Parse_Error = error{
    OutOfMemory,
    InvalidInt,
    NoPrefix,
    InvalidToken,
};

const Prefix_Parse_Fn = *const fn (p: *Parser) Parse_Error!ast.Expression;
const Infix_Parse_Fn = *const fn (p: *Parser, ast.Expression) Parse_Error!?ast.Expression;

const Precedence = enum {
    lowest,
    equals,
    less_greater,
    sum,
    product,
    prefix,
    call,
};

pub const Parser = struct {
    lexer: *lexer.Lexer,
    alloc: std.mem.Allocator,
    errors: std.ArrayList([]const u8),
    cur_token: token.Token,
    peek_token: token.Token,

    prefix_parse_fns: std.AutoHashMap(token.Token_Kind, Prefix_Parse_Fn),
    infix_parse_fns: std.AutoHashMap(token.Token_Kind, Infix_Parse_Fn),
};

pub fn init_parser(alloc: std.mem.Allocator, l: *lexer.Lexer) !Parser {
    var p = Parser{
        .lexer = l,
        .alloc = alloc,
        .errors = std.ArrayList([]const u8).init(alloc),
        .cur_token = undefined,
        .peek_token = undefined,
        .prefix_parse_fns = std.AutoHashMap(token.Token_Kind, Prefix_Parse_Fn).init(alloc),
        .infix_parse_fns = std.AutoHashMap(token.Token_Kind, Infix_Parse_Fn).init(alloc),
    };

    try register_prefix(&p, .ident, parse_identifier);
    try register_prefix(&p, .int, parse_integer_literal);
    try register_prefix(&p, .bang, parse_prefix_expression);
    try register_prefix(&p, .minus, parse_prefix_expression);

    next_token(&p);
    next_token(&p);

    return p;
}

pub fn deinit_parser(p: *Parser) void {
    for (p.errors.items) |msg| {
        p.alloc.free(msg);
    }
    p.errors.deinit();
    p.prefix_parse_fns.deinit();
    p.infix_parse_fns.deinit();
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
            if (ptr.return_value) |rv| deinit_expression(alloc, rv);
            alloc.destroy(ptr);
        },
        .expr_stmt => |ptr| {
            if (ptr.expression) |e| deinit_expression(alloc, e);
            alloc.destroy(ptr);
        },
    }
}

pub fn deinit_expression(alloc: std.mem.Allocator, e: ast.Expression) void {
    switch (e) {
        .identifier => |ptr| {
            alloc.destroy(ptr);
        },
        .int => |ptr| {
            alloc.destroy(ptr);
        },
        .prefix => |ptr| {
            deinit_expression(alloc, ptr.right);
            alloc.destroy(ptr);
        },
    }
}

fn parse_prefix_expression(p: *Parser) !ast.Expression {
    const prefix_ptr = try p.alloc.create(ast.Prefix_Expression);
    errdefer p.alloc.destroy(prefix_ptr);

    prefix_ptr.token = p.cur_token;
    prefix_ptr.operator = p.cur_token.literal;
    next_token(p);
    prefix_ptr.right = try parse_expression(p, .prefix);

    return .{ .prefix = prefix_ptr };
}

fn parse_identifier(p: *Parser) !ast.Expression {
    const ident_ptr = try p.alloc.create(ast.Identifier);
    ident_ptr.* = .{
        .token = p.cur_token,
        .value = p.cur_token.literal,
    };

    return .{ .identifier = ident_ptr };
}

fn register_prefix(p: *Parser, kind: token.Token_Kind, func: Prefix_Parse_Fn) !void {
    try p.prefix_parse_fns.put(kind, func);
}

fn register_infix(p: *Parser, kind: token.Token_Kind, func: Prefix_Parse_Fn) !void {
    try p.prefix_parse_fns.put(kind, func);
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

pub fn parse_program(p: *Parser) !ast.Program {
    var statements_arr = std.ArrayList(ast.Statement).init(p.alloc);
    errdefer statements_arr.deinit();

    errdefer {
        for (statements_arr.items) |stmt| {
            deinit_statement(p.alloc, stmt);
        }
    }

    while (p.cur_token.kind != .eof) {
        const stmt = try parse_statement(p);
        try statements_arr.append(stmt);
        next_token(p);
    }

    var program: ast.Program = undefined;
    program.statements = try statements_arr.toOwnedSlice();

    return program;
}

fn parse_statement(p: *Parser) !ast.Statement {
    return switch (p.cur_token.kind) {
        .let => try parse_let_statement(p),
        .return_ => try parse_return_statement(p),
        else => try parse_expression_statement(p),
    };
}

fn parse_expression_statement(p: *Parser) !ast.Statement {
    const expr_stmt_ptr = try p.alloc.create(ast.Expression_Statement);
    errdefer p.alloc.destroy(expr_stmt_ptr);
    expr_stmt_ptr.token = p.cur_token;
    expr_stmt_ptr.expression = try parse_expression(p, .lowest);

    if (peek_token_is(p, .semicolon)) {
        next_token(p);
    }

    return .{ .expr_stmt = expr_stmt_ptr };
}

fn parse_expression(p: *Parser, precedence: Precedence) !ast.Expression {
    _ = precedence;
    const prefix = p.prefix_parse_fns.get(p.cur_token.kind) orelse {
        try no_prefix_parse_fn_error(p, p.cur_token.kind);
        return error.NoPrefix;
    };

    const left_exp = try prefix(p);
    return left_exp;
}

fn no_prefix_parse_fn_error(p: *Parser, t: token.Token_Kind) !void {
    const msg = try std.fmt.allocPrint(p.alloc, "no prefix parse function for {} found", .{t});
    try p.errors.append(msg);
}

fn parse_return_statement(p: *Parser) !ast.Statement {
    const stmt_ptr = try p.alloc.create(ast.Return_Statement);
    stmt_ptr.* = ast.Return_Statement{ .token = p.cur_token, .return_value = null };

    next_token(p);

    while (!cur_token_is(p, .semicolon)) {
        next_token(p);
    }

    return .{ .return_ = stmt_ptr };
}

fn parse_let_statement(p: *Parser) !ast.Statement {
    const stmt_token = p.cur_token;

    if (!try expect_peek(p, .ident)) return error.InvalidToken;

    const stmt_name = .{ .token = p.cur_token, .value = p.cur_token.literal };

    if (!try expect_peek(p, .assign)) return error.InvalidToken;

    while (!cur_token_is(p, .semicolon)) {
        next_token(p);
    }

    const stmt_ptr = try p.alloc.create(ast.Let_Statement);
    stmt_ptr.token = stmt_token;
    stmt_ptr.name = stmt_name;

    return .{ .let = stmt_ptr };
}

fn parse_integer_literal(p: *Parser) !ast.Expression {
    const lit_ptr = try p.alloc.create(ast.Integer_Literal);
    lit_ptr.token = p.cur_token;
    lit_ptr.value = std.fmt.parseInt(i64, p.cur_token.literal, 10) catch {
        const msg = try std.fmt.allocPrint(p.alloc, "could not parse {s} as integer", .{p.cur_token.literal});
        try p.errors.append(msg);
        return error.InvalidInt;
    };
    return .{ .int = lit_ptr };
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
    var p = try init_parser(std.testing.allocator, &l);
    defer deinit_parser(&p);

    var program = parse_program(&p) catch {
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
    var p = try init_parser(std.testing.allocator, &l);
    defer deinit_parser(&p);

    var program = parse_program(&p) catch {
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

test "string" {
    var ident = ast.Identifier{
        .token = .{
            .kind = .ident,
            .literal = "anotherVar",
        },
        .value = "anotherVar",
    };

    var let = ast.Let_Statement{
        .token = .{
            .kind = .let,
            .literal = "let",
        },
        .name = .{
            .token = .{ .kind = .ident, .literal = "myVar" },
            .value = "myVar",
        },
        .value = .{ .identifier = &ident },
    };

    var statements = [_]ast.Statement{
        .{ .let = &let },
    };

    var program = ast.Program{ .statements = &statements };

    const stmt = ast.Statement{ .program = &program };

    var buff = std.ArrayList(u8).init(std.testing.allocator);
    defer buff.deinit();

    try ast.statement_string(stmt, &buff);

    if (!std.mem.eql(u8, buff.items, "let myVar = anotherVar;")) {
        std.debug.print("statement_string(stmt, buff) wrong. got='{s}'\n", .{buff.items});
    }
}

test "identifier expression" {
    const input = "foobar;";

    var l = lexer.init_lexer(input);
    var p = try init_parser(std.testing.allocator, &l);
    defer deinit_parser(&p);

    var program = parse_program(&p) catch {
        std.debug.print("parse_program() returned null\n", .{});
        return error.parseProgramNull;
    };
    defer deinit_program(std.testing.allocator, &program);

    if (program.statements.len != 1) {
        std.debug.print("program.statements has not enough statements. got='{d}'\n", .{program.statements.len});
        return error.invalidStatementsCnt;
    }

    if (program.statements[0] != .expr_stmt) {
        std.debug.print("program.statements[0] is not ast.Expression_Statement. got={}\n", .{program.statements[0]});
        return error.InvalidStatement;
    }
    const stmt = program.statements[0];

    const exp = stmt.expr_stmt.expression orelse {
        std.debug.print("no expression in Expression_Statement.\n", .{});
        return error.NoExpression;
    };
    if (exp != .identifier) {
        std.debug.print("exp not *ast.Identifier. got={}\n", .{stmt.expr_stmt});
        return error.InvalidExpression;
    }
    const ident = exp.identifier;

    if (!std.mem.eql(u8, ident.value, "foobar")) {
        std.debug.print("ident.value not {s}. got={s}\n", .{ "foobar", ident.value });
        return error.InvalidIdenifier;
    }
    if (!std.mem.eql(u8, ast.expression_token_literal(exp), "foobar")) {
        std.debug.print(
            "expression_token_literal(exp) not {s}. got={s}\n",
            .{ "foobar", ast.expression_token_literal(exp) },
        );
        return error.InvalidIdenifier;
    }
}

test "integer literal expression" {
    const input = "5;";
    var l = lexer.init_lexer(input);
    var p = try init_parser(std.testing.allocator, &l);
    defer deinit_parser(&p);

    var program = parse_program(&p) catch {
        std.debug.print("parse_program() returned null\n", .{});
        return error.parseProgramNull;
    };
    defer deinit_program(std.testing.allocator, &program);
    try check_parse_errors(&p);

    if (program.statements.len != 1) {
        std.debug.print("program has not enough statements. got={d}\n", .{program.statements.len});
        return error.NotEnoughStatements;
    }

    if (program.statements[0] != .expr_stmt) {
        std.debug.print("program.statements[0] is not ast.Expression_Statement. got={}\n", .{program.statements[0]});
        return error.NotExprStmt;
    }
    const stmt = program.statements[0];

    const expr = stmt.expr_stmt.expression orelse {
        std.debug.print("stmt.expr_stmt.expression is null.\n", .{});
        return error.ExpressionIsNull;
    };

    if (expr != .int) {
        std.debug.print("expr is not ast.Integer_Literal. got={}\n", .{expr});
        return error.NotIntLiteral;
    }
    const literal = expr.int;

    if (literal.value != 5) {
        std.debug.print("literal.value not {d}. got={d}\n", .{ 5, literal.value });
        return error.InvalidIntValue;
    }
    if (!std.mem.eql(u8, ast.expression_token_literal(expr), "5")) {
        std.debug.print("literal token_literal not {d}\n", .{ast.expression_token_literal(expr)});
    }
}

test "parsing expressions" {
    const Prefix = struct {
        input: []const u8,
        operator: []const u8,
        value: i64,
    };

    const prefix_tests = [_]Prefix{
        .{ .input = "!5;", .operator = "!", .value = 5 },
        .{ .input = "-15;", .operator = "-", .value = 15 },
    };

    inline for (prefix_tests) |tt| {
        var l = lexer.init_lexer(tt.input);
        var p = try init_parser(std.testing.allocator, &l);
        defer deinit_parser(&p);

        var program = parse_program(&p) catch {
            return try check_parse_errors(&p);
        };
        defer deinit_program(std.testing.allocator, &program);

        if (program.statements.len != 1) {
            std.debug.print("program.statements does not contain {d} statements. got={d}\n", .{ 1, program.statements.len });
            return error.InvalidNumberOfStatements;
        }

        if (program.statements[0] != .expr_stmt) {
            std.debug.print("program.statements[0] is not ast.Expression_Statement. got={any}", .{program.statements[0]});
            return error.NotExprStmt;
        }
        const stmt = program.statements[0].expr_stmt;

        const exp = stmt.expression orelse {
            std.debug.print("no expression in Expression_Statement.\n", .{});
            return error.NoExpression;
        };
        if (exp != .prefix) {
            std.debug.print("stmt is not ast .Prefix_Expression. got={any}\n", .{stmt.expression});
            return error.NotPrefixExpression;
        }
        const prefix = exp.prefix;

        if (!std.mem.eql(u8, prefix.operator, tt.operator)) {
            std.debug.print("prefix.operator is not {s}. got={s}\n", .{ tt.operator, prefix.operator });
            return error.OperatorNotMatch;
        }

        try test_integer_literal(prefix.right, comptime tt.value);
    }
}

fn test_integer_literal(il: ast.Expression, comptime value: i64) !void {
    if (il != .int) {
        std.debug.print("il not *ast.Integer_Literal. got={any}\n", .{il});
        return error.NotInteger;
    }
    const integ = il.int;

    if (integ.value != value) {
        std.debug.print("integ.value is not {d}. got={d}\n", .{ value, integ.value });
        return error.NotThatInteger;
    }

    const stringified = std.fmt.comptimePrint("{d}", .{value});
    if (!std.mem.eql(u8, ast.expression_token_literal(il), stringified)) {
        std.debug.print("integ.token_literal not {d}. got={s}", .{ value, ast.expression_token_literal(il) });
        return error.NotThatInteger;
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
