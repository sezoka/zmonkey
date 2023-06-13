const std = @import("std");
const Token = @import("token.zig").Token;

pub const Node = union(enum) {
    statements: Statement,
    expression: Expression,
};

pub const Statement = union(enum) {
    program: *Program,
    let: *Let_Statement,
    return_: *Return_Statement,
    expr_stmt: *Expression_Statement,
    block: *Block_Statement,
};

pub const Expression = union(enum) {
    identifier: *Identifier,
    int: *Integer_Literal,
    prefix: *Prefix_Expression,
    infix: *Infix_Expression,
    boolean: *Boolean,
    if_: *If_Expression,
};

pub const Prefix_Expression = struct {
    token: Token,
    operator: []const u8,
    right: Expression,
};

pub const Expression_Statement = struct {
    token: Token,
    expression: ?Expression,
};

pub const Program = struct {
    statements: []Statement,
};

pub const Let_Statement = struct {
    token: Token,
    name: Identifier,
    value: ?Expression,
};

pub const Infix_Expression = struct {
    token: Token,
    left: Expression,
    operator: []const u8,
    right: Expression,
};

pub const If_Expression = struct {
    token: Token,
    condition: Expression,
    consequence: *Block_Statement,
    alternative: ?*Block_Statement,
};

pub const Return_Statement = struct {
    token: Token,
    return_value: ?Expression,
};

pub const Block_Statement = struct {
    token: Token,
    statements: []Statement,
};

pub const Identifier = struct {
    token: Token,
    value: []const u8,
};

pub const Integer_Literal = struct {
    token: Token,
    value: i64,
};

pub const Boolean = struct {
    token: Token,
    value: bool,
};

pub fn node_token_literal(node: Node) []const u8 {
    switch (node) {
        .statement => |stmt| statement_token_literal(stmt),
        .expression => |expr| expression_token_literal(expr),
    }
}

pub fn node_string(node: Node, buff: std.ArrayList(u8)) !void {
    switch (node) {
        .statement => |stmt| try statement_string(stmt, buff),
        .expression => |expr| try expression_string(expr, buff),
    }
}

pub fn statement_string(stmt: Statement, buff: *std.ArrayList(u8)) !void {
    const writer = buff.writer();
    switch (stmt) {
        .program => |p| {
            for (p.statements) |s| {
                try statement_string(s, buff);
            }
        },
        .let => |ls| {
            try writer.writeAll(statement_token_literal(stmt));
            try writer.writeAll(ls.name.value);
            try writer.writeAll(" ");
            try writer.writeAll(" = ");
            if (ls.value) |val| {
                try expression_string(val, buff);
            }
            try writer.writeAll(";");
        },
        .return_ => |rs| {
            try writer.writeAll(rs.token.literal);
            try writer.writeAll(" ");
            if (rs.return_value) |val| {
                try expression_string(val, buff);
            }
            try writer.writeAll(";");
        },
        .expr_stmt => |es| {
            if (es.expression) |e| {
                try expression_string(e, buff);
            }
        },
        .block => |b| {
            for (b.statements) |s| {
                try statement_string(s, buff);
            }
        },
    }
}

const Espr_Str_Error = error{
    OutOfMemory,
};

pub fn expression_string(expr: Expression, buff: *std.ArrayList(u8)) Espr_Str_Error!void {
    const writer = buff.writer();
    switch (expr) {
        .identifier => |i| {
            try writer.writeAll(i.value);
        },
        .int => |i| {
            try writer.writeAll(i.token.literal);
        },
        .prefix => |p| {
            try writer.writeAll("(");
            try writer.writeAll(p.operator);
            try expression_string(p.right, buff);
            try writer.writeAll(")");
        },
        .infix => |i| {
            try writer.writeAll("(");
            try expression_string(i.left, buff);
            try writer.writeAll(" ");
            try writer.writeAll(i.operator);
            try writer.writeAll(" ");
            try expression_string(i.right, buff);
            try writer.writeAll(")");
        },
        .boolean => |b| {
            try writer.writeAll(b.token.literal);
        },
        .if_ => |i| {
            try writer.writeAll("if (");
            try expression_string(i.condition, buff);
            try writer.writeAll(") { ");
            try statement_string(.{ .block = i.consequence }, buff);
            try writer.writeAll(" }");

            if (i.alternative != null) {
                try writer.writeAll(" else { ");
                try statement_string(.{ .block = i.alternative.? }, buff);
                try writer.writeAll(" }");
            }
        },
    }
}

pub fn statement_token_literal(stmt: Statement) []const u8 {
    return switch (stmt) {
        .program => |p| {
            if (0 < p.statements.len) {
                return statement_token_literal(p.statements[0]);
            } else {
                return "<fn>";
            }
        },
        .return_ => |r| {
            return r.token.literal;
        },
        .let => |l| {
            return l.token.literal;
        },
        .expr_stmt => |es| {
            return es.token.literal;
        },
        .block => |b| {
            return b.token.literal;
        },
    };
}

pub fn expression_token_literal(expr: Expression) []const u8 {
    return switch (expr) {
        .identifier => |i| return i.token.literal,
        .int => |i| return i.token.literal,
        .prefix => |p| return p.token.literal,
        .infix => |i| return i.token.literal,
        .boolean => |b| return b.token.literal,
        .if_ => |i| return i.token.literal,
    };
}
