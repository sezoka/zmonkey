const std = @import("std");
const Token = @import("token.zig").Token;

pub const Node = union(enum) {
    program: *Program,
    let: *Let_Statement,
    return_: *Return_Statement,
    expr_stmt: *Expression_Statement,
    block: *Block_Statement,
    identifier: *Identifier,
    int: *Integer_Literal,
    prefix: *Prefix_Expression,
    infix: *Infix_Expression,
    boolean: *Boolean,
    if_: *If_Expression,
    function: *Function_Literal,
    call: *Call_Expression,
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
    function: *Function_Literal,
    call: *Call_Expression,
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
    value: Expression,
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

pub const Call_Expression = struct {
    token: Token,
    function: Expression,
    arguments: []Expression,
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

pub const Function_Literal = struct {
    token: Token,
    parameters: []*Identifier,
    body: *Block_Statement,
};

pub fn node_token_literal(node: Node) []const u8 {
    return switch (node) {
        .program => |p| if (0 < p.statements.len) {
            return statement_token_literal(p.statements[0]);
        } else {
            return "<fn>";
        },
        .return_ => |r| return r.token.literal,
        .let => |l| l.token.literal,
        .expr_stmt => |es| es.token.literal,
        .block => |b| return b.token.literal,
        .identifier => |i| return i.token.literal,
        .int => |i| return i.token.literal,
        .prefix => |p| return p.token.literal,
        .infix => |i| return i.token.literal,
        .boolean => |b| return b.token.literal,
        .if_ => |i| return i.token.literal,
        .function => |f| return f.token.literal,
        .call => |c| return c.token.literal,
    };
}

pub fn node_string(node: Node, buff: *std.ArrayList(u8)) !void {
    const writer = buff.writer();
    switch (node) {
        .program => |p| {
            for (p.statements) |s| {
                try statement_string(s, buff);
                try writer.writeAll(" ");
            }
        },
        .let => |ls| {
            try writer.writeAll(ls.token.literal);
            try writer.writeAll(" ");
            try writer.writeAll(ls.name.value);
            try writer.writeAll(" = ");
            try expression_string(ls.value, buff);
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
            try writer.writeAll("{ ");
            for (b.statements) |s| {
                try statement_string(s, buff);
            }
            try writer.writeAll(" }");
        },
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
            try writer.writeAll(") ");
            try node_string(.{ .block = i.consequence }, buff);

            if (i.alternative != null) {
                try writer.writeAll(" else { ");
                try node_string(.{ .block = i.alternative.? }, buff);
                try writer.writeAll(" }");
            }
        },
        .function => |f| {
            try writer.writeAll(f.token.literal);
            try writer.writeAll("(");
            if (f.parameters.len != 0) {
                for (f.parameters[0 .. f.parameters.len - 1]) |p| {
                    try expression_string(.{ .identifier = p }, buff);
                    try writer.writeAll(", ");
                }

                try expression_string(.{ .identifier = f.parameters[f.parameters.len - 1] }, buff);
            }
            try writer.writeAll(")");
            try node_string(.{ .block = f.body }, buff);
        },
        .call => |c| {
            try expression_string(c.function, buff);
            try writer.writeAll("(");
            if (c.arguments.len != 0) {
                for (c.arguments[0 .. c.arguments.len - 1]) |a| {
                    try expression_string(a, buff);
                    try writer.writeAll(", ");
                }

                try expression_string(c.arguments[c.arguments.len - 1], buff);
            }
            try writer.writeAll(")");
        },
    }
}

pub fn expression_to_node(expr: Expression) Node {
    return switch (expr) {
        .identifier => |i| .{ .identifier = i },
        .int => |i| .{ .int = i },
        .prefix => |p| .{ .prefix = p },
        .infix => |i| .{ .infix = i },
        .boolean => |b| .{ .boolean = b },
        .if_ => |i| .{ .if_ = i },
        .function => |f| .{ .function = f },
        .call => |c| .{ .call = c },
    };
}

pub fn statement_to_node(expr: Statement) Node {
    return switch (expr) {
        .program => |p| .{ .program = p },
        .let => |l| .{ .let = l },
        .expr_stmt => |e| .{ .expr_stmt = e },
        .block => |b| .{ .block = b },
        .return_ => |r| .{ .return_ = r },
    };
}

pub fn expression_string(expr: Expression, buff: *std.ArrayList(u8)) error{OutOfMemory}!void {
    return node_string(expression_to_node(expr), buff);
}

pub fn statement_string(stmt: Statement, buff: *std.ArrayList(u8)) error{OutOfMemory}!void {
    return node_string(statement_to_node(stmt), buff);
}

pub fn statement_token_literal(stmt: Statement) []const u8 {
    return switch (stmt) {
        .program => |p| node_token_literal(.{ .program = p }),
        .return_ => |r| node_token_literal(.{ .return_ = r }),
        .let => |l| node_token_literal(.{ .let = l }),
        .expr_stmt => |es| node_token_literal(.{ .expr_stmt = es }),
        .block => |b| node_token_literal(.{ .block = b }),
    };
}

// pub fn expression_token_literal(expr: Expression) []const u8 {
//     return node_token_literal(.{expr});
// }
