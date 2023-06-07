const Token = @import("token.zig").Token;

pub const Node = union(enum) {
    statements: Statement,
    expresstion: Expression,
};

pub const Statement = union(enum) {
    program: *Program,
    let: *Let_Statement,
};

pub const Expression = union(enum) {
    identifier: *Identifier,
};

pub const Program = struct {
    statements: []Statement,
};

pub const Let_Statement = struct {
    token: Token,
    name: Identifier,
    value: Expression,
};

pub const Identifier = struct {
    token: Token,
    value: []const u8,
};

pub fn node_token_literal(node: Node) []const u8 {
    switch (node) {
        .statement => |stmt| statement_token_literal(stmt),
        .expresstion => |expr| expresstion_token_literal(expr),
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
        .let => |l| {
            return l.token.literal;
        },
    };
}

pub fn expresstion_token_literal(expr: Expression) []const u8 {
    return switch (expr) {
        .identifier => |i| return i.token.literal,
    };
}
