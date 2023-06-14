const ast = @import("ast.zig");
const object = @import("object.zig");

pub fn eval(node: ast.Node) object.Object {
    return switch (node) {
        .program => |p| eval_statements(p.statements),
        .expr_stmt => |es| eval(ast.expression_to_node(es.expression.?)),
        .int => |i| .{ .integer = .{ .value = i.value } },
        else => .{ .null_ = .{} },
    };
}

pub fn eval_statements(statements: []ast.Statement) object.Object {
    var result: object.Object = undefined;

    for (statements) |s| {
        result = eval(ast.statement_to_node(s));
    }

    return result;
}
