const std = @import("std");
const ast = @import("ast.zig");
const object = @import("object.zig");

pub fn eval(node: ast.Node) object.Object {
    return switch (node) {
        .program => |p| eval_statements(p.statements),
        .expr_stmt => |es| eval(ast.expression_to_node(es.expression.?)),
        .int => |i| .{ .integer = .{ .value = i.value } },
        .boolean => |b| .{ .boolean = .{ .value = b.value } },
        .prefix => |p| {
            const right = eval(ast.expression_to_node(p.right));
            return eval_prefix_expression(p.operator, right);
        },
        else => .{ .null_ = .{} },
    };
}

fn eval_prefix_expression(operator: []const u8, right: object.Object) object.Object {
    if (std.mem.eql(u8, operator, "!")) {
        return eval_bang_operator_expression(right);
    } else if (std.mem.eql(u8, operator, "-")) {
        return eval_minus_prefix_operator_expression(right);
    } else {
        return .{ .null_ = .{} };
    }
}

fn eval_bang_operator_expression(right: object.Object) object.Object {
    return switch (right) {
        .boolean => |b| .{ .boolean = .{ .value = !b.value } },
        .null_ => .{ .boolean = .{ .value = true } },
        else => .{ .boolean = .{ .value = false } },
    };
}

fn eval_minus_prefix_operator_expression(right: object.Object) object.Object {
    if (object.kind(right) != .integer) {
        return .{ .null_ = .{} };
    }

    const negated = -right.integer.value;
    return .{ .integer = .{ .value = negated } };
}

pub fn eval_statements(statements: []ast.Statement) object.Object {
    var result: object.Object = undefined;

    for (statements) |s| {
        result = eval(ast.statement_to_node(s));
    }

    return result;
}
