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
        .infix => |i| {
            const left = eval(ast.expression_to_node(i.left));
            const right = eval(ast.expression_to_node(i.right));
            return eval_infix_expression(i.operator, left, right);
        },
        else => .{ .null_ = .{} },
    };
}

fn eval_infix_expression(operator: []const u8, left: object.Object, right: object.Object) object.Object {
    // if (std.mem.eql(u8, operator, "")) {
    if (object.kind(left) == object.Object_Kind.integer and object.kind(right) == object.Object_Kind.integer) {
        return eval_integer_infix_expression(operator, left, right);
    } else if (std.mem.eql(u8, operator, "==")) {
        return .{ .boolean = .{ .value = std.meta.eql(left, right) } };
    } else if (std.mem.eql(u8, operator, "!=")) {
        return .{ .boolean = .{ .value = !std.meta.eql(left, right) } };
    } else {
        return .{ .null_ = .{} };
    }
}

fn eval_integer_infix_expression(operator: []const u8, left: object.Object, right: object.Object) object.Object {
    const left_val = left.integer.value;
    const right_val = right.integer.value;

    if (std.mem.eql(u8, operator, "+")) {
        return .{ .integer = .{ .value = left_val + right_val } };
    } else if (std.mem.eql(u8, operator, "-")) {
        return .{ .integer = .{ .value = left_val - right_val } };
    } else if (std.mem.eql(u8, operator, "*")) {
        return .{ .integer = .{ .value = left_val * right_val } };
    } else if (std.mem.eql(u8, operator, "/")) {
        return .{ .integer = .{ .value = @divTrunc(left_val, right_val) } };
    } else if (std.mem.eql(u8, operator, "<")) {
        return .{ .boolean = .{ .value = left_val < right_val } };
    } else if (std.mem.eql(u8, operator, ">")) {
        return .{ .boolean = .{ .value = left_val > right_val } };
    } else if (std.mem.eql(u8, operator, "==")) {
        return .{ .boolean = .{ .value = left_val == right_val } };
    } else if (std.mem.eql(u8, operator, "!=")) {
        return .{ .boolean = .{ .value = left_val != right_val } };
    } else {
        return .{ .null_ = .{} };
    }
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
