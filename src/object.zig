const std = @import("std");

pub const Object_Kind = enum {
    integer,
    boolean,
    null_,
};

pub const Object = union(Object_Kind) {
    integer: Integer,
    boolean: Boolean,
    null_: Null,
};

pub const Integer = struct {
    value: i64,
};

pub const Boolean = struct {
    value: bool,
};

pub const Null = struct {};

pub fn inspect(alloc: std.mem.Allocator, obj: Object) ![]const u8 {
    return switch (obj) {
        .integer => |i| try std.fmt.allocPrint(alloc, "{d}", .{i.value}),
        .boolean => |b| try std.fmt.allocPrint(alloc, "{}", .{b.value}),
        .null_ => try std.fmt.allocPrint(alloc, "null", .{}),
    };
}

pub fn kind(obj: Object) Object_Kind {
    return switch (obj) {
        .integer => .integer,
        .boolean => .boolean,
        .null_ => .null_,
    };
}
