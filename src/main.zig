const std = @import("std");
const repl = @import("repl.zig");

fn greeting() !void {
    const out = std.io.getStdOut().writer();
    try out.writeAll("Hello! This is the Monkey programming language!\n");
    try out.writeAll("Feel free to type commands\n");
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const alloc = gpa.allocator();

    try greeting();
    try repl.start_repl(alloc);
}
