const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");

pub fn start_repl(alloc: std.mem.Allocator) !void {
    const in = std.io.getStdIn().reader();
    const out = std.io.getStdOut().writer();

    var buff = std.ArrayList(u8).init(alloc);
    defer buff.deinit();

    while (true) {
        try out.writeAll(">> ");
        const input = (try in.readUntilDelimiterOrEofAlloc(alloc, '\n', 1024)) orelse break;
        defer alloc.free(input);

        var lex = lexer.init_lexer(input);
        var p = try parser.init_parser(alloc, &lex);
        defer parser.deinit_parser(&p);
        var program = parser.parse_program(&p) catch {
            try print_parser_errors(out, p.errors.items);
            continue;
        };
        defer parser.deinit_program(alloc, &program);

        try ast.statement_string(.{ .program = &program }, &buff);
        std.debug.print("{s}\n", .{buff.items});
        buff.clearRetainingCapacity();
        // var tok = lexer.next_token(&lex);

        // while (tok.kind != .eof) : (tok = lexer.next_token(&lex)) {
        // try out.print("'{s}' -> {}\n", .{ tok.literal, tok.kind });
        // }
    }
}

fn print_parser_errors(out: anytype, errors: [][]const u8) !void {
    for (errors) |msg| {
        try out.print("\t{s}\n", .{msg});
    }
}
