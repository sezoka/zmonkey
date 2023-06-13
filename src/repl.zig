const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");

pub fn start_repl(alloc: std.mem.Allocator) !void {
    const in = std.io.getStdIn().reader();
    const out = std.io.getStdOut().writer();

    while (true) {
        try out.writeAll(">> ");
        const input = (try in.readUntilDelimiterOrEofAlloc(alloc, '\n', 1024)) orelse break;
        defer alloc.free(input);

        var lex = lexer.init_lexer(input);
        var p = try parser.init_parser(alloc, &lex);

        defer parser.deinit_parser(&p);
        var program = try parser.parse_program(&p);
        defer parser.deinit_program(alloc, &program);

        var buff = std.ArrayList(u8).init(alloc);
        defer buff.deinit();

        try ast.statement_string(.{ .program = &program }, &buff);
        std.debug.print("{s}\n", .{buff.items});
        // var tok = lexer.next_token(&lex);

        // while (tok.kind != .eof) : (tok = lexer.next_token(&lex)) {
        // try out.print("'{s}' -> {}\n", .{ tok.literal, tok.kind });
        // }
    }
}
