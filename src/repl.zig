const std = @import("std");
const lexer = @import("lexer.zig");

pub fn start_repl(alloc: std.mem.Allocator) !void {
    const in = std.io.getStdIn().reader();
    const out = std.io.getStdOut().writer();

    while (true) {
        try out.writeAll(">> ");
        const input = (try in.readUntilDelimiterOrEofAlloc(alloc, '\n', 1024)) orelse break;
        defer alloc.free(input);

        var lex = lexer.init_lexer(input);
        var tok = lexer.next_token(&lex);

        while (tok.kind != .eof) : (tok = lexer.next_token(&lex)) {
            try out.print("'{s}' -> {}\n", .{ tok.literal, tok.kind });
        }
    }
}
