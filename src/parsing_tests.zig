const std = @import("std");
const ast = @import("ast.zig");
const lexing = @import("lexing.zig");
const parsing = @import("parsing.zig");
const stringreader = @import("stringreader.zig");

const Lexer = lexing.Lexer;
const Parser = parsing.Parser;
const StringReader = stringreader.StringReader;

const ParserContext = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    source: StringReader,
    literals: std.ArrayList([]const u8),
    nodes: std.ArrayList(ast.Node),

    pub fn init(allocator: std.mem.Allocator, source: []const u8) !Self {
        return Self{
            .allocator = allocator,
            .source = StringReader.init(source),
            .literals = std.ArrayList([]const u8).init(allocator),
            .nodes = std.ArrayList(ast.Node).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.nodes.deinit();
        for (self.literals.items) |str| {
            self.allocator.free(str);
        }
        self.literals.deinit();
    }
};

fn verifyAST(allocator: std.mem.Allocator, nodes: []const ast.Node, literals: []const[]const u8, node_index: ast.AstIndex, expected: []const u8) !void {
    var buf = std.ArrayList(u8).init(allocator);
    defer buf.deinit();
    try ast.write_node(&buf.writer().any(), &nodes, &literals, node_index);
    var err = buf.count != expected.len;
    var i: usize = 0;
    while (!err) {
        if (expected[i] != buf.items[i]) err = true;
        i += 1;
    }

    if (err) {
        std.debug.print("AST incorrect!\nExpected: {s}\nGot:      {s}", .{expected, buf.items});
        unreachable;
    }
}

test "parse assign" {
    const alloc = std.testing.allocator;
    
    const source: []const u8 = 
    \\inline = "Hello, world!";
    \\add = [a, b](^(a+b));
    ;
    std.debug.print("Source:\n{s}\n", .{source});
    var context = try ParserContext.init(alloc, source);
    var lexer = Lexer.init(alloc, &context.literals, context.source.reader().any());
    var parser = try Parser.init(alloc, &lexer, &context.literals, &context.nodes);
    defer context.deinit();
    
    const root = try parser.parseProgram();
    
    std.debug.print("Parsed:\n", .{});
    try ast.write_node(std.io.getStdErr().writer().any(), &context.nodes.items, &context.literals.items, root);
    std.debug.print("\n", .{});

    std.debug.print("AST as list:\n", .{});
    for (context.nodes.items) |n| {
        std.debug.print("{s}, ", .{@tagName(n.expr)});
    }
}