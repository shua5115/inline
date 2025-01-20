const std = @import("std");
const ast = @import("ast.zig");
const tokens = @import("tokens.zig");
const lexing = @import("lexing.zig");
const parsing = @import("parsing.zig");
const stringreader = @import("stringreader.zig");

const Lexer = lexing.Lexer;
const Parser = parsing.Parser;
const StringReader = stringreader.StringReader;


test "parse testing" {
    const alloc = std.testing.allocator;
    
    const source: []const u8 = 
    \\inline := "Hello, world!";
    \\add := [a, b](^a+b);
    \\table := {1, 2, 3};
    \\nil := ~;
    \\a,b,c := 1,2,3;
    \\multiassign=a=b=c; -- assignments evaluated from right to left
    \\y := -x^2; y == -x^2^1;
    \\math_grouping := a*(b+c);
    \\left_precedence := 1+2+3+4+5+6
    \\right_precedence := 1^2^3^4^5^6 -- is 1, but your computer probably can't compute this
    \\concat := "I have " .. 99 .. " problems";
    \\but := "semicolons aint " .. 1;
    \\comments... -- line comment
    \\--[[
    \\block.comments.too
    \\]]
    \\--[===[
    \\[[big]] block comments
    \\]===]
    \\bigstring = [[
    \\just how I like 'em
    \\with newlines and all
    \\]]
    \\biggerstring = [====[
    \\[[nesting? fr?]]
    \\]====]
    \\type := :obj -- lua: type(obj)
    \\metatable := ::obj -- lua: getmetatable(obj)
    \\::obj = meta -- lua: setmetatable(obj, meta)
    \\global_obj := @obj
    \\@obj = val;
    \\@3.141592 = "global pi?";
    \\()?(not_infinite_loop; ^^a);
    \\(a==!~)?(a = 1); -- while loop
    ;
    var sourcereader = StringReader.init(source);
    std.debug.print("Source:\n{s}\n", .{source});

    var parser = try Parser.init(alloc, sourcereader.reader().any());
    defer parser.deinit();
    
    const root = try parser.parseProgram();

    std.debug.print("\nParsed:\n", .{});
    try ast.write_node(std.io.getStdErr().writer().any(), &parser.nodes.items, &parser.lexer.literals.items, root);
    std.debug.print("\n", .{});

    const nliterals = parser.lexer.literals.items.len;
    std.debug.print("\nLiterals stored: {d}\n", .{nliterals});

    std.debug.print("\nAST as list:\n", .{});
    try ast.write_ast_list(std.io.getStdErr().writer().any(), parser.nodes.items, parser.lexer.literals.items);
}

test "Node memory layout" {
    std.debug.print(
        "sizeof(Node) = {d}\nalignof(Node) = {d}\n",
        .{@sizeOf(ast.Node), @alignOf(ast.Node)}
    );
    const info = @typeInfo(ast.Expression);
    const max_size: usize = 
        comptime blk: {
            var max: usize = 0;
            for(info.Union.fields) |f| {
                const sz = @sizeOf(f.type);
                max = @max(max, sz);
            }
            break :blk max;
        };
    
    std.debug.print(
        "sizeof(Expression) = {d}\nalignof(Expression) = {d}\nmax Expression size = {d}\n",
        .{@sizeOf(ast.Expression), @alignOf(ast.Expression), max_size}
    );
    
}