const std = @import("std");
const ast = @import("ast.zig");
const parsing = @import("parsing.zig");
const luavm = @import("luavm.zig");

const Parser = parsing.Parser;

pub const CodeGenerator = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    output: std.ArrayList(u8),
    nodes: []const ast.Node,
    literals: []const[]const u8,
    lines: []const c_int,

    pub fn init(allocator: std.mem.Allocator, nodes: []const ast.Node, literals: []const[]const u8, lines: []const c_int) Self {
        return Self {
            .allocator = allocator,
            .output = std.ArrayList(u8).init(allocator),
            .nodes = nodes,
            .literals = literals,
            .lines = lines,
        };
    }

    pub fn deinit(self: *Self) void {
        self.parser.deinit();
    }

    pub fn genProgram(self: *Self, root: ast.AstIndex, name: ?[]const u8) anyerror!void {
        try self.output.appendSlice(luavm.LUAC_HEADER);
        try self.genFunction(root, name);
    }

    fn genFunction(self: *Self, fn_literal: ast.AstIndex, name: ?[]const u8) anyerror!void {
        // source name
        if (name) |s| {
            try self.output.writer().writeInt(c_int, s.len, luavm.endianness);
            try self.output.appendSlice(s);
        }
        // line defined
        try self.output.writer().writeInt(c_int, self.lines[fn_literal], luavm.endianness);
        // last line defined (TBD)
        const last_line_defined_index = self.output.items.len;
        _ = last_line_defined_index;
        try self.output.writer().writeInt(c_int, 0, luavm.endianness);
        // number of upvalues (TBD)
        const nups_index = self.output.items.len;
        _ = nups_index;
        try self.output.append(0);
        // TODO
    }
};