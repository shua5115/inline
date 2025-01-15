const std = @import("std");
const tokens = @import("tokens.zig");

const Token = tokens.Token;
const TokenType = tokens.TokenType;
const TokenLiteral = tokens.TokenLiteral;

pub const AstIndex = enum(usize) {
    FINAL = std.math.maxInt(usize),
    _
};

pub const Expression = union(enum) {
    Nil: Nil,
    Ellipsis: Ellipsis,
    Identifier: Identifier,
    Global: Global,
    NumberLiteral: NumberLiteral,
    StringLiteral: StringLiteral,
    FunctionLiteral: FunctionLiteral,
    TableLiteral: TableLiteral,
    PrefixExpression: PrefixExpression,
    InfixExpression: InfixExpression,
    CallExpression: CallExpression,
    Block: Block,
    Break: Break,
    Return: Return,
};

pub const Node = struct {
    /// Expression representation
    expr: Expression,
    /// Index to the next statement in a chain of statements
    next_index: AstIndex = AstIndex.FINAL,
};

pub const Nil = struct {};

pub const Ellipsis = struct {};

pub const Identifier = struct {
    literal_index: usize,
};

pub const Global = struct {
    rhs_index: AstIndex,
};

pub const NumberLiteral = struct {
    value: f64,
    literal_index: usize,
};

pub const StringLiteral = struct {
    literal_index: usize,
};

pub const FunctionLiteral = struct {
    params_index: AstIndex,
    body_index: AstIndex,
};

pub const TableLiteral = struct {
    start_index: AstIndex
};

pub const PrefixExpression = struct {
    rhs_index: AstIndex,
    op: TokenType,
};

pub const InfixExpression = struct {
    lhs_index: AstIndex,
    rhs_index: AstIndex,
    op: TokenType,
};

pub const CallExpression = struct {
    lhs_index: AstIndex,
    args_index: AstIndex,
};

pub const Block = struct {
    start_index: AstIndex,
};

pub const Break = struct {
    
};

pub const Return = struct {
    rhs_index: AstIndex,
};

pub fn write_ast_list(writer: std.io.AnyWriter, nodes: []const Node, literals: []const[]const u8) anyerror!void {
    for (nodes, 0..) |n, i| {
        try writer.print("{d}.\t{s}", .{i, @tagName(n.expr)});
        switch (n.expr) {
            .Identifier => try writer.print("({s})", .{literals[n.expr.Identifier.literal_index]}),
            .StringLiteral => try writer.print("({s})", .{literals[n.expr.StringLiteral.literal_index]}),
            .NumberLiteral => try writer.print("({s})", .{literals[n.expr.NumberLiteral.literal_index]}),
            .PrefixExpression => try writer.print("({s} ->{d})", .{tokens.token_type_str(n.expr.PrefixExpression.op) orelse "<INVALID OP>", @intFromEnum(n.expr.PrefixExpression.rhs_index)}),
            .InfixExpression => try writer.print("(->{d} {s} ->{d})", .{@intFromEnum(n.expr.InfixExpression.lhs_index), tokens.token_type_str(n.expr.InfixExpression.op) orelse "<INVALID OP>", @intFromEnum(n.expr.InfixExpression.rhs_index)}),
            .FunctionLiteral => try writer.print("(->{d})", .{@intFromEnum(n.expr.FunctionLiteral.body_index)}),
            .Block => try writer.print("(->{d})", .{@intFromEnum(n.expr.Block.start_index)}),
            .TableLiteral => try writer.print("(->{d})", .{@intFromEnum(n.expr.TableLiteral.start_index)}),
            else => {}
        }
        if (n.next_index != .FINAL) {
            try writer.print(" -> {d}", .{@intFromEnum(n.next_index)});
        }
        try writer.writeByte('\n');
    }
}

pub fn write_node(writer: std.io.AnyWriter, nodes: *const[]const Node, literals: *const[]const[]const u8, node_index: AstIndex) anyerror!void {
    if (node_index == .FINAL) return;
    const cur_index: usize = @intFromEnum(node_index);
    const cur_node = nodes.*[cur_index];
    // std.debug.print("{s} ", .{@tagName(cur_node.expr)});
    switch (cur_node.expr) {
        .Nil => {
            try writer.writeByte('~');
        },
        .Ellipsis => {
            _ = try writer.write("...");
        },
        .Identifier => |n| {
            _ = try writer.write(literals.*[n.literal_index]);
        },
        .Global => |n| {
            try writer.writeByte('@');
            try write_node(writer, nodes, literals, n.rhs_index);
        },
        .NumberLiteral => |n| {
            _ = try writer.write(literals.*[n.literal_index]);
        },
        .StringLiteral => |n| {
            try writer.writeByte('"');
            for (literals.*[n.literal_index]) |c| {
                _ = try switch (c) {
                    0x07 => writer.write("\\a"),
                    0x08 => writer.write("\\b"),
                    0x1B => writer.write("\\e"),
                    0x0C => writer.write("\\f"),
                    0x0A => writer.write("\\n"),
                    0x0D => writer.write("\\r"),
                    '\t' => writer.write("\\t"),
                    0x0B => writer.write("\\v"),
                    '\\' => writer.write("\\\\"),
                    '\'' => writer.write("\\'"),
                    '"' => writer.write("\\\""),
                    0x3F => writer.write("\\?"),
                    else => {try writer.writeByte(c); continue;}
                };
            }
            try writer.writeByte('"');
        },
        .FunctionLiteral => |n| {
            try writer.writeByte('[');
            var child_index = n.params_index;
            while(child_index != .FINAL) {
                try write_node(writer, nodes, literals, child_index);
                const child_node = nodes.*[@intFromEnum(child_index)];
                child_index = child_node.next_index;
                if (child_index != .FINAL) _ = try writer.write(", ");
            }
            _ = try writer.write("](");
            child_index = n.body_index;
            while(child_index != .FINAL) {
                try write_node(writer, nodes, literals, child_index);
                const child_node = nodes.*[@intFromEnum(child_index)];
                child_index = child_node.next_index;
                if (child_index != .FINAL) _ = try writer.write("; ");
            }
            try writer.writeByte(')');
        },
        .TableLiteral => |n| {
            try writer.writeByte('{');
            var child_index = n.start_index;
            while(child_index != .FINAL) {
                try write_node(writer, nodes, literals, child_index);
                const child_node = nodes.*[@intFromEnum(child_index)];
                child_index = child_node.next_index;
                if (child_index != .FINAL) _ = try writer.write(", ");
            }
            try writer.writeByte('}');
        },
        .CallExpression => |n| {
            try write_node(writer, nodes, literals, n.lhs_index);
            try writer.writeByte('(');
            var child_index = n.args_index;
            while(child_index != .FINAL) {
                try write_node(writer, nodes, literals, child_index);
                const child_node = nodes.*[@intFromEnum(child_index)];
                child_index = child_node.next_index;
                if (child_index != .FINAL) _ = try writer.write(", ");
            }
            try writer.writeByte(')');
        },
        .PrefixExpression => |n| {
            _ = try writer.write(tokens.token_type_str(n.op) orelse "<ILLEGAL PREFIX>");
            try write_node(writer, nodes, literals, n.rhs_index);
        },
        .InfixExpression => |n| {
            try write_node(writer, nodes, literals, n.lhs_index);
            try writer.writeByte(' ');
            _ = try writer.write(tokens.token_type_str(n.op) orelse "<ILLEGAL INFIX>");
            try writer.writeByte(' ');
            try write_node(writer, nodes, literals, n.rhs_index);
        },
        .Block => |n| {
            try writer.writeByte('(');
            var child_index = n.start_index;
            while(child_index != .FINAL) {
                try write_node(writer, nodes, literals, child_index);
                const child_node = nodes.*[@intFromEnum(child_index)];
                child_index = child_node.next_index;
                if (child_index != .FINAL) _ = try writer.write("; ");
            }
            try writer.writeByte(')');
        },
        .Break => {
            _ = try writer.write("^^");
        },
        .Return => |n| {
            try writer.writeByte('^');
            try write_node(writer, nodes, literals, n.rhs_index);
        },
    }
}

// TESTS

// test "size of Node" {
//     const print = std.debug.print;
//     const a = Node{
//         .expr = .{.Nil = .{}}
//     };
//     print("Size of node: {d}\n", .{@sizeOf(@TypeOf(a))});
// }