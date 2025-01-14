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
    rhs_index: AstIndex,
};

pub const Return = struct {
    rhs_index: AstIndex,
};

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
        .Break => |n| {
            try writer.writeByte('^');
            try write_node(writer, nodes, literals, n.rhs_index);
            if (cur_node.next_index != .FINAL) _ = try writer.write("; ");
        },
        .Return => |n| {
            _ = try writer.write("^^");
            try write_node(writer, nodes, literals, n.rhs_index);
            if (cur_node.next_index != .FINAL) _ = try writer.write("; ");
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