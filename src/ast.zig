const std = @import("std");
const tokens = @import("tokens.zig");
const lua_Number = @import("luavm.zig").lua_Number;

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
    // PrefixExpression: PrefixExpression,
    Not: Not,
    Unm: Unm,
    Len: Len,
    Type: Type,
    Meta: Meta,
    // InfixExpression: InfixExpression,
    Comma: Comma,
    Assign: Assign,
    Or: Or,
    And: And,
    Eq: Eq,
    Neq: Neq,
    LT: LT,
    GT: GT,
    LE: LE,
    GE: GE,
    Plus: Plus,
    Minus: Minus,
    Mul: Mul,
    Div: Div,
    Mod: Mod,
    Pow: Pow,
    Concat: Concat,
    Index: Index,
    Call: Call,
    Loop: Loop,
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
    value: lua_Number,
    literal_index: usize,
};

pub const StringLiteral = struct {
    literal_index: usize,
};

pub const FunctionLiteral = struct {
    params_start: AstIndex,
    body_start: AstIndex,
};

pub const TableLiteral = struct {
    start_index: AstIndex
};

pub const Not = struct{
    rhs_index: AstIndex
};
pub const Unm = struct{
    rhs_index: AstIndex
};
pub const Len = struct{
    rhs_index: AstIndex
};
pub const Type = struct{
    rhs_index: AstIndex
};
pub const Meta = struct{
    rhs_index: AstIndex
};

pub const Comma = struct {
    lhs_index: AstIndex,
    rhs_index: AstIndex,
};
pub const Assign = struct {
    lhs_index: AstIndex,
    rhs_index: AstIndex,
};
pub const Or = struct {
    lhs_index: AstIndex,
    rhs_index: AstIndex,
};
pub const And = struct {
    lhs_index: AstIndex,
    rhs_index: AstIndex,
};
pub const Eq = struct {
    lhs_index: AstIndex,
    rhs_index: AstIndex,
};
pub const Neq = struct {
    lhs_index: AstIndex,
    rhs_index: AstIndex,
};
pub const LT = struct {
    lhs_index: AstIndex,
    rhs_index: AstIndex,
};
pub const GT = struct {
    lhs_index: AstIndex,
    rhs_index: AstIndex,
};
pub const LE = struct {
    lhs_index: AstIndex,
    rhs_index: AstIndex,
};
pub const GE = struct {
    lhs_index: AstIndex,
    rhs_index: AstIndex,
};
pub const Plus = struct {
    lhs_index: AstIndex,
    rhs_index: AstIndex,
};
pub const Minus = struct {
    lhs_index: AstIndex,
    rhs_index: AstIndex,
};
pub const Mul = struct {
    lhs_index: AstIndex,
    rhs_index: AstIndex,
};
pub const Div = struct {
    lhs_index: AstIndex,
    rhs_index: AstIndex,
};
pub const Mod = struct {
    lhs_index: AstIndex,
    rhs_index: AstIndex,
};
pub const Pow = struct {
    lhs_index: AstIndex,
    rhs_index: AstIndex,
};
pub const Concat = struct {
    lhs_index: AstIndex,
    rhs_index: AstIndex,
};
pub const Index = struct {
    lhs_index: AstIndex,
    rhs_index: AstIndex,
};

pub const Call = struct {
    lhs_index: AstIndex,
    args_index: AstIndex,
};

pub const Loop = struct {
    cond_index: AstIndex,
    body_index: AstIndex,
};

pub const Block = struct {
    start_index: AstIndex,
};

pub const Break = struct {};

pub const Return = struct {
    rhs_index: AstIndex,
};

pub fn write_ast_list(writer: std.io.AnyWriter, nodes: []const Node, literals: []const[]const u8) anyerror!void {
    for (nodes, 0..) |n, i| {
        try writer.print("{d}.\t{s}", .{i, @tagName(n.expr)});
        switch (n.expr) {
            .Nil => {},
            .Ellipsis => {},
            .Break => {},
            .Return => |e| if(e.rhs_index != .FINAL) {try writer.print("(->{d})", .{@intFromEnum(e.rhs_index)});},
            .Identifier => |e| try writer.print("({s})", .{literals[e.literal_index]}),
            .StringLiteral => |e| try writer.print("({s})", .{literals[e.literal_index]}),
            .NumberLiteral => |e| try writer.print("({s})", .{literals[e.literal_index]}),
            .Global => |e| try writer.print("(->{d})", .{@intFromEnum(e.rhs_index)}),
            .Not => |e| try writer.print("(! ->{d})", .{@intFromEnum(e.rhs_index)}),
            .Unm => |e| try writer.print("(- ->{d})", .{@intFromEnum(e.rhs_index)}),
            .Len => |e| try writer.print("(# ->{d})", .{@intFromEnum(e.rhs_index)}),
            .Type => |e| try writer.print("(: ->{d})", .{@intFromEnum(e.rhs_index)}),
            .Meta => |e| try writer.print("(:: ->{d})", .{@intFromEnum(e.rhs_index)}),
            .Comma => |e| try writer.print("(->{d} , ->{d})", .{@intFromEnum(e.lhs_index), @intFromEnum(e.rhs_index)}),
            .Assign => |e| try writer.print("(->{d} = ->{d})", .{@intFromEnum(e.lhs_index), @intFromEnum(e.rhs_index)}),
            .Or => |e| try writer.print("(->{d} | ->{d})", .{@intFromEnum(e.lhs_index), @intFromEnum(e.rhs_index)}),
            .And => |e| try writer.print("(->{d} & ->{d})", .{@intFromEnum(e.lhs_index), @intFromEnum(e.rhs_index)}),
            .Eq => |e| try writer.print("(->{d} == ->{d})", .{@intFromEnum(e.lhs_index), @intFromEnum(e.rhs_index)}),
            .Neq => |e| try writer.print("(->{d} != ->{d})", .{@intFromEnum(e.lhs_index), @intFromEnum(e.rhs_index)}),
            .LT => |e| try writer.print("(->{d} < ->{d})", .{@intFromEnum(e.lhs_index), @intFromEnum(e.rhs_index)}),
            .GT => |e| try writer.print("(->{d} > ->{d})", .{@intFromEnum(e.lhs_index), @intFromEnum(e.rhs_index)}),
            .LE => |e| try writer.print("(->{d} <= ->{d})", .{@intFromEnum(e.lhs_index), @intFromEnum(e.rhs_index)}),
            .GE => |e| try writer.print("(->{d} >= ->{d})", .{@intFromEnum(e.lhs_index), @intFromEnum(e.rhs_index)}),
            .Plus => |e| try writer.print("(->{d} + ->{d})", .{@intFromEnum(e.lhs_index), @intFromEnum(e.rhs_index)}),
            .Minus => |e| try writer.print("(->{d} - ->{d})", .{@intFromEnum(e.lhs_index), @intFromEnum(e.rhs_index)}),
            .Mul => |e| try writer.print("(->{d} * ->{d})", .{@intFromEnum(e.lhs_index), @intFromEnum(e.rhs_index)}),
            .Div => |e| try writer.print("(->{d} / ->{d})", .{@intFromEnum(e.lhs_index), @intFromEnum(e.rhs_index)}),
            .Mod => |e| try writer.print("(->{d} % ->{d})", .{@intFromEnum(e.lhs_index), @intFromEnum(e.rhs_index)}),
            .Pow => |e| try writer.print("(->{d} ^ ->{d})", .{@intFromEnum(e.lhs_index), @intFromEnum(e.rhs_index)}),
            .Concat => |e| try writer.print("(->{d} .. ->{d})", .{@intFromEnum(e.lhs_index), @intFromEnum(e.rhs_index)}),
            .Index => |e| try writer.print("(->{d} . ->{d})", .{@intFromEnum(e.lhs_index), @intFromEnum(e.rhs_index)}),
            .Call => |e| try writer.print("(->{d}(->{d}))", .{@intFromEnum(e.lhs_index), @intFromEnum(e.args_index)}),
            .FunctionLiteral => |e| try writer.print("([->{d}](->{d}))", .{@intFromEnum(e.params_start), @intFromEnum(e.body_start)}),
            .Block => try writer.print("(->{d})", .{@intFromEnum(n.expr.Block.start_index)}),
            .Loop => if (n.expr.Loop.cond_index == .FINAL) {
                    try writer.print("(? ->{d})", .{@intFromEnum(n.expr.Loop.body_index)});
                } else {
                    try writer.print("(->{d} ? ->{d})", .{@intFromEnum(n.expr.Loop.cond_index), @intFromEnum(n.expr.Loop.body_index)});
                },
            .TableLiteral => try writer.print("(->{d})", .{@intFromEnum(n.expr.TableLiteral.start_index)}),
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
            try writer.print("{any}", .{n.value});
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
            var child_index = n.params_start;
            while(child_index != .FINAL) {
                try write_node(writer, nodes, literals, child_index);
                const child_node = nodes.*[@intFromEnum(child_index)];
                child_index = child_node.next_index;
                if (child_index != .FINAL) _ = try writer.write(", ");
            }
            _ = try writer.write("](");
            child_index = n.body_start;
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
        .Call => |n| {
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
        .Not => |n| {
            try writer.writeByte('!');
            try write_node(writer, nodes, literals, n.rhs_index);
        },
        .Unm => |n| {
            try writer.writeByte('-');
            try write_node(writer, nodes, literals, n.rhs_index);
        },
        .Len => |n| {
            try writer.writeByte('#');
            try write_node(writer, nodes, literals, n.rhs_index);
        },
        .Type => |n| {
            try writer.writeByte(':');
            try write_node(writer, nodes, literals, n.rhs_index);
        },
        .Meta => |n| {
            _ = try writer.write("::");
            try write_node(writer, nodes, literals, n.rhs_index);
        },
        .Comma => |n| {
            try writer.writeByte('(');
            try write_node(writer, nodes, literals, n.lhs_index);
            _ = try writer.write(" , ");
            try write_node(writer, nodes, literals, n.rhs_index);
            try writer.writeByte(')');
        },
        .Assign => |n| {
            try writer.writeByte('(');
            try write_node(writer, nodes, literals, n.lhs_index);
            _ = try writer.write(" = ");
            try write_node(writer, nodes, literals, n.rhs_index);
            try writer.writeByte(')');
        },
        .Or => |n| {
            try writer.writeByte('(');
            try write_node(writer, nodes, literals, n.lhs_index);
            _ = try writer.write(" | ");
            try write_node(writer, nodes, literals, n.rhs_index);
            try writer.writeByte(')');
        },
        .And => |n| {
            try writer.writeByte('(');
            try write_node(writer, nodes, literals, n.lhs_index);
            _ = try writer.write(" & ");
            try write_node(writer, nodes, literals, n.rhs_index);
            try writer.writeByte(')');
        },
        .Eq => |n| {
            try writer.writeByte('(');
            try write_node(writer, nodes, literals, n.lhs_index);
            _ = try writer.write(" == ");
            try write_node(writer, nodes, literals, n.rhs_index);
            try writer.writeByte(')');
        },
        .Neq => |n| {
            try writer.writeByte('(');
            try write_node(writer, nodes, literals, n.lhs_index);
            _ = try writer.write(" != ");
            try write_node(writer, nodes, literals, n.rhs_index);
            try writer.writeByte(')');
        },
        .LT => |n| {
            try writer.writeByte('(');
            try write_node(writer, nodes, literals, n.lhs_index);
            _ = try writer.write(" < ");
            try write_node(writer, nodes, literals, n.rhs_index);
            try writer.writeByte(')');
        },
        .GT => |n| {
            try writer.writeByte('(');
            try write_node(writer, nodes, literals, n.lhs_index);
            _ = try writer.write(" > ");
            try write_node(writer, nodes, literals, n.rhs_index);
            try writer.writeByte(')');
        },
        .LE => |n| {
            try writer.writeByte('(');
            try write_node(writer, nodes, literals, n.lhs_index);
            _ = try writer.write(" <= ");
            try write_node(writer, nodes, literals, n.rhs_index);
            try writer.writeByte(')');
        },
        .GE => |n| {
            try writer.writeByte('(');
            try write_node(writer, nodes, literals, n.lhs_index);
            _ = try writer.write(" >= ");
            try write_node(writer, nodes, literals, n.rhs_index);
            try writer.writeByte(')');
        },
        .Plus => |n| {
            try writer.writeByte('(');
            try write_node(writer, nodes, literals, n.lhs_index);
            _ = try writer.write(" + ");
            try write_node(writer, nodes, literals, n.rhs_index);
            try writer.writeByte(')');
        },
        .Minus => |n| {
            try writer.writeByte('(');
            try write_node(writer, nodes, literals, n.lhs_index);
            _ = try writer.write(" - ");
            try write_node(writer, nodes, literals, n.rhs_index);
            try writer.writeByte(')');
        },
        .Mul => |n| {
            try writer.writeByte('(');
            try write_node(writer, nodes, literals, n.lhs_index);
            _ = try writer.write(" * ");
            try write_node(writer, nodes, literals, n.rhs_index);
            try writer.writeByte(')');
        },
        .Div => |n| {
            try writer.writeByte('(');
            try write_node(writer, nodes, literals, n.lhs_index);
            _ = try writer.write(" / ");
            try write_node(writer, nodes, literals, n.rhs_index);
            try writer.writeByte(')');
        },
        .Mod => |n| {
            try writer.writeByte('(');
            try write_node(writer, nodes, literals, n.lhs_index);
            _ = try writer.write(" % ");
            try write_node(writer, nodes, literals, n.rhs_index);
            try writer.writeByte(')');
        },
        .Pow => |n| {
            try writer.writeByte('(');
            try write_node(writer, nodes, literals, n.lhs_index);
            _ = try writer.write(" ^ ");
            try write_node(writer, nodes, literals, n.rhs_index);
            try writer.writeByte(')');
        },
        .Concat => |n| {
            try writer.writeByte('(');
            try write_node(writer, nodes, literals, n.lhs_index);
            _ = try writer.write(" .. ");
            try write_node(writer, nodes, literals, n.rhs_index);
            try writer.writeByte(')');
        },
        .Index => |n| {
            try writer.writeByte('(');
            try write_node(writer, nodes, literals, n.lhs_index);
            _ = try writer.write(" . ");
            try write_node(writer, nodes, literals, n.rhs_index);
            try writer.writeByte(')');
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
        .Loop => |n| {
            try write_node(writer, nodes, literals, n.cond_index);
            try writer.writeByte('?');
            try write_node(writer, nodes, literals, n.body_index);
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