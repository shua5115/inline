const std = @import("std");

pub const Allocator = std.testing.allocator;

pub const assert = std.debug.assert;

pub fn assert_errmsg(ok: bool, comptime fmt: []const u8, args: anytype) void {
    if (ok) return;
    std.debug.print(fmt, args);
    unreachable; // assertion failure
}