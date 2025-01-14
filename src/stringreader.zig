const std = @import("std");

pub const StringReader = struct {
    const Self = @This();
    const Context = *Self;
    const ReadError = error{ IMPOSSIBLE };
    const Reader = std.io.GenericReader(*Self, ReadError, read);

    str: []const u8,
    pos: usize = 0,

    pub fn init(str: []const u8) Self {
        return Self{.str=str};
    }

    pub fn reader(self: *Self) Reader {
        return Reader{ .context = self };
    }

    pub fn read(self: *Self, dest: []u8) ReadError!usize {
        // shamelessly copied from std
        // because std needs to fill this niche
        const size: usize = @min(dest.len, self.str.len - self.pos);
        const end = self.pos + size;

        @memcpy(dest[0..size], self.str[self.pos..end]);
        self.pos = end;

        return size;
    }
};