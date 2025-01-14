const std = @import("std");
const Lexer_Module = @import("lexing.zig");

const Lexer = Lexer_Module.Lexer;

const C = @cImport({
    @cInclude("lua.h");
    @cInclude("lualib.h");
    @cInclude("lauxlib.h");
});

pub const lua_State = ?*C.struct_lua_State;
const testing = std.testing;

// pub export fn parse(L: lua_State, reader: std.io.AnyReader) !void {
//     const buf = [32]u8{0};
//     const read = try reader.read(buf);
//     if (read == 0) return;
// }

test "lua stdlib usage" {
    const L = C.lua_open();
    defer C.lua_close(L);
    C.luaL_openlibs(L);
    C.lua_getglobal(L, "print");
    C.lua_pushstring(L, "hello lua!");
    C.lua_call(L, 1, 0);
}