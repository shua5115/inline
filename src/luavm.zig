const std = @import("std");
// CONFIGURATION

pub const lua_Number = f64;

// END CONFIGURATION

pub const luac_Version: u8 = 0x51;
pub const luac_Format: u8 = 0;
pub const luac_IsLittleEndian: u8 = @intFromBool(@import("builtin").target.cpu.arch.endian() == .little);
pub const luac_NumberIsInteger: u8 = @intFromBool(@typeInfo(lua_Number) == .Int);

pub const LUAC_HEADER = std.fmt.comptimePrint(
    "\x1BLUA{c}{c}{c}{c}{c}{c}{c}{c}",
    .{
        luac_Version, luac_Format, luac_IsLittleEndian,
        @sizeOf(c_int), @sizeOf(usize), @sizeOf(Instruction), @sizeOf(lua_Number),
        luac_NumberIsInteger
    }
);

pub const Instruction = u32;

pub const OpMode = enum {
    iABC, iABx, iAsBx
};

// R(i) = register at index i
// RK(i) = if (MSB(i)=0) register at index i else constant at index (i & 0xFF)
// Kst = constant list
// UpValue = upvalue list
// Glb = global table
pub const OpCode = enum(u6) {
                    //args      description
    OP_MOVE,        //A B       R(A) := R(B)
    OP_LOADK,       //A Bx      R(A) := Kst(Bx)
    OP_LOADBOOL,    //A B C     R(A) := (bool)B; if (C) pc++
    OP_LOADNIL,     //A B       R(A), R(A+1), ..., R(B-1), R(B) := nil
    OP_GETUPVAL,    //A B       R(A) := UpValue[B]

    OP_GETGLOBAL,   //A Bx      R(A) := Glb[Kst(Bx)]
    OP_GETTABLE,    //A B C     R(A) := R(B)[RK(C)]

    OP_SETGLOBAL,   //A Bx      Gbl[Kst(Bx)] := R(A)
    OP_SETUPVAL,    //A B       UpValue[B] := R(A)
    OP_SETTABLE,    //A B C     R(A)[RK(B)] := RK(C)

    OP_NEWTABLE,    //A B C     R(A) := {} with array capacity B and hash capacity C (as 1-byte floats)

    OP_SELF,        //A B C     R(A+1) := R(B); R(A) := R(B)[RK(C)]

    OP_ADD,         //A B C     R(A) := RK(B) + RK(C)
    OP_SUB,         //A B C     ...
    OP_MUL,         //A B C     ...
    OP_DIV,         //A B C     ...
    OP_MOD,         //A B C     ...
    OP_POW,         //A B C     ...
    OP_UNM,         //A B       R(A) := -R(B)
    OP_NOT,         //A B       R(A) := not R(B)
    OP_LEN,         //A B       R(A) := #R(B)

    OP_CONCAT,      //A B C     R(A) := R(B) .. R(B+1).....R(C-1) .. R(C)

    OP_JMP,         //sBx       pc += sBx
    // boolean tests: "if comparison doesn't equal expected value, skip next instruction"
    OP_EQ,          //A B C     if ((RK(B) == RK(C)) != A) pc++; "if result"
    OP_LT,          //A B C     if ((RK(B) < RK(C)) != A) pc++;
    OP_LE,          //A B C     if ((RK(B) <= RK(C)) != A) pc++;

    OP_TEST,        //A C       if ((bool)R(A) != (bool)C) pc++;
    OP_TESTSET,     //A B C     if ((bool)R(B) == (bool)C) R(A) := R(B) else pc++;

    OP_CALL,        //A B C     R(A), ..., R(A+C-2) := R(A)(R(A+1),...,R(A+B-1))
    OP_TAILCALL,    //A B C     return R(A)(R(A+1),...,R(A+B-1))
    OP_RETURN,      //A B       return R(A), ..., R(A+B-2)

    OP_FORLOOP,     //A sBx     (not using this one)
    OP_FORPREP,     //A sBx     (not using this one)
    OP_TFORLOOP,    //A C       (not using this one)

    OP_SETLIST,     //A B C     R(A)[(C-1)*FPF+i] := R(A+i), 1<=i<=B

    OP_CLOSE,       //A         close all variables in stack up to R(A)
    OP_CLOSURE,     //A Bx      R(A) := closure(KPROTO[Bx], R(A), ..., R(A+n))
    OP_VARARG,      //A B       R(A), R(A+1), ..., R(A+B-1) = unpack(vararg)
};

pub const NUM_OPCODES: comptime_int = @intFromEnum(OpCode.OP_VARARG)+1;

pub const OpArgMask = enum(u2) {
    /// Unused arg
    OpArgN,
    /// Used arg
    OpArgU,
    /// Arg is register or jump offset
    OpArgR,
    /// Arg is register or constant index
    OpArgK,
};

pub const MAXSTACK = 250;

pub const SIZE_C = 9;
pub const SIZE_B = 9;
pub const SIZE_Bx = SIZE_B + SIZE_C;
pub const SIZE_A = 8;
pub const SIZE_OP = 6;
pub const POS_OP = 0;
pub const POS_A = POS_OP + SIZE_OP;
pub const POS_C = POS_A + SIZE_A;
pub const POS_B = POS_C + SIZE_C;
pub const POS_Bx = POS_C;

pub inline fn mask1(n: comptime_int, p: comptime_int) Instruction {
    return ((~((~@as(Instruction, 0))<<n))<<p);
}

pub inline fn mask0(n: comptime_int, p: comptime_int) Instruction {
    return ~mask1(n, p);
}