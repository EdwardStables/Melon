const std = @import("std");
const ArrayList = std.ArrayList;
const assert = std.debug.assert;
const testing = std.testing;

const module = @import("module.zig");
const Module = module.Module;

// An IR that implements the operations within a single proc or comb block

const IRInstr = enum {
    ADD,
    SUB,
    LSL,
    LSR,
    CONCAT,
    AND,
    OR,
    XOR,
    INV,
    AND_REDUCE,
    OR_REDUCE,
    XOR_REDUCE,
    BEQUAL,
    BZERO,
    BNZERO,
    STOP,
};

const IRInstrType = enum {
    NONE_NONE,
    ONE_TWO,
    ONE_ONE,
    BRA_TWO,
    BRA_ONE,

    pub fn get_type(instr: IRInstr) IRInstrType {
        return switch (instr) {
            IRInstr.ADD => IRInstrType.ONE_TWO,
            IRInstr.SUB => IRInstrType.ONE_TWO,
            IRInstr.LSL => IRInstrType.ONE_TWO,
            IRInstr.LSR => IRInstrType.ONE_TWO,
            IRInstr.CONCAT => IRInstrType.ONE_TWO,
            IRInstr.AND => IRInstrType.ONE_TWO,
            IRInstr.OR => IRInstrType.ONE_TWO,
            IRInstr.XOR => IRInstrType.ONE_TWO,
            IRInstr.INV => IRInstrType.ONE_ONE,
            IRInstr.AND_REDUCE => IRInstrType.ONE_ONE,
            IRInstr.OR_REDUCE => IRInstrType.ONE_ONE,
            IRInstr.XOR_REDUCE => IRInstrType.ONE_ONE,
            IRInstr.BEQUAL => IRInstrType.BRA_TWO,
            IRInstr.BZERO => IRInstrType.BRA_TWO,
            IRInstr.BNZERO => IRInstrType.BRA_TWO,
            IRInstr.STOP => IRInstrType.NONE_NONE,
        };
    }
};

const IRSignalID = u16;
/// IR version of signal used in source code.
/// Virtual, therefore can be created as needed.
/// Has a width and a signal it derives from, if not created as a purely intermediatary signal
const IRSignal = struct {
    id: IRSignalID,
    width: u8,
    ref: ?module.SignalID,
    val: u64,
};

const IRStatementID = u16;
const IRStatement = struct {
    id: IRStatementID,
    instr: IRInstr,
    type: IRInstrType,

    target: ?IRStatementID,

    output: ?IRSignalID,
    input1: ?IRSignalID,
    input2: ?IRSignalID,

    const Self = @This();

    pub fn make(id: IRStatementID, instr: IRInstr, args: []const IRSignalID, target: ?IRStatementID) IRStatement {
        const stmt_type = IRInstrType.get_type(instr);

        const stmt_target = switch (stmt_type) {
            IRInstrType.BRA_ONE, IRInstrType.BRA_TWO => target orelse unreachable,
            else => null,
        };

        const output = switch (stmt_type) {
            IRInstrType.NONE_NONE, IRInstrType.BRA_TWO, IRInstrType.BRA_ONE => null,
            else => args[0],
        };

        const in1 = switch (stmt_type) {
            IRInstrType.NONE_NONE => null,
            IRInstrType.BRA_TWO, IRInstrType.BRA_ONE => args[0],
            else => args[1],
        };

        const in2 = switch (stmt_type) {
            IRInstrType.ONE_TWO => args[2],
            IRInstrType.BRA_TWO => args[1],
            else => null,
        };

        return .{
            .id = id,
            .instr = instr,
            .type = stmt_type,
            .target = stmt_target,
            .output = output,
            .input1 = in1,
            .input2 = in2,
        };
    }

    pub fn execute(self: Self, block: *IRBlock) void {
        const v1 = if (self.input1) |in1| block.get(in1) else null;
        const v2 = if (self.input2) |in2| block.get(in2) else null;
        switch (self.instr) {
            IRInstr.ADD => {
                block.set(self.output.?, v1.?.val +% v2.?.val);
            },
            IRInstr.SUB => {
                block.set(self.output.?, v1.?.val -% v2.?.val);
            },
            IRInstr.LSL => {
                block.set(self.output.?, v1.?.val << @as(u6, @intCast(v2.?.val))); //TODO u6 is only temporary here
            },
            IRInstr.LSR => {
                block.set(self.output.?, v1.?.val >> @as(u6, @intCast(v2.?.val)));
            },
            IRInstr.CONCAT => {
                block.set(self.output.?, (v1.?.val << @as(u6, @intCast(v2.?.width))) | v2.?.val);
            },
            IRInstr.AND => {
                block.set(self.output.?, v1.?.val & v2.?.val);
            },
            IRInstr.OR => {
                block.set(self.output.?, v1.?.val | v2.?.val);
            },
            IRInstr.XOR => {
                block.set(self.output.?, v1.?.val ^ v2.?.val);
            },
            IRInstr.INV => {
                block.set(self.output.?, ~v1.?.val);
            },
            IRInstr.AND_REDUCE => {
                assert(block.get(self.output.?).width == 1);
                for (0..v1.?.width) |ind| {
                    if ((1 & (v1.?.val >> @as(u6, @intCast(ind)))) == 0) {
                        block.set(self.output.?, 0);
                        break;
                    }
                } else {
                    block.set(self.output.?, 1);
                }
            },
            IRInstr.OR_REDUCE => {
                assert(block.get(self.output.?).width == 1);
                for (0..v1.?.width) |ind| {
                    if ((1 & (v1.?.val >> @as(u6, @intCast(ind)))) == 1) {
                        block.set(self.output.?, 1);
                        break;
                    }
                } else {
                    block.set(self.output.?, 0);
                }
            },
            IRInstr.XOR_REDUCE => {
                assert(block.get(self.output.?).width == 1);
                var count: usize = 0;
                for (0..v1.?.width) |ind| {
                    if ((1 & (v1.?.val >> @as(u6, @intCast(ind)))) == 1) {
                        count += 1;
                    }

                    if (count > 1) {
                        block.set(self.output.?, 0);
                        break;
                    }
                } else {
                    if (count == 1) {
                        block.set(self.output.?, 1);
                    } else {
                        block.set(self.output.?, 0);
                    }
                }
            },
            IRInstr.BEQUAL => {
                if (v1.?.val == v2.?.val) {
                    block.setTarget(self.target.?);
                }
            },
            IRInstr.BZERO => {
                if (v1.?.val == 0) {
                    block.setTarget(self.target.?);
                }
            },
            IRInstr.BNZERO => {
                if (v1.?.val != 0) {
                    block.setTarget(self.target.?);
                }
            },
            IRInstr.STOP => {
                assert(false);
            },
        }
    }
};

/// Contains all the IR statements for a single comb/proc block
/// TODO: This will probably accept basic blocks and flatten them into statements
/// for not just accept single statements and assume they are valid
/// Input values are copied to virtual signals, proc/comb provides the reference for inputs.
/// Code generation should ensure proc blocks reusing assigned values still refer to the input, so it doesn't affect this code
pub const IRBlock = struct {
    block: module.Block,
    statements: ArrayList(IRStatement),
    signals: ArrayList(IRSignal),

    const Self = @This();

    pub fn init(mod: *Module, block: module.Block, alloc: std.mem.Allocator) !IRBlock {
        var self: IRBlock = .{
            .block = block,
            .statements = ArrayList(IRStatement).init(alloc),
            .signals = ArrayList(IRSignal).init(alloc),
        };

        //Make ref signals for IR internals
        switch (block) {
            .CombBlock => |id| {
                for (mod.comb.items[id].inputs.items) |comb_input| {
                    const s = mod.signals.items[comb_input];
                    try self.signals.append(.{ .id = @as(u16, @intCast(self.signals.items.len)), .width = s.width, .ref = comb_input, .val = 0 });
                }
                for (mod.comb.items[id].outputs.items) |comb_output| {
                    const s = mod.signals.items[comb_output];
                    try self.signals.append(.{ .id = @as(u16, @intCast(self.signals.items.len)), .width = s.width, .ref = comb_output, .val = 0 });
                }
            },
            .ProcBlock => |id| {
                for (mod.proc.items[id].inputs.items) |proc_input| {
                    const s = mod.signals.items[proc_input];
                    try self.signals.append(.{ .id = @as(u16, @intCast(self.signals.items.len)), .width = s.width, .ref = proc_input, .val = 0 });
                }
                for (mod.proc.items[id].outputs.items) |proc_output| {
                    const s = mod.signals.items[proc_output];
                    try self.signals.append(.{ .id = @as(u16, @intCast(self.signals.items.len)), .width = s.width, .ref = proc_output, .val = 0 });
                }
            },
        }

        return self;
    }

    pub fn deinit(self: Self) void {
        self.statements.deinit();
        self.signals.deinit();
    }

    fn getIRSignalFromModuleSignal(self: Self, signal: module.SignalID) ?IRSignalID {
        for (self.signals.items, 0..) |s, id| {
            if (s.ref) |ref| {
                if (ref == signal) {
                    return @as(u16, @intCast(id));
                }
            }
        }
        return null;
    }

    fn set(self: Self, id: IRSignalID, val: u64) void {
        const mask = (@as(u64, 0) -% 1) >> @as(u6, @intCast(64 - self.signals.items[id].width));
        self.signals.items[id].val = mask & val;
    }

    fn get(self: Self, id: IRSignalID) IRSignal {
        return self.signals.items[id];
    }

    fn setTarget(self: Self, target: IRStatementID) void {
        _ = self;
        _ = target;
        //TODO
        assert(false);
    }

    pub fn addStatement(self: *Self, statement: IRStatement) !void {
        var stmt = statement;
        stmt.id = @as(IRStatementID, @intCast(self.statements.items.len));
        try self.statements.append(stmt);
    }

    /// Update the internal values of IRSignals based on module signals, takes a reference to the intended source
    /// as proc and comb blocks handle this differently
    fn sync_from(self: *Self, ref: *ArrayList(module.Signal)) void {
        for (self.signals.items, 0..) |signal, ind| {
            if (signal.ref) |ref_id| {
                self.signals.items[ind].val = ref.items[ref_id].get();
            }
        }
    }
    fn sync_to(self: Self, ref: *ArrayList(module.Signal)) void {
        for (self.signals.items) |signal| {
            if (signal.ref) |ref_id| {
                ref.items[ref_id].set(signal.val);
            }
        }
    }

    /// Assume the signals have already been copied
    pub fn execute(self: *Self, mod: *Module) void {
        //Update internal copies
        self.sync_from(switch (self.block) {
            .CombBlock => &mod.signals,
            .ProcBlock => &mod.input_copy,
        });

        var index: usize = 0;

        while (index < self.statements.items.len) {
            self.statements.items[index].execute(self);
            index += 1; //Increment as a PC
        }

        self.sync_to(&mod.signals);
    }
};

test "Test Instrs" {
    var mod = try Module.init(0, testing.allocator);
    defer mod.deinit();

    //Little two input block
    const comb = try mod.addComb();
    const in1 = try mod.addSignal(4);
    const in2 = try mod.addSignal(4);
    const out1 = try mod.addSignal(4);

    try mod.addCombInput(comb, in1);
    try mod.addCombInput(comb, in2);
    try mod.addCombOutput(comb, out1);

    var block = try IRBlock.init(&mod, module.Block{ .CombBlock = comb }, testing.allocator);
    defer block.deinit();

    const ir_in1 = block.getIRSignalFromModuleSignal(in1) orelse unreachable;
    const ir_in2 = block.getIRSignalFromModuleSignal(in2) orelse unreachable;
    const ir_out1 = block.getIRSignalFromModuleSignal(out1) orelse unreachable;
    const args = [_]IRSignalID{ ir_out1, ir_in1, ir_in2 };
    try block.addStatement(IRStatement.make(0, .ADD, &args, null));

    mod.signals.items[in1].val = module.SignalValue{ .Direct = 4 };
    mod.signals.items[in2].val = module.SignalValue{ .Direct = 4 };

    block.execute(&mod);

    try testing.expectEqual(8, mod.signals.items[out1].val.Direct);
}
