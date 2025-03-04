const std = @import("std");
const ArrayList = std.ArrayList;
const assert = std.debug.assert;

pub const SignalID = u16;
pub const ModuleID = u16;
pub const CombID = u16;
pub const ProcID = u16;

pub const SignalType = enum { Reference, Direct }; //Depending on the type it may be a reference to a signal held elsewhere
pub const SignalValue = union(SignalType) {
    Reference: *Signal,
    Direct: u64                                //Value limited to 2^64-1, later improvements will make this arbitrary
};

pub const Signal = struct { //Basic implementation limits width 0-255 bits
    id: SignalID,
    width: u8,              
    val: SignalValue,
    pub fn make(id: SignalID, width: u8) Signal {
        return .{
            .id = id,
            .width = width,
            .val = .{.Direct = 0},
        };
    }
};

pub const Comb = struct {
    id: CombID,
    inputs: ArrayList(SignalID), //Signals that trigger the combinational logic to run
    outputs: ArrayList(SignalID), //Signals that this block drives

    //IR Representation

    pub fn init(id: CombID, alloc: std.mem.Allocator) !Comb {
        return .{
            .id = id,
            .inputs = ArrayList(SignalID).init(alloc),
            .outputs = ArrayList(SignalID).init(alloc),
        };
    }

    pub fn deinit(self: Comb) void {
        self.inputs.deinit();
        self.outputs.deinit();
    }

    pub fn evaluate(self: Comb, mod: Module) void {
        //evaluate the procedure from the ir using the input copy
        _ = self;
        _ = mod;
    }
};


const Trigger = struct {
    signal: SignalID,
    edge: enum {
        NegEdge,
        Posedge
    },
};

pub const Proc = struct {
    id: ProcID,
    clk: Trigger,    //Must be present, may be set by default
    reset: ?Trigger, //Optional async reset
    inputs: ArrayList(SignalID), //Signals that this block may sample upon trigger
    outputs: ArrayList(SignalID), //Signals that this block drives

    //IR Representation

    pub fn init(id: ProcID, alloc: std.mem.Allocator) !Proc {
        return .{
            .id = id,
            .inputs = ArrayList(SignalID).init(alloc),
            .outputs = ArrayList(SignalID).init(alloc),
        };
    }

    pub fn deinit(self: Proc) void {
        self.inputs.deinit();
        self.outputs.deinit();
    }


    pub fn evaluate(self: Proc, mod: Module) void {
        //evaluate the procedure from the ir using the input copy
        _ = self;
        _ = mod;
    }
};

pub const Module = struct {
    id: ModuleID,
    signals: ArrayList(Signal),
    input_copy: ArrayList(Signal),
    comb: ArrayList(Comb),
    proc: ArrayList(Proc),

    alloc: std.mem.Allocator,

    pub fn init(id: ModuleID, alloc: std.mem.Allocator) !Module {
        return .{
            .id = id,
            .signals    = ArrayList(Signal).init(alloc),
            .input_copy = ArrayList(Signal).init(alloc),
            .comb       = ArrayList(Comb).init(alloc),
            .proc       = ArrayList(Proc).init(alloc),
            .alloc      = alloc,
        };
    }

    pub fn deinit(self: Module) void {
        self.signals.deinit();
        self.input_copy.deinit();
        for (self.comb.items) |c| c.deinit();
        self.comb.deinit();
        for (self.proc.items) |p| p.deinit();
        self.proc.deinit();
    }

    pub fn addSignal(self: *Module, width: u8) !SignalID {
        const id: SignalID = @intCast(self.signals.items.len);
        try self.signals.append(Signal.make(id, width));
        return id;
    }

    pub fn addComb(self: *Module) !CombID {
        const id: CombID = @intCast(self.comb.items.len);
        try self.comb.append(try Comb.init(id, self.alloc));
        return id;
    }

    pub fn addProc(self: *Module) !ProcID {
        const id: ProcID = @intCast(self.proc.items.len);
        try self.proc.append(try Proc.init(id, self.alloc));
        return id;
    }

    pub fn addCombInput(self: *Module, comb: CombID, signal: SignalID) !void {
        assert(@as(u16, signal) < self.signals.items.len);
        try self.comb.items[comb].inputs.append(signal);
    }

    pub fn addCombOutput(self: *Module, comb: CombID, signal: SignalID) !void {
        assert(@as(u16, signal) < self.signals.items.len);
        try self.comb.items[comb].outputs.append(signal);
    }

    pub fn addProcInput(self: *Module, proc: ProcID, signal: SignalID) !void {
        assert(@as(u16, signal) < self.signals.items.len);
        try self.proc.items[proc].inputs.append(signal);
    }
    pub fn addProcOutput(self: *Module, proc: ProcID, signal: SignalID) !void {
        assert(@as(u16, signal) < self.signals.items.len);
        try self.proc.items[proc].outputs.append(signal);
    }

    // Make a copy of signals at the given time s.t. independent proc blocks can sample their values
    pub fn sample_inputs(self: Module) !void {
        self.input_copy.clearRetainingCapacity();
        for (self.signals.items) |signal| {
            self.input_copy.append(signal);
        }
    }
};