const std = @import("std");
const ArrayList = std.ArrayList;
const assert = std.debug.assert;

const SignalID = u16;
const ModuleID = u16;

const Signal = struct { //Basic implementation limits width 0-255 bits
    width: u8,          //and value limited to 2^64-1
    id: SignalID,
    type: enum {            //Informs the meaning of the data being held without needing to test the
        Reference,          //union. Later will be extended to include data beyond 64 bits
        Direct
    },
    val: union {            //Depending on the type it may be a reference to a signal held elsewhere
        direct: u64,        //Intended for use with input signals
        reference: *Signal
    },
};

const Comb = struct {
    id: u16,
    inputs: ArrayList(SignalID), //Signals that trigger the combinational logic to run
    outputs: ArrayList(SignalID), //Signals that this block drives

    //IR Representation

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

const Proc = struct {
    id: u16,
    clk: Trigger,    //Must be present, may be set by default
    reset: ?Trigger, //Optional async reset
    inputs: ArrayList(SignalID), //Signals that this block may sample upon trigger
    outputs: ArrayList(SignalID), //Signals that this block drives

    //IR Representation

    pub fn evaluate(self: Proc, mod: Module) void {
        //evaluate the procedure from the ir using the input copy
        _ = self;
        _ = mod;
    }
};

const Module = struct {
    id: ModuleID,
    signals: ArrayList(Signal),
    input_copy: ArrayList(Signal),
    comb: ArrayList(Comb),
    proc: ArrayList(Proc),

    pub fn init(alloc: std.mem.Allocator) !Module {
        var module: Module = .{};
        module.signals = ArrayList(Signal).init(alloc);
        module.input_copy = ArrayList(Signal).init(alloc);
        module.comb = ArrayList(Comb).init(alloc);
        module.proc = ArrayList(Proc).init(alloc);
    }

    // Make a copy of signals at the given time s.t. independent proc blocks can sample their values
    pub fn sample_inputs(self: Module) !void {
        self.input_copy.clearRetainingCapacity();
        for (self.signals.items) |signal| {
            self.input_copy.append(signal);
        }
    }
};