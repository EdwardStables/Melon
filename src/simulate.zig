const std = @import("std");
const ArrayList = std.ArrayList;
const assert = std.debug.assert;

const module = @import("module.zig");

const SimulationError = error {
    SchedulerError,
    UnexpectedBehaviorError,
};

const SimulationResult = enum {
    FinishedSuccess
};

const DAG = struct {
    comb_order: ArrayList(usize), //Statically defined order to run combinational logic updates
};

pub fn build_DAG() SimulationError!DAG {

}

// Continuously run the event loop until either no events are enabled, there is an error, or a finish command is set
pub fn run(mod: module.Module, dag: DAG) SimulationError!SimulationResult {
    while (true) {
        if (try run_cycle(mod, dag)) |result| {
            assert(result == SimulationResult.FinishedSuccess);
            std.debug.print("Got simulation finish event, stopping loop\n", .{});
            break;
        }
    }
}

pub fn run_cycle(mod: module.Module, dag: DAG) SimulationError!?SimulationResult {
    //Copy the current state to independently evaluate procedures
    try mod.sample_inputs();

    //Evaluate all the procedures
    for (mod.proc.items) |proc| {
        proc.evaluate(mod);
    }

    for (dag.comb_order.items) |comb_index| {
        mod.comb.items[comb_index].evaluate(mod);
    }
}
