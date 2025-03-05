const std = @import("std");
const ArrayList = std.ArrayList;
const assert = std.debug.assert;

const module = @import("module.zig");
const cdag = @import("comb_dag.zig");
const DAG = cdag.DAG;

const SimulationError = error {
    SchedulerError,
    UnexpectedBehaviorError,
};

pub const SimulationResult = enum {
    FinishedSuccess
};


// Continuously run the event loop until either no events are enabled, there is an error, or a finish command is set
pub fn run(mod: *module.Module, dag: DAG, cycle_limit: u64) !SimulationResult {
    for (0..cycle_limit) |cycle| {
        std.log.info("Cycle {}", .{cycle});
        if (try run_cycle(mod, dag)) |result| {
            assert(result == SimulationResult.FinishedSuccess);
            std.debug.print("Got simulation finish event, stopping loop\n", .{});
            break;
        }
    }

    return SimulationResult.FinishedSuccess;
}

pub fn run_cycle(mod: *module.Module, dag: DAG) !?SimulationResult {
    //Copy the current state to independently evaluate procedures
    try mod.sample_inputs();

    //Evaluate all the procedures
    for (mod.proc.items) |proc| {
        proc.evaluate(mod);
    }

    for (dag.comb_order.items) |comb_index| {
        mod.comb.items[comb_index].evaluate(mod);
    }

    //TODO return FinishedSuccess if an end simulation directive is found

    return null;
}
