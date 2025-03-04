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
    comb_order: ArrayList(module.CombID), //Statically defined order to run combinational logic updates

    pub fn deinit(self: DAG) void {
        self.comb_order.deinit();
    }
};

pub fn build_DAG(mod: module.Module, alloc: std.mem.Allocator) !DAG {
    // Kahn's Algorithm (https://en.wikipedia.org/wiki/Topological_sorting#Kahn's_algorithm)
    // - Create set S of nodes with no incoming edges (aka, when incoming edges only belong to the source outputs)
    // - Create empty list L of topologically sorted nodes
    // Repeat until S is empty:
    //     - Remove node s from S
    //     - Add s to L
    //     - For each node m with an edge e from n to m:
    //          - Remove e from graph
    //          - If m has no other incoming edges then insert m into L

    //Create the graph structure
    //TODO: Really this should be its own struct with fully contiguous memory. Nested ArrayList has extra indirection
    const table_size = mod.comb.items.len;
    var graph = try ArrayList(ArrayList(bool)).initCapacity(alloc, table_size);
    defer {
        for (graph.items) |row| {
            row.deinit();
        }
        graph.deinit();
    }

    // Set all entries to false
    for (0..table_size) |_| {
        var row = try ArrayList(bool).initCapacity(alloc, table_size);
        for (0..table_size) |_| {
            try row.append(false);
        }
        try graph.append(row);
    }

    // Set fields to true when there is an edge
    for (mod.comb.items, 0..) |mod1, i| {
        for (mod.comb.items, 0..) |mod2, j| {
            if (i==j) continue;
            
            output_loop: for (mod1.outputs.items) |src| {
                for (mod2.inputs.items) |snk| {
                    if (src == snk) {
                        graph.items[i].items[j] = true;
                        break :output_loop;
                    }
                }
            }
        }
    }

    // Search throw graph to build S (nodes where there are no inputs in the graph)
    var S = ArrayList(module.CombID).init(alloc);
    defer S.deinit();

    for (0..table_size) |row| {
        var has_input = false;
        for (0..table_size) |col| {
            if (graph.items[row].items[col]) {
                has_input = true;
                break;
            }
        }

        if (!has_input) {
            try S.append(@intCast(row));
        }
    }

    // Create L ahead of time
    // Used in return, do not deinit
    var L = try ArrayList(module.CombID).initCapacity(alloc, table_size);

    while (S.popOrNull()) |node| {
        try L.append(node);
        for (graph.items[node].items, 0..) |has_link, dest| {
            if (!has_link) continue;
            graph.items[node].items[dest] = false;

            for (0..table_size) |i| {
                if (graph.items[i].items[dest]) break;
            } else {
                try S.append(@intCast(dest));
            }
        }
    }

    return .{.comb_order=L};
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

test "one item DAG" {
    var mod = try module.Module.init(0, std.testing.allocator);
    defer mod.deinit();

    const s1 = try mod.addSignal(1);
    const s2 = try mod.addSignal(1);
    const comb = try mod.addComb();

    try mod.addCombInput(comb, s1);
    try mod.addCombOutput(comb, s2);

    var dag = try build_DAG(mod, std.testing.allocator);
    defer dag.deinit();

    assert(std.mem.eql(module.CombID, dag.comb_order.items, &[_]module.CombID{0}));
}
