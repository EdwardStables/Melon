const std = @import("std");
const ArrayList = std.ArrayList;
const assert = std.debug.assert;

const module = @import("module.zig");

const SimulationError = error {
    LoopedDAGError,
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

test "make graph" {
    var inp = try ArrayList(module.Comb).initCapacity(std.testing.allocator, 2);
    defer inp.deinit();

    try inp.append(try module.Comb.init(0, std.testing.allocator));
    try inp.append(try module.Comb.init(1, std.testing.allocator));
    defer inp.items[0].deinit();
    defer inp.items[1].deinit();

    try inp.items[0].inputs.append(0);
    try inp.items[0].outputs.append(1);
    try inp.items[0].outputs.append(2);

    try inp.items[1].inputs.append(1);
    try inp.items[1].outputs.append(3);

    var g = try make_graph(inp, std.testing.allocator);
    
    try std.testing.expectEqual(2, g.items.len);
    try std.testing.expectEqual(2, g.items[0].items.len);
    try std.testing.expectEqual(2, g.items[1].items.len);
    try std.testing.expect(!g.items[0].items[0]);
    try std.testing.expect( g.items[0].items[1]);
    try std.testing.expect(!g.items[1].items[0]);
    try std.testing.expect(!g.items[1].items[1]);

    g.items[0].deinit();
    g.items[1].deinit();
    g.deinit();
}

test "self loop graph" {
    var inp = try ArrayList(module.Comb).initCapacity(std.testing.allocator, 2);
    defer inp.deinit();

    try inp.append(try module.Comb.init(0, std.testing.allocator));
    defer inp.items[0].deinit();

    try inp.items[0].inputs.append(0);
    try inp.items[0].outputs.append(0);

    var g = try make_graph(inp, std.testing.allocator);
    try std.testing.expectEqual(1, g.items.len);
    try std.testing.expectEqual(1, g.items[0].items.len);
    try std.testing.expect(g.items[0].items[0]);
    g.items[0].deinit();
    g.deinit();
}

const Graph = ArrayList(ArrayList(bool));
fn make_graph(comb: ArrayList(module.Comb), alloc: std.mem.Allocator) !Graph {
    //TODO: Really this should be its own struct with fully contiguous memory. Nested ArrayList has extra indirection
    const table_size = comb.items.len;
    var graph = try ArrayList(ArrayList(bool)).initCapacity(alloc, table_size);

    // Set all entries to false
    for (0..table_size) |_| {
        var row = try ArrayList(bool).initCapacity(alloc, table_size);
        for (0..table_size) |_| {
            try row.append(false);
        }
        try graph.append(row);
    }

    // Set fields to true when there is an edge
    for (comb.items, 0..) |mod1, i| {
        for (comb.items, 0..) |mod2, j| {
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

    return graph;
}

fn count_graph_edges(graph: Graph) u32 {
    var count: u32 = 0;
    for (graph.items) |row| {
        for (row.items) |edge| {
            if (edge) count += 1;
        }
    }
    return count;
}

test "make S" {
    var g = ArrayList(ArrayList(bool)).init(std.testing.allocator);
    defer g.deinit();

    // G is: ABC
    //     A 000
    //     B 001
    //     C 000
    //Meaning one edge from B to C
    //Therefore S should be A,B as neither have any incoming edges

    try g.append(ArrayList(bool).init(std.testing.allocator));
    try g.items[0].append(false); try g.items[0].append(false); try g.items[0].append(false);
    try g.append(ArrayList(bool).init(std.testing.allocator));
    try g.items[1].append(false); try g.items[1].append(false); try g.items[1].append(true);
    try g.append(ArrayList(bool).init(std.testing.allocator));
    try g.items[2].append(false); try g.items[2].append(false); try g.items[2].append(false);
    defer g.items[0].deinit(); defer g.items[1].deinit(); defer g.items[2].deinit();

    var S = try make_S_array(g, std.testing.allocator);
    defer S.deinit();
    try std.testing.expectEqualSlices(module.CombID, &[_]module.CombID{0,1}, S.items);
}

fn make_S_array(graph: Graph, alloc: std.mem.Allocator) !ArrayList(module.CombID) {
    var S =  ArrayList(module.CombID).init(alloc);
    const table_size = graph.items.len;
    for (0..table_size) |col| {
        for (0..table_size) |row| {
            if (graph.items[row].items[col]) {
                break;
            }
        } else {
            try S.append(@intCast(col));
        }
    }

    return S;
}

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
    var graph = try make_graph(mod.comb, alloc);
    defer {
        for (graph.items) |row| {
            row.deinit();
        }
        graph.deinit();
    }

    const table_size = mod.comb.items.len;
    const self_edge = for (0..table_size) |i| {
        if (graph.items[i].items[i]) {
            break true;
        }
    } else blk: {
        break :blk false;
    };

    if (self_edge) {
        return error.LoopedDAGError;
    }

    // Search throw graph to build S (nodes where there are no inputs in the graph)
    var S = try make_S_array(graph, alloc);
    defer S.deinit();

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

    if (count_graph_edges(graph) > 0) {
        L.deinit();
        return error.LoopedDAGError;
    }

    assert(S.items.len == 0);
    assert(L.items.len == mod.comb.items.len);

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
    // --s1-->| comb |--s2-->
    var mod = try module.Module.init(0, std.testing.allocator);
    defer mod.deinit();

    const s1 = try mod.addSignal(1);
    const s2 = try mod.addSignal(1);
    const comb = try mod.addComb();

    try mod.addCombInput(comb, s1);
    try mod.addCombOutput(comb, s2);

    var dag = try build_DAG(mod, std.testing.allocator);
    defer dag.deinit();

    try std.testing.expectEqualSlices(module.CombID, &[_]module.CombID{comb}, dag.comb_order.items);
}

test "two item DAG" {
    // --s1-->| comb1 |--s2-->| comb2 |--s3-->
    var mod = try module.Module.init(0, std.testing.allocator);
    defer mod.deinit();

    const s1 = try mod.addSignal(1);
    const s2 = try mod.addSignal(1);
    const s3 = try mod.addSignal(1);
    const comb1 = try mod.addComb();
    const comb2 = try mod.addComb();

    try mod.addCombInput(comb1, s1);
    try mod.addCombOutput(comb1, s2);
    try mod.addCombInput(comb2, s2);
    try mod.addCombOutput(comb2, s3);

    var dag = try build_DAG(mod, std.testing.allocator);
    defer dag.deinit();

    try std.testing.expectEqualSlices(module.CombID, &[_]module.CombID{comb1, comb2}, dag.comb_order.items);
}

test "complex DAG" {
    //
    // --s1-->| comb4 |--s3-->| comb2 |--s5-->
    //  |---->|            |->|
    //  |  |->|            |------<------
    //  |  |------------|               |
    // --s2-->| comb3 |--s4-->| comb1 |--s6-->
    //                  -s1-->|
    var mod = try module.Module.init(0, std.testing.allocator);
    defer mod.deinit();

    const s1 = try mod.addSignal(1);
    const s2 = try mod.addSignal(1);
    const s3 = try mod.addSignal(1);
    const s4 = try mod.addSignal(1);
    const s5 = try mod.addSignal(1);
    const s6 = try mod.addSignal(1);

    const comb1 = try mod.addComb();
    const comb2 = try mod.addComb();
    const comb3 = try mod.addComb();
    const comb4 = try mod.addComb();

    //Comb4
    try mod.addCombInput(comb4, s1);
    try mod.addCombInput(comb4, s2);
    try mod.addCombInput(comb4, s4);
    try mod.addCombOutput(comb4, s3);
    //Comb3
    try mod.addCombInput(comb3, s2);
    try mod.addCombOutput(comb3, s4);
    //Comb2
    try mod.addCombInput(comb2, s3);
    try mod.addCombInput(comb2, s6);
    try mod.addCombOutput(comb2, s5);
    //Comb1
    try mod.addCombInput(comb1, s4);
    try mod.addCombInput(comb1, s1);
    try mod.addCombOutput(comb1, s6);

    var dag = try build_DAG(mod, std.testing.allocator);
    defer dag.deinit();

    try std.testing.expectEqualSlices(module.CombID, &[_]module.CombID{comb3, comb4, comb1, comb2}, dag.comb_order.items);
}

test "looped DAG error" {
    // --s1-->| comb1 |--s2-->
    //     |->|         |
    //     |-------------
    var mod = try module.Module.init(0, std.testing.allocator);
    defer mod.deinit();

    const s1 = try mod.addSignal(1);
    const s2 = try mod.addSignal(1);
    const comb1 = try mod.addComb();

    try mod.addCombInput(comb1, s1);
    try mod.addCombInput(comb1, s2);
    try mod.addCombOutput(comb1, s2);

    try std.testing.expectError(error.LoopedDAGError, build_DAG(mod, std.testing.allocator));
}