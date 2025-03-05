const std = @import("std");

const module = @import("module.zig");
const simulate = @import("simulate.zig");
const cdag = @import("comb_dag.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var test_module = try module.Module.init(0, gpa.allocator());
    defer test_module.deinit();

    var dag = try cdag.build_DAG(test_module, gpa.allocator());
    defer dag.deinit();

    const result = try simulate.run(&test_module, dag, 10);

    if (result == simulate.SimulationResult.FinishedSuccess) {
        std.log.info("Simulation Completed Successfully", .{});
    }
}

test {
    std.testing.refAllDecls(@This());
}