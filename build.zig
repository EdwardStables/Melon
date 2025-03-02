const std = @import("std");

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});
    const exe = b.addExecutable(.{
        .name = "lsim",
        .root_source_file = b.path("src/main.zig"),
        .target = b.graph.host,
        .optimize = optimize,
    });

    b.installArtifact(exe);
}
