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

    //*** Grammar specific testing ***//
    const grammar_step = b.step("grammar", "Parse and test grammar only");
    const grammar_tests = b.addTest(.{ .root_source_file = b.path("src/grammar.zig") });
    const run_grammar_tests = b.addRunArtifact(grammar_tests);
    grammar_step.dependOn(&run_grammar_tests.step);

    //*** Load and initial processing of grammar file ***//
    var options = b.addOptions();

    var grammar_file = std.fs.cwd().openFile("grammar", .{}) catch 
        std.debug.panic("Grammar file could not be opened", .{});
    defer grammar_file.close();

    var buf_reader = std.io.bufferedReader(grammar_file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;
    var rules = std.ArrayList([]const u8).init(b.allocator);
    defer {
        for (rules.items) |i| b.allocator.free(i);
        rules.deinit();
    }

    while (
        in_stream.readUntilDelimiterOrEof(&buf, ';') catch
            std.debug.panic("Error while reading grammar file contents", .{})
    ) |line| {
        if (line.len == 0) continue;
        const copied_line = b.allocator.alloc(u8, line.len+1) catch 
            std.debug.panic("Error while creating buffer for rule line", .{});
        std.mem.copyForwards(u8, copied_line, line);
        rules.append(copied_line) catch 
            std.debug.panic("Error while appending grammar rule to rule list", .{});
    }

    // Add the file names as an option to the exe, making it available
    // as a string array at comptime
    options.addOption([]const []const u8, "rules", rules.items);
    exe.root_module.addOptions("config", options);
    grammar_tests.root_module.addOptions("config", options);
}
