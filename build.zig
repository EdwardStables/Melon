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
    const Rule = std.ArrayList([]const u8);
    const Rules = std.ArrayList(Rule); 
    var rules = Rules.init(b.allocator);
    defer {
        for (rules.items) |rule| {
            for (rule.items) |str| b.allocator.free(str);
            rule.deinit();
        }
        rules.deinit();
    }

    while (
        in_stream.readUntilDelimiterOrEof(&buf, ';') catch
            std.debug.panic("Error while reading grammar file contents", .{})
    ) |line| {
        if (line.len == 0) continue;

        //Split into name and rules
        var rule_it = std.mem.tokenizeAny(u8, line, "=");

        //Use iterator to isolate rule name
        const rule_name_full_slice = rule_it.next() orelse unreachable;
        var rule_name_full_slice_it = std.mem.tokenizeAny(u8, rule_name_full_slice, "\n ");
        const rule_name_slice = rule_name_full_slice_it.next() orelse unreachable;
        std.debug.assert(rule_name_full_slice_it.next() == null);
        const rule_name = b.allocator.alloc(u8, rule_name_slice.len) catch
            std.debug.panic("Error while creating buffer for rule name", .{});
        std.mem.copyForwards(u8, rule_name, rule_name_slice);


        //Get the right hand side from the other part of the iterator
        const rule_rhs = rule_it.next() orelse unreachable;
        std.debug.assert(rule_it.next() == null);

        var alternative_it = std.mem.tokenizeAny(u8, rule_rhs, "|");

        while (alternative_it.next()) |alt| {
            //Rule starts with the rule name
            var new_rule = std.ArrayList([]const u8).init(b.allocator);
            new_rule.append(rule_name) catch
                std.debug.panic("Error while appending rule name to rule alternative", .{});

            //Tokens split by space usually, but newline is technically ok
            var token_it = std.mem.tokenizeAny(u8, alt, " \n");
            while (token_it.next()) |token| {
                const token_copy = b.allocator.alloc(u8, token.len) catch
                    std.debug.panic("Error while creating buffer for rule token", .{});
                std.mem.copyForwards(u8, token_copy, token);
                new_rule.append(token_copy) catch
                    std.debug.panic("Error while token to rule alternative", .{});
            }

            rules.append(new_rule) catch
                std.debug.panic("Error while appending grammar rule to rule list", .{});
        }
    }

    // Options can only be slices
    const ruleslice: [][]const []const u8 = b.allocator.alloc([]const []const u8, rules.items.len) catch
        std.debug.panic("Error while allocating rule array", .{});
    for (rules.items, 0..) |rule, i| {
        ruleslice[i] = rule.items;
    }
    const cruleslice: [][]const []const u8 = ruleslice;

    // Add the file names as an option to the exe, making it available
    // as a string array at comptime
    options.addOption([]const []const []const u8, "rules", cruleslice);
    exe.root_module.addOptions("config", options);
    grammar_tests.root_module.addOptions("config", options);
}
