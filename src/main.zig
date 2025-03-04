const std = @import("std");

const module = @import("module.zig");
const simulate = @import("simulate.zig");


const Options = struct {
    run_task1: bool = false,
    run_task2: bool = false,
    iterations: u32 = 1,
    test_input: bool = false 
};

fn get_options(alloc: std.mem.Allocator) !Options {
    var args = try std.process.argsWithAllocator(alloc);
    defer args.deinit();
    var options: Options = .{};

    var count_option_on_next = false;
    var first_arg = true;

    while (args.next()) |arg| {
        if (first_arg) {
            first_arg = false;
            continue;
        } else if (count_option_on_next) {
            options.iterations = try std.fmt.parseInt(u32, arg, 10);
            count_option_on_next = false;
        } else if (std.mem.eql(u8, arg, "--t1")) {
            options.run_task1 = true;
        } else if (std.mem.eql(u8, arg, "--t2")) {
            options.run_task2 = true;
        } else if (std.mem.eql(u8, arg, "--test")) {
            options.test_input = true;
        } else if (std.mem.eql(u8, arg, "--number")) {
            count_option_on_next = true;
        } else {
            std.log.err("Unknown argument {s}", .{arg});
            return error.OptionError;
        }
    }

    return options;
}

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const options = try get_options(gpa.allocator());
    _ = options;

    var input = std.ArrayList([]const u8).init(gpa.allocator());
    defer input.deinit();

    try stdout.print("Test stuff builds", .{});
}