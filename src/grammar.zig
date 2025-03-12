const std = @import("std");
const rules = @import("config").rules;

pub const RuleEnum = blk: {
    //Iterate through to get rule count
    var rule_count = 0;
    for (rules) |rule| {
        //@compileLog("got rule", rule);
        @setEvalBranchQuota(16384); //Large amount of branching from tokenize, so raise the limit here
        var it = std.mem.tokenizeAny(u8, rule, " \n");
        while (it.next()) |v| {
            if (v.len == 0) continue;
            //@compileLog("got token ", v);
        }
        rule_count += 1;
    }

    const numFields = rule_count;
    var fields: [numFields]std.builtin.Type.EnumField = undefined;

    var index = 0;
    //Iterate again to get rule names
    for (rules) |rule| {
        var it = std.mem.splitAny(u8, rule, " \n");
        while (it.next()) |v| {
            if (v.len == 0) continue;
            var s: [v.len:0]u8 = undefined;
            s[s.len] = 0;
            @memcpy(&s, v.ptr);
            fields[index] = std.builtin.Type.EnumField{.name=&s, .value = index};
            break;
        }
        index += 1;
    }

    const enumInfo = std.builtin.Type.Enum{
        .tag_type = u8,
        .fields = &fields,
        .decls = &[0]std.builtin.Type.Declaration{},
        .is_exhaustive = true,
    };

    break :blk @Type(std.builtin.Type{ .@"enum" = enumInfo });
};

test "RuleEnum: Stuff gets loaded" {
    std.debug.print("{s}", .{@tagName(RuleEnum.module)});
}