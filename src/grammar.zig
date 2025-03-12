const std = @import("std");

const tokeniser = @import("tokeniser.zig");
const Token = tokeniser.Token;
const TokenBuffer = tokeniser.TokenBuffer;

const SpecialCase = enum {
    Empty,
    Null
};

const RuleCount = blk: {
    //Iterate through to get rule count
    var rule_count = 0;
    for (@import("config").rules) |rule| {
        @setEvalBranchQuota(16384); //Large amount of branching from tokenize, so raise the limit here
        var it = std.mem.tokenizeAny(u8, rule, " \n");
        while (it.next()) |v| {
            if (v.len == 0) continue;
            //@compileLog("got token ", v);
        }
        rule_count += 1;
    }

    break :blk rule_count;
};
const MaxTermCount = 6; //TODO this can be picked up automatically
const MaxAlternativeCount = blk: {
    var m = 0;
    @setEvalBranchQuota(16384); //Large amount of branching from tokenize, so raise the limit here
    for (@import("config").rules) |rule| {
        const c = std.mem.count(u8, rule, "|");
        m = if (c > m) c else m;
    }

    break :blk m+1;
};

pub const RuleEnum = blk: {
    var fields: [RuleCount]std.builtin.Type.EnumField = undefined;

    var index = 0;
    //Iterate again to get rule names
    for (@import("config").rules) |rule| {
        var it = std.mem.splitAny(u8, rule, " \n");
        @setEvalBranchQuota(16384); //Large amount of branching from tokenize, so raise the limit here
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

const ElementType = enum {
    token, rule
};

const RuleElement = union(ElementType) {
    token: Token,
    rule: RuleEnum,
};

const RuleAlternative = struct {
    empty: bool,
    terms: [MaxTermCount]?RuleElement,
    term_count: u8,
    fn make(empty: bool) RuleAlternative {
        if (empty) {
            return .{.empty=true, .terms=.{null,null,null,null,null,null}, .term_count=0};
        } else {
            return .{.empty=false, .terms=.{null,null,null,null,null,null}, .term_count=0};
        }
    }

    fn append(self: *RuleAlternative, e: RuleElement) void {
        self.terms[self.term_count] = e;
        self.term_count += 1;
    }
};

const Rules = blk: {
    var rules: [RuleCount][MaxAlternativeCount]?RuleAlternative = undefined;
    for (0..RuleCount) |i| {
        for (0..MaxAlternativeCount) |j| {
            rules[i][j] = null;
        }
    }

    for (@import("config").rules, 0..) |rule, i| {
        @setEvalBranchQuota(24000); //Large amount of branching from tokenize, so raise the limit here
        var it = std.mem.tokenizeAny(u8, rule, " \n:=");
        var j = 0;
        var k = 0;
        var first = true;
        while (it.next()) |v| {
            if (v.len == 1 and v[0] == '|') {
                j += 1;
                k = 0;
                continue;
            }
            if (v.len == 0) continue;
            if (first) {
                first = false;
                continue;
            }
            if (v.len == 1 and v[0] == 'E') {
                rules[i][j] = RuleAlternative.make(true);
            } else {
                if (rules[i][j] == null) {
                    std.debug.assert(k == 0);
                    rules[i][j] = RuleAlternative.make(false);
                }
                if (tokeniser.tokenFromTokenString(v)) |t|
                    rules[i][j].?.append(RuleElement{.token = t})
                else if (ruleFromRuleString(v)) |r|
                    rules[i][j].?.append(RuleElement{.rule = r})
                else unreachable;
            }
        }

    }

    break :blk rules;
};


pub fn ruleFromRuleString(str: []const u8) ?RuleEnum {
    inline for (@typeInfo(RuleEnum).@"enum".fields) |f| {
        if (std.mem.eql(u8, str, f.name)) return @enumFromInt(f.value);
    }

    return null;
}

//fn Rule(name: RuleEnum) type {
    //inline for (rules) |rule| {
    //    @setEvalBranchQuota(16384); //Large amount of branching from tokenize, so raise the limit here
    //    var it = std.mem.tokenizeAny(u8, rule, " \n");
    //    while (it.next()) |v| {
    //        if (v.len == 0) continue;
    //    }
    //}
//}

test "RuleEnum: Stuff gets loaded" {
    const rule_names = [_]RuleEnum {.module, .module_name, .control_ports, .control_port, .signal_ports,
    .signal_port_list, .signal_port, .direction, .width, .signal_name,
    .module_body, .module_body_list, .module_body_item, .declaration, .signal_name_list,
    .block, .block_type, .block_body, .statement_list, .statement,
    .if_statement, .assignment_statement, .expr, .expr_tail, .atom_follow,
    .atomic_expr, .unary_op, .binary_op, .instantiation, .signal_connection_list,
    .signal_connection};

    try std.testing.expectEqual(rule_names.len, @typeInfo(RuleEnum).@"enum".fields.len);
    inline for (rule_names, 0..) |r,i| {
        try std.testing.expectEqual(r, @as(RuleEnum,@enumFromInt(@typeInfo(RuleEnum).@"enum".fields[i].value)));
    }
}

test "asderg" {
    for (0..RuleCount) |i| {
        std.debug.print("{s}\n", .{@tagName(@as(RuleEnum, @enumFromInt(i)))});
        for (0..MaxAlternativeCount) |j| {
            for (0..MaxTermCount) |k| {
                if (Rules[i][j] != null and k < Rules[i][j].?.term_count)
                    std.debug.print("   {s} ", .{
                        switch(Rules[i][j].?.terms[k].?) {
                            .token => |t| @tagName(t),
                            .rule => |r| @tagName(r),
                        }
                    });
            }
        }
        std.debug.print("\n",.{});
    }
}