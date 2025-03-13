const std = @import("std");
const ArrayList = std.ArrayList;
const testing = std.testing;

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


fn ruleFromRuleString(str: []const u8) ?RuleEnum {
    inline for (@typeInfo(RuleEnum).@"enum".fields) |f| {
        if (std.mem.eql(u8, str, f.name)) return @enumFromInt(f.value);
    }

    return null;
}

fn constructParseTable(alloc: std.mem.Allocator) !void {
    var first_sets = std.AutoHashMap(RuleEnum, std.EnumSet(Token)).init(alloc);
    defer first_sets.deinit();

    //Initialise sets to empty
    for (0..RuleCount) |rule_index| {
        var set = std.EnumSet(Token).initEmpty();        

        // Add empty token if this is a legitimate alternative
        for (0..MaxAlternativeCount) |alternative_index| {
            if (Rules[rule_index][alternative_index] == null) break;
            if (Rules[rule_index][alternative_index].?.empty) {
                set.insert(.PR_EMPTY);
            }
        }

        try first_sets.put(@enumFromInt(rule_index), set);
    }

    var change = true;
    while (change) {
        change = false;
        rules: for (0..RuleCount) |rule_index| {
            const rule: RuleEnum = @enumFromInt(rule_index);
            for (0..MaxAlternativeCount) |alternative_index| {
                //If it's null then we've gone through all the alternatives
                if (Rules[rule_index][alternative_index] == null) continue :rules;
                const ra = Rules[rule_index][alternative_index].?;
                if (ra.empty) continue;

                var this_set = first_sets.get(rule) orelse unreachable;
                std.debug.assert(ra.term_count > 0);

                //First set includes the first set of all terms where prior terms can be empty
                terms: for (0..ra.term_count) |term_index| {
                    switch (ra.terms[term_index] orelse unreachable) {
                        .token => |t| {
                            std.debug.assert(t != .PR_EMPTY); //TODO this can be allowed if we fix other stuff. It simplifies RuleAlternative as the empty case is just a token
                            if (!this_set.contains(t)) {
                                this_set.insert(t);
                                try first_sets.put(rule, this_set);
                                change = true;
                            }
                             //If we've seen a token then it must be terminal and we can disregard following ones. TODO: this won't be true if we allow PR_EMPTY in the grammar
                            break :terms;
                        },
                        .rule => |r| {
                            const other_set = first_sets.get(r) orelse unreachable;
                            var it = other_set.iterator();
                            var local_change = false; //Test this locally to avoid unnecessary extra put operations
                            while (it.next()) |t| {
                                if (!this_set.contains(t)) {
                                    this_set.insert(t);
                                    change = true;
                                    local_change = true;
                                }
                            }
                            if (local_change) try first_sets.put(rule, this_set);

                            // If this term cannot be empty then we can disregard following terms.
                            if (!other_set.contains(.PR_EMPTY)) {
                                break :terms;
                            }
                        }
                    }
                }
            }
        }
    }



    var it = first_sets.iterator();
    while (it.next()) |entry| {
        std.debug.print("{s}: ", .{@tagName(entry.key_ptr.*)});
        var vi_it = entry.value_ptr.iterator();
        while (vi_it.next()) |token| {
            std.debug.print("{s} ", .{@tagName(token)});
        }
        std.debug.print("\n", .{});
    }
}

pub fn expand(stack: *ArrayList(RuleElement), next_token: Token) !bool {
    const top_rule = stack.pop();

    const alternatives = switch(top_rule) {
        .token => |t| return t == next_token,
        .rule => |r| Rules[@intFromEnum(r)]
    };

    std.debug.print("{}\n",.{alternatives.len});

    return true;
}

test "Expand: basic" {
    var stack = ArrayList(RuleElement).init(testing.allocator); defer stack.deinit();
    try stack.append(.{ .token = .KW_module });
    try testing.expect(try expand(&stack, .KW_module));
    try testing.expectEqual(0, stack.items.len);
}

pub fn parse(tokens: *TokenBuffer, alloc: std.mem.Allocator) !bool {
    var parse_stack = ArrayList(RuleElement).init(alloc);
    defer parse_stack.deinit();

    parse_stack.append(.{ .rule = .module });

    for (0..tokens.size) |index| {
        const t = tokens.tokens[index];
        const loc = tokens.locations[index];
        if (!expand(&parse_stack, t)) {
            std.log.err("Could not find viable rule to match token {s} at {}:{}\n", .{@tagName(t), loc.line, loc.column});
            return false;
        }
    }

    if (parse_stack.items.len > 0) {
        std.log.err("Input token stream ended unexpectedly.\n", .{});
        return false;
    }

    return true;
}

test "RuleEnum: Stuff gets loaded" {
    const rule_names = [_]RuleEnum {.module, .module_name, .control_ports, .control_port_body, .control_port_list, .control_port, .signal_ports,
    .signal_port_body, .signal_port_list, .signal_port, .direction, .width, .signal_name,
    .module_body, .module_body_list, .module_body_item, .declaration, .signal_name_list,
    .block, .block_type, .block_body, .statement_list, .statement,
    .if_statement, .assignment_statement, .expr, .expr_tail, .atom_follow,
    .atomic_expr, .unary_op, .binary_op, .instantiation, .signal_connection_body, .signal_connection_list,
    .signal_connection};

    try std.testing.expectEqual(rule_names.len, @typeInfo(RuleEnum).@"enum".fields.len);
    inline for (rule_names, 0..) |r,i| {
        try std.testing.expectEqual(r, @as(RuleEnum,@enumFromInt(@typeInfo(RuleEnum).@"enum".fields[i].value)));
    }
}

test "Parser Table" {
    try constructParseTable(std.testing.allocator);
}

//test "Debug: print rules" {
//    rules: for (0..RuleCount) |i| {
//        std.debug.print("{s}\n", .{@tagName(@as(RuleEnum, @enumFromInt(i)))});
//        for (0..MaxAlternativeCount) |j| {
//            terms: for (0..MaxTermCount) |k| {
//                if (Rules[i][j] == null) continue :rules;
//                if (Rules[i][j]) |r| {
//                    if (!r.empty and k >= Rules[i][j].?.term_count) break :terms;
//                    if (r.empty) {
//                        std.debug.print("   E", .{});
//                        break :terms;
//                    } else {
//                        std.debug.print("   {s} ", .{
//                            switch(Rules[i][j].?.terms[k].?) {
//                                .token => |t| @tagName(t),
//                                .rule => |t| @tagName(t),
//                            }
//                        });
//                    }
//                } 
//            }
//            std.debug.print("\n",.{});
//        }
//    }
//}