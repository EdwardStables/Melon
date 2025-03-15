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

const StrRules = @import("config").rules;
const RuleCount = StrRules.len;
const MaxTermCount = blk: {
    var max: u8 = 0;
    for (StrRules) |rule| {
        max = @max(rule.len-1, max);
    }

    break :blk max;
};

const MaxAlternativeCount = blk: {
    var current: ?[]const u8 = null;
    var current_count: u16 = 0;
    var max: u16 = 0;
    for (StrRules) |rule| {
        if (current == null or !std.mem.eql(u8, current.?, rule[0])) {
            if (current != null) {
                max = @max(max, current_count);
            }
            current = rule[0];
            current_count = 0;
        }

        current_count += 1;
    }

    break :blk max;
};

const NonTerminalCount: u16 = blk: {
    var count = 0;
    var current: ?[]const u8 = null;
    for (StrRules) |rule| {
        if (current == null) {
            current = rule[0];
            count += 1;
        } else
        if (!std.mem.eql(u8, current.?, rule[0])) {
            current = rule[0];
            count += 1;
        }
    }

    break :blk count;
};

/// Each alternative is given a unique enum to identify it. However this means
/// that rule references on the RHS cannot directly identify all the relevant rules.
/// Therefore use this table to identify the possible enums. This assumes that all rules with the same LHS are adjacent
const AlternativeTableType = [NonTerminalCount][MaxAlternativeCount]?u16;
const AlternativeTable: AlternativeTableType = blk: {
    var alternatives: AlternativeTableType  = undefined;
    for (0..NonTerminalCount) |r| {
        for(0..MaxAlternativeCount) |a| {
            alternatives[r][a] = null;
        }
    }

    var current: ?[]const u8 = null;
    var current_index: u16 = 0;
    var current_alt_index: u16 = 0;
    for (StrRules, 0..) |rule, rule_index| {
        if (current == null) {
            current = rule[0];

        } else
        if (!std.mem.eql(u8, current.?, rule[0])) {
            current = rule[0];
            current_index += 1;
            current_alt_index = 0;
        }

        alternatives[current_index][current_alt_index] = rule_index;
        current_alt_index += 1;
    }

    break :blk alternatives;
};


/// Enum of all non-terminal symbols in the grammar, derived from
/// the LHS of the rule declarations.
pub const RuleEnum = blk: {
    var fields: [RuleCount]std.builtin.Type.EnumField = undefined;

    //Iterate to get unique LHS rule names
    var current: ?[]const u8 = null;
    var index: u16 = 0;
    for (StrRules) |rule| {
        if (current == null or !std.mem.eql(u8, current.?, rule[0])) {
            var s: [rule[0].len:0]u8 = undefined;
            s[s.len] = 0;
            @memcpy(&s, rule[0].ptr);
            fields[index] = std.builtin.Type.EnumField{.name=&s, .value = index};
            current = rule[0];
            index += 1;
        }
    }

    const enumInfo = std.builtin.Type.Enum{
        .tag_type = u8,
        .fields = fields[0..index],
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
const Rule = struct {
    terms: [MaxTermCount]?RuleElement,
    term_count: u8,
    fn make() Rule {
        return .{.terms=.{null,null,null,null,null,null}, .term_count=0};
    }
    fn add(self: *Rule, elem: RuleElement) void {
        if (elem == .token and elem.token == .PR_EMPTY) std.debug.assert(self.term_count == 0); //Can only have a single term if empty
        self.terms[self.term_count] = elem;
    }
};

const RuleType = [RuleCount]Rule;
const Rules: RuleType = blk: {
    var rules: RuleType = undefined;
    for (0..RuleCount) |i| {
        rules[i] = Rule.make();
    }

    for (StrRules, 0..) |rule, i| {
        std.debug.assert(rule.len > 1);
        for (rule[1..]) |r| {
            if (r.len == 1 and r[0] == 'E') {
                rules[i].add(RuleElement{.token = .PR_EMPTY});
            } else {
                @setEvalBranchQuota(16384); //Large amount of branching from tokenize, so raise the limit here
                if (tokeniser.tokenFromTokenString(r)) |t|
                    rules[i].add(RuleElement{.token = t})
                else if (ruleFromRuleString(r)) |nt|
                    rules[i].add(RuleElement{.rule = nt})
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

const RuleTokenSet = std.AutoHashMap(RuleEnum, std.EnumSet(Token));

fn printRuleTokenSet(sets: *RuleTokenSet) void {
    var it = sets.iterator();
    while (it.next()) |entry| {
        std.debug.print("{s}: ", .{@tagName(entry.key_ptr.*)});
        var vi_it = entry.value_ptr.iterator();
        while (vi_it.next()) |token| {
            std.debug.print("{s} ", .{@tagName(token)});
        }
        std.debug.print("\n", .{});
    }
}

fn constructFirstSets(alloc: std.mem.Allocator) !RuleTokenSet {
    var first_sets = RuleTokenSet.init(alloc);

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

    return first_sets;
}

fn constructFollowSets(first_sets: *RuleTokenSet, alloc: std.mem.Allocator) !RuleTokenSet {
    var follow_sets = RuleTokenSet.init(alloc);

    //Initialise sets to empty
    for (0..RuleCount) |rule_index| {
        var set = std.EnumSet(Token).initEmpty();
        const rule: RuleEnum = @enumFromInt(rule_index);

        if (rule == .module) { //module is the start symbol and therefore has the follow token of PR_END
            set.insert(.PR_END);
        }

        try follow_sets.put(rule, set);
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

                std.debug.assert(ra.term_count > 0);

                // For pattern B->wAw' if terminal a is in First(w') then add it to Follow(A)
                // For pattern B->wAw' if E is in First(w') or w' has length 0 then add Follow(B) to Follow(A)
                var active_set = follow_sets.get(rule) orelse unreachable; //Init to B's follow set
                var term_index = ra.term_count;
                while (term_index > 0) {
                    term_index -= 1;
                    switch(ra.terms[term_index] orelse unreachable) {
                        //If we see a token then the active set must be cleared and just this token added
                        .token => |t| {
                            active_set = @TypeOf(active_set).initEmpty();
                            active_set.insert(t);
                        },
                        // First update the follow set of r with the active set
                        // Then update the active set:
                        //     - If E is in the rule's first set then the existing active set can be kept with new first set entries
                        //     - If E is not in the rule's first set then we make the active set equal to the rule's first set
                        .rule => |r| {
                            var rule_follow_set = follow_sets.get(r) orelse unreachable;
                            var rule_first_set = first_sets.get(r) orelse unreachable;
                            var updated_rule_follow_set = rule_follow_set.unionWith(active_set);

                            // Update follow
                            if (!updated_rule_follow_set.eql(rule_follow_set)) {
                                try follow_sets.put(r, updated_rule_follow_set);
                                change = true;
                            }

                            if (rule_first_set.contains(.PR_EMPTY)) { // Containing empty means we are transparent and continue using the active set
                                // Add this rule's first set, but excluding the empty token
                                active_set = active_set.unionWith(rule_first_set.differenceWith(std.EnumSet(Token).initOne(.PR_EMPTY)));
                            } else { // Not containing empty resets the active set
                                active_set = rule_first_set;
                            }
                        }
                    }
                }
            }
        }
    }


    return follow_sets;
}

const TokenCount = @typeInfo(Token).@"enum".fields.len;
const RuleIndex = struct{rule: RuleEnum, alternative: u8,};
const ParseTable = [RuleCount][TokenCount]?RuleIndex;


fn constructParseTable(first_sets: *RuleTokenSet, follow_sets: *RuleTokenSet, alloc: std.mem.Allocator) !*ParseTable{
    var table: *ParseTable = try alloc.create(ParseTable);
    for (0..RuleCount) |rule_index| {
        for (0..TokenCount) |token_index| {
            const rule: RuleEnum = @enumFromInt(rule_index);
            const token: Token = @enumFromInt(token_index);
            const follow = follow_sets.get(rule) orelse unreachable;

            table[rule_index][token_index] = null;

            for (0..RuleCount) |next_rule_index| {
                const next_rule: RuleEnum = @enumFromInt(next_rule_index);
                for (0..MaxAlternativeCount) |next_alternative_index| {
                    const alt = Rules[next_rule_index][next_alternative_index];
                    if (alt == null) break;
                    if (alt.?.empty) continue;

                    const first = switch (alt.?.terms[0] orelse unreachable) {
                        .token => |t| std.EnumSet(Token).initOne(t),
                        .rule => |r| first_sets.get(r) orelse unreachable,
                    };

                    if (first.contains(token) or (first.contains(.PR_EMPTY) and follow.contains(token))) {
                        //This should be an LL(1) grammar, if there has already been an entry then there is a bug in the parser or the grammar itself
                        std.debug.print("{} {} {} ", .{rule, token, next_rule});
                        std.debug.print("{} {}\n", .{first.contains(token), (first.contains(.PR_EMPTY) and follow.contains(token))});
                        std.debug.assert(table[rule_index][token_index] == null);
                        table[rule_index][token_index] = .{.rule=next_rule, .alternative=@intCast(next_alternative_index)};
                    }
                }
            }
        }
    }

    return table;
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

test "Alternatives" {
    try std.testing.expectEqual(68, RuleCount);
    try std.testing.expectEqual(35, NonTerminalCount);
    try std.testing.expectEqual(9, MaxAlternativeCount);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{0, null,null,null,null,null,null,null,null}, &AlternativeTable[@intFromEnum(RuleEnum.module)]);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{1, null,null,null,null,null,null,null,null}, &AlternativeTable[@intFromEnum(RuleEnum.module_name)]);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{2,    3,null,null,null,null,null,null,null}, &AlternativeTable[@intFromEnum(RuleEnum.control_ports)]);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{4, null,null,null,null,null,null,null,null}, &AlternativeTable[@intFromEnum(RuleEnum.control_port_body)]);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{5,    6,null,null,null,null,null,null,null}, &AlternativeTable[@intFromEnum(RuleEnum.control_port_list)]);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{7,    8,null,null,null,null,null,null,null}, &AlternativeTable[@intFromEnum(RuleEnum.control_port)]);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{9, null,null,null,null,null,null,null,null}, &AlternativeTable[@intFromEnum(RuleEnum.signal_ports)]);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{10,  11,null,null,null,null,null,null,null}, &AlternativeTable[@intFromEnum(RuleEnum.signal_port_body)]);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{12,  13,null,null,null,null,null,null,null}, &AlternativeTable[@intFromEnum(RuleEnum.signal_port_list)]);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{14,null,null,null,null,null,null,null,null}, &AlternativeTable[@intFromEnum(RuleEnum.signal_port)]);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{15,  16,null,null,null,null,null,null,null}, &AlternativeTable[@intFromEnum(RuleEnum.direction)]);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{17,  18,null,null,null,null,null,null,null}, &AlternativeTable[@intFromEnum(RuleEnum.width)]);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{19,null,null,null,null,null,null,null,null}, &AlternativeTable[@intFromEnum(RuleEnum.signal_name)]);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{20,null,null,null,null,null,null,null,null}, &AlternativeTable[@intFromEnum(RuleEnum.module_body)]);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{21,  22,null,null,null,null,null,null,null}, &AlternativeTable[@intFromEnum(RuleEnum.module_body_list)]);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{23,  24,  25,null,null,null,null,null,null}, &AlternativeTable[@intFromEnum(RuleEnum.module_body_item)]);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{26,null,null,null,null,null,null,null,null}, &AlternativeTable[@intFromEnum(RuleEnum.declaration)]);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{27,  28,null,null,null,null,null,null,null}, &AlternativeTable[@intFromEnum(RuleEnum.signal_name_list)]);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{29,null,null,null,null,null,null,null,null}, &AlternativeTable[@intFromEnum(RuleEnum.block)]);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{30,  31,null,null,null,null,null,null,null}, &AlternativeTable[@intFromEnum(RuleEnum.block_type)]);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{32,  33,null,null,null,null,null,null,null}, &AlternativeTable[@intFromEnum(RuleEnum.block_body)]);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{34,  35,null,null,null,null,null,null,null}, &AlternativeTable[@intFromEnum(RuleEnum.statement_list)]);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{36,  37,null,null,null,null,null,null,null}, &AlternativeTable[@intFromEnum(RuleEnum.statement)]);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{38,null,null,null,null,null,null,null,null}, &AlternativeTable[@intFromEnum(RuleEnum.if_statement)]);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{39,null,null,null,null,null,null,null,null}, &AlternativeTable[@intFromEnum(RuleEnum.assignment_statement)]);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{40,  41,null,null,null,null,null,null,null}, &AlternativeTable[@intFromEnum(RuleEnum.expr)]);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{42,  43,null,null,null,null,null,null,null}, &AlternativeTable[@intFromEnum(RuleEnum.expr_tail)]);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{44,  45,null,null,null,null,null,null,null}, &AlternativeTable[@intFromEnum(RuleEnum.atom_follow)]);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{46,  47,  48,null,null,null,null,null,null}, &AlternativeTable[@intFromEnum(RuleEnum.atomic_expr)]);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{49,  50,  51,  52,null,null,null,null,null}, &AlternativeTable[@intFromEnum(RuleEnum.unary_op)]);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{53,  54,  55,  56,  57,  58,  59,  60,  61}, &AlternativeTable[@intFromEnum(RuleEnum.binary_op)]);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{62,null,null,null,null,null,null,null,null}, &AlternativeTable[@intFromEnum(RuleEnum.instantiation)]);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{63,  64,null,null,null,null,null,null,null}, &AlternativeTable[@intFromEnum(RuleEnum.signal_connection_body)]);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{65,  66,null,null,null,null,null,null,null}, &AlternativeTable[@intFromEnum(RuleEnum.signal_connection_list)]);
    try std.testing.expectEqualSlices(?u16, &[_]?u16{67,null,null,null,null,null,null,null,null}, &AlternativeTable[@intFromEnum(RuleEnum.signal_connection)]);
}

test "Gen Parse Table" {
    var first_sets = try constructFirstSets(std.testing.allocator);
    defer first_sets.deinit();


//    var follow_sets = try constructFollowSets(&first_sets, std.testing.allocator);
//    defer follow_sets.deinit();
//
//    printRuleTokenSet(&first_sets);
//    std.debug.print("\n", .{});
//    printRuleTokenSet(&follow_sets);
//
//    const table = try constructParseTable(&first_sets, &follow_sets, std.testing.allocator);
//    defer std.testing.allocator.destroy(table);
//
//    for (0..RuleCount) |rule_index| {
//        for (0..TokenCount) |token_index| {
//            const rl = table[rule_index][token_index];
//
//            if (rl == null) {
//                std.debug.print("---", .{});
//            } else {
//                std.debug.print("{d: >3}.{d}", .{@intFromEnum(rl.?.rule), rl.?.alternative});
//            }
//        }
//        std.debug.print("\n", .{});
//    }
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