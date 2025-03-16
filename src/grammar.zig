const std = @import("std");
const ArrayList = std.ArrayList;
const testing = std.testing;

const tokeniser = @import("tokeniser.zig");
const Token = tokeniser.Token;
const TokenBuffer = tokeniser.TokenBuffer;

const builtin = @import("builtin");
const log = if (builtin.is_test)
    struct {
        const base = std.log.scoped(.parser);
        const err = warn;
        const warn = base.warn;
        const info = base.info;
        const debug = base.debug;
    }
else
    log.scoped(.parser);

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
        self.term_count += 1;
    }
    fn empty(self: Rule) bool {
        return switch (self.terms[0].?) {
            .rule => false,
            .token => |t| t==.PR_EMPTY
        };
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
    for (0..NonTerminalCount) |non_terminal_index| {
        var set = std.EnumSet(Token).initEmpty();        
        for (AlternativeTable[non_terminal_index]) |alt_op| {
            const alt = alt_op orelse break;
            if (Rules[alt].empty()) set.insert(.PR_EMPTY);
        }
        try first_sets.put(@enumFromInt(non_terminal_index), set);
    }

    var change = true;
    while (change) {
        change = false;
        for (0..NonTerminalCount) |non_terminal_index| {
            const rule: RuleEnum = @enumFromInt(non_terminal_index);
            var this_set = first_sets.get(rule) orelse unreachable;

            for (AlternativeTable[non_terminal_index]) |alt_op| {
                const alt = alt_op orelse break;
                const ra = Rules[alt];
                std.debug.assert(ra.term_count > 0);
                if (ra.empty()) std.debug.assert(ra.term_count == 1);

                //First set includes the first set of all terms where prior terms can be empty
                terms: for (0..ra.term_count) |term_index| {
                    switch (ra.terms[term_index] orelse unreachable) {
                        .token => |t| {
                            if (!this_set.contains(t)) {
                                this_set.insert(t);
                                try first_sets.put(rule, this_set);
                                change = true;
                            }
                             //If we've seen a token then it must be terminal and we can disregard following ones.
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

    for (0..NonTerminalCount) |non_terminal_index| {
        var set = std.EnumSet(Token).initEmpty();        
        const rule: RuleEnum = @enumFromInt(non_terminal_index);

        if (rule == .module) { //module is the start symbol and therefore has the follow token of PR_END
            set.insert(.PR_END);
        }
        try follow_sets.put(rule, set);
    }

    var change = true;
    while (change) {
        change = false;
        for (0..NonTerminalCount) |non_terminal_index| {
            const rule: RuleEnum = @enumFromInt(non_terminal_index);
            for (AlternativeTable[non_terminal_index]) |alt_op| {
                const alt = alt_op orelse break;
                const ra = Rules[alt];

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
const ParseTable = [NonTerminalCount][TokenCount]?u16;

fn constructParseTable(first_sets: *RuleTokenSet, follow_sets: *RuleTokenSet, alloc: std.mem.Allocator) !*ParseTable{
    var table: *ParseTable = try alloc.create(ParseTable);
    for (0..NonTerminalCount) |non_terminal_index| {
        const rule: RuleEnum = @enumFromInt(non_terminal_index);
        const follow = follow_sets.get(rule) orelse unreachable;

        for (0..TokenCount) |token_index| {
            const token: Token = @enumFromInt(token_index);

            table[non_terminal_index][token_index] = null;

            for (AlternativeTable[non_terminal_index]) |alt_op| {
                const alternative = alt_op orelse break;

                const first = switch (Rules[alternative].terms[0] orelse unreachable) {
                    .token => |t| std.EnumSet(Token).initOne(t),
                    .rule => |r| first_sets.get(r) orelse unreachable,
                };

                const first_has = first.contains(token);
                const follow_has = first.contains(.PR_EMPTY) and follow.contains(token); 
                if (first_has or follow_has) {
                    log.debug("Parse Table: Entry set for rule {s} token {s}. First set present: {}. Follow set present: {}", .{@tagName(rule),@tagName(token),first_has,follow_has});
                    //This should be an LL(1) grammar, if there has already been an entry then there is a bug in the parser or the grammar itself
                    if (table[non_terminal_index][token_index] != null) {
                        log.err("Grammar is ambiguous, for rule {s} token {s} can lead to multiple productions.", .{@tagName(rule),@tagName(token)});
                        unreachable;
                    }
                    table[non_terminal_index][token_index] = alternative;
                }
            }
        }
    }

    return table;
}

fn parse(table: *ParseTable, tokens: TokenBuffer, start_symbol: RuleElement, alloc: std.mem.Allocator) !?u32 {
    var stack = std.ArrayList(RuleElement).init(alloc);
    defer stack.deinit();
    try stack.append(.{ .token = .PR_END });
    try stack.append(start_symbol);

    var index: u32 = 0;
    while (index < tokens.size or stack.items.len > 1) {
        const next_input_token = if (index < tokens.size) tokens.tokens[index] else .PR_EMPTY;
        const next_stack_element = stack.popOrNull() orelse unreachable;
        switch (next_stack_element) {
            .rule => |r2| log.debug("Popped {s}", .{@tagName(r2)}),
            .token => |t2| log.debug("Popped {s}", .{@tagName(t2)}),
        }

        switch (next_stack_element) {
            .token => |t| if (t == .PR_END) {
                log.err("Reached end of parse stack but still see input tokens. Next observed token is {s}, index {}", .{@tagName(next_input_token),index});
                return index;
            },
            else => {}
        }

        switch (next_stack_element) {
            .token => |t| {
                if (t == next_input_token) {
                    //Matching
                    index += 1;
                    log.debug("Matched token {s}", .{@tagName(t)});
                    continue;
                } else {
                    //mismatching
                    log.debug("Saw unexpected token {s} at index {}. Expected token {s}.", .{@tagName(next_input_token),index,@tagName(t)});
                    return index;
                }
            },
            .rule => |r| {
                // No rule matches input token
                const next = table[@intFromEnum(r)][@intFromEnum(next_input_token)] orelse {
                    log.err("Input token {s} at index {} does not have a matching alternative. Stack top is rule {s}", .{@tagName(next_input_token),index,@tagName(r)});
                    return index;
                };
                const rule = Rules[next];
                var term_index = rule.term_count;
                while (term_index > 0) {
                    term_index -= 1;
                    const next_element = rule.terms[term_index] orelse undefined;
                    switch (next_element) {
                        .token => |t2| if (t2 == .PR_EMPTY) continue,
                        else => {},
                    }
                    switch (next_element) {
                        .rule => |r2| log.debug("For {s} push {s}", .{@tagName(r),@tagName(r2)}),
                        .token => |t2| log.debug("For {s} push {s}", .{@tagName(r),@tagName(t2)}),
                    }
                    try stack.append(next_element);
                }
            }
        }
    }

    if (stack.items.len > 1) {
        log.err("Input tokens all consumed but parse stack has {} remaning symbols:", .{stack.items.len-1});
        var stack_index = stack.items.len;
        while (stack_index > 0) {
            stack_index -= 1;
            switch (stack.items[stack_index]) {
                .rule => |r| log.info("{s}", .{@tagName(r)}),
                .token => |t| log.info("{s}", .{@tagName(t)}),
            }
        }
        return index;
    }

    //Success
    return null;
}

test "Gen Parse Table" {
    var first_sets = try constructFirstSets(std.testing.allocator);
    defer first_sets.deinit();

    var follow_sets = try constructFollowSets(&first_sets, std.testing.allocator);
    defer follow_sets.deinit();

    const table = try constructParseTable(&first_sets, &follow_sets, std.testing.allocator);
    defer std.testing.allocator.destroy(table);
}


fn tableForTesting() !*ParseTable {
    var first_sets = try constructFirstSets(testing.allocator); defer first_sets.deinit();
    var follow_sets = try constructFollowSets(&first_sets, testing.allocator); defer follow_sets.deinit();
    const table = try constructParseTable(&first_sets, &follow_sets, testing.allocator);

    return table;
}

fn runParseTest(inp: []const u8, start_symbol: RuleElement) !?u32 {
    const table = try tableForTesting(); defer testing.allocator.destroy(table);
    var tokens = try TokenBuffer.init(1024, testing.allocator); defer tokens.deinit();
    try testing.expectEqual(inp.len, try tokeniser.tokenise(inp, &tokens));
    return try parse(table, tokens, start_symbol, testing.allocator);
}

test "Test Good Expr Parses" {
    try testing.expectEqual(null, try runParseTest( "x+s", .{ .rule = .expr }));
    try testing.expectEqual(null, try runParseTest( "x+y+z", .{ .rule = .expr }));
    try testing.expectEqual(null, try runParseTest( "x & 13'b0", .{ .rule = .expr }));
    try testing.expectEqual(null, try runParseTest( "13'b0 << var", .{ .rule = .expr }));
    try testing.expectEqual(null, try runParseTest( "13'b0 <= var", .{ .rule = .expr }));
    try testing.expectEqual(null, try runParseTest( "13'b0 <= var ++ var2 + var3 ", .{ .rule = .expr }));
    try testing.expectEqual(null, try runParseTest( "var[a]", .{ .rule = .expr }));
    try testing.expectEqual(null, try runParseTest( "~var[a]", .{ .rule = .expr }));
    try testing.expectEqual(null, try runParseTest( "^x+ &y+z", .{ .rule = .expr }));
}
    
test "Test Bad Expr Parses" {
    testing.log_level = .err; //Don't print errors for bad inputs
    try testing.expectEqual(2, try runParseTest( "13'b0 <<", .{ .rule = .expr }));
    try testing.expectEqual(4, try runParseTest( "var[a][b]", .{ .rule = .expr })); //Multidimensional arrays not supported
    try testing.expectEqual(3, try runParseTest( "^x- -y+z", .{ .rule = .expr }));
}

test "Module Good Parses" {
    try testing.expectEqual(null, try runParseTest( "module mymodule () {}", .{ .rule = .module }));
    try testing.expectEqual(null, try runParseTest( "module mymodule [+clk] () {}", .{ .rule = .module }));
    try testing.expectEqual(null, try runParseTest( "module mymodule [+clk,-resetn] () {}", .{ .rule = .module }));
    try testing.expectEqual(null, try runParseTest( "module mymodule [+clk,+clk,+clk] () {}", .{ .rule = .module })); //Semantically might not be right, but syntactically fine
}

test "Module Bad Parses" {
    testing.log_level = .err; //Don't print errors for bad inputs
    try testing.expectEqual(1, try runParseTest( "module module () {}", .{ .rule = .module })); //error keywork as name
    try testing.expectEqual(3, try runParseTest( "module mymodule [] () {}", .{ .rule = .module })); //[] needs at least one value within
    try testing.expectEqual(3, try runParseTest( "module mymodule [clk] () {}", .{ .rule = .module })); //terms in [] need +- signifiers
    try testing.expectEqual(3, try runParseTest( "module mymodule [^clk] () {}", .{ .rule = .module })); //terms in [] need +- signifiers
}

test "Block" {
    try testing.expectEqual(null, try runParseTest( "proc x = y;", .{ .rule = .block }));
    try testing.expectEqual(null, try runParseTest( "comb x = y;", .{ .rule = .block }));
    try testing.expectEqual(null, try runParseTest( "proc {x = y;\nif &a {x = x << x;}}", .{ .rule = .block }));
}

test "Declaration" {
    try testing.expectEqual(null, try runParseTest( "signal a;", .{ .rule = .declaration }));
    try testing.expectEqual(null, try runParseTest( "signal a, b, c;", .{ .rule = .declaration }));
    try testing.expectEqual(null, try runParseTest( "signal [5] a;", .{ .rule = .declaration }));
}

test "Instantiation" {
    try testing.expectEqual(null, try runParseTest( "modulname i_modulename ();", .{ .rule = .instantiation }));
    try testing.expectEqual(null, try runParseTest( "modulname i_modulename (.clk(clk), .sig(sig3));", .{ .rule = .instantiation }));
}

test "Full Module" {
    const mod =
\\module fifo [+clk,-resetn] (
\\  input valid_up,
\\  output enable_up,
\\  input [8] data_up,
\\
\\  output valid_down,
\\  input enable_down,
\\  output [8] data_down,
\\) {
\\  signal [4] read_ptr, write_ptr;
\\  signal [4] read_ptr_next, write_ptr_next;
\\  signal do_read, do_write;
\\  signal empty, full;
\\  
\\  proc {
\\      read_ptr = read_ptr_next;
\\      write_ptr = write_ptr_next;
\\  }
\\
\\  comb empty = read_ptr == write_ptr;
\\  comb valid_down = ~empty;
\\  comb do_read = valid_down & enable_down;
\\
\\  comb full = ((read_ptr & 4'b0111) == (write_ptr & 4'b0111)) & (read_ptr[3] ^ write_ptr[3]);
\\  comb enable_up = ~full;
\\  comb do_write = valid_up & enable_up;
\\
\\  comb {
\\      read_ptr_next = read_ptr;
\\      write_ptr_next = write_ptr;
\\      if (do_read) {
\\          read_ptr_next = read_ptr + 1'b1;
\\      }
\\      if (do_write) {
\\          write_ptr_next = write_ptr + 1'b1;
\\      }
\\  }
\\
\\}
;
    try testing.expectEqual(null, try runParseTest(mod, .{ .rule = .module }));
}
