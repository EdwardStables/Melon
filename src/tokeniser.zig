const std = @import("std");
const builtin = @import("builtin");
const ArrayList = std.ArrayList;
const testing = std.testing;
const assert = std.debug.assert;

const log = if (builtin.is_test)
    struct {
        const base = std.log.scoped(.tokeniser);
        const err = warn;
        const warn = base.warn;
        const info = base.info;
        const debug = base.debug;
    }
else
    log.scoped(.tokeniser);

//Read source code from input buffer and turn into a list of tokens

const TokeniserError = error{
    UnknownTokenError,
    InputEndedError,
    InternalTokeniserError,
    MalformedToken,
    LiteralWidthError,
};

/// Used by grammar, not tokeniser
/// Is linear in tokensize but only runs at compile time on limit input
pub fn tokenFromTokenString(str: []const u8) ?Token {
    inline for (@typeInfo(Token).@"enum".fields) |f| {
        if (std.mem.eql(u8, str, f.name)) return @enumFromInt(f.value);
    }

    return null;
}

pub const Token = enum {
    //Keywords
    KW_module,
    KW_input,
    KW_output,
    KW_signal,
    KW_proc,
    KW_comb,
    KW_if,

    //Structural Symbols
    SS_open_brace, //{
    SS_close_brace, //}
    SS_open_bracket, //[
    SS_close_bracket, //]
    SS_open_parenthesis, //(
    SS_close_parenthesis, //)
    SS_semi_colon,
    SS_comma,
    SS_assign,

    //Operators
    OP_add,
    OP_subtract,
    OP_lsl,
    OP_lsr,
    OP_concat,
    OP_and,
    OP_dot,
    OP_or,
    OP_xor,
    OP_negate,
    OP_equals,
    OP_gt,
    OP_lt,
    OP_gte,
    OP_lte,

    //Valued,
    VL_variable, // a-zA-Z0-9_
    VL_integer_literal, // Widthless binary: 0 or 1

    //Special symbols used by parser, can never actually be tokenised from input
    PR_EMPTY,
    PR_END,
};

const IntegerWithWidth = struct { //TODO needs to be arbitrary
    val: u64,
    width: u8,
};

const Location = struct {
    line: usize,
    column: usize,
};

pub const TokenBuffer = struct {
    tokens: []Token,
    locations: []Location,
    variable_values: []?[]const u8,
    integer_literal_values: []?IntegerWithWidth,
    size: u32,
    capacity: u32,
    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn init(capacity: u32, alloc: std.mem.Allocator) !Self {
        return .{
            .tokens = try alloc.alloc(Token, capacity),
            .locations = try alloc.alloc(Location, capacity),
            .variable_values = try alloc.alloc(?[]const u8, capacity),
            .integer_literal_values = try alloc.alloc(?IntegerWithWidth, capacity),
            .size = 0,
            .capacity = capacity,
            .allocator = alloc,
        };
    }

    pub fn deinit(self: Self) void {
        self.allocator.free(self.tokens);
        self.allocator.free(self.locations);
        self.allocator.free(self.variable_values);
        self.allocator.free(self.integer_literal_values);
    }

    fn clear(self: *Self) void {
        self.size = 0;
    }

    fn add(self: *Self, token: Token, loc: Location, variable: ?[] const u8, value: ?IntegerWithWidth) !void {
        if (self.size == self.capacity) {
            return error.FullBuffer;
        }

        self.tokens[self.size] = token;
        self.locations[self.size] = loc;
        self.variable_values[self.size] = variable;
        self.integer_literal_values[self.size] = value;

        self.size += 1;
    }

    pub fn addToken(self: *Self, loc: Location, token: Token) !void {
        if (token == .VL_variable or token == .VL_integer_literal)
            return error.InternalTokeniserError;
        try self.add(token, loc, null, null);
    }

    pub fn addVariable(self: *Self, loc: Location, variable: []const u8) !void {
        try self.add(.VL_variable, loc, variable, null);
    }

    pub fn addLiteral(self: *Self, loc: Location, value: IntegerWithWidth) !void {
        try self.add(.VL_integer_literal, loc, null, value);
    }
};

fn isSingleCharacterToken(c: u8) bool {
    return switch (c) {
        '{', '}', '[', ']', '(', ')', ';', '=', '<', '>', '+', '-', '&', '|', '^', '~', '#' => true,
        else => false,
    };
}

/// Return true for various characters that would end a multi-character token
/// Does not account for other single character tokens as they are handled separately
fn isTokenDelimiter(c: u8) bool {
    return switch (c) {
        ' ', '\n', '\t' => true,
        else => false,
    };
}

fn readLiteralToken(trial_token: []const u8) !IntegerWithWidth {
    var width: ?u8 = null;
    var apostrophe: usize = 0;

    //Find width first
    if (trial_token[0] != '\'') {
        for (trial_token, 0..) |c, i| {
            if (c == '\'') {
                apostrophe = i;
                width = std.fmt.parseInt(u8, trial_token[0..i], 10) catch |err| {
                    if (err == error.Overflow) {
                        log.err("Maximum signal width currently limited to 255", .{});
                        return err;
                    }
                    unreachable;
                };
                break;
            } else if (c >= '0' and c <= '9') {
                //do nothing
            } else {
                log.err("Unexpected character '{}' found while parsing literal width", .{ c });
                return error.MalformedToken;
            }
        } else {
            log.err("Could not find width delimited ' while parsing literal value", .{});
            return error.MalformedToken;
        }
    }

    if (apostrophe == trial_token.len - 1) {
        log.err("Literal declaration lacks base specifier and value", .{});
        return error.MalformedToken;
    }

    const base_char: u8 = trial_token[apostrophe + 1];
    if (base_char != 'b' and base_char != 'h' and base_char != 'd') {
        log.err("Literal declaration base specifier must be one of h, d, or b. Found value '{}'", .{ base_char});
        return error.MalformedToken;
    }

    const value_start = apostrophe + 2;
    const base: u8 = switch (base_char) {
        'h' => 16,
        'd' => 10,
        'b' => 2,
        else => unreachable,
    };

    if (value_start == trial_token.len) {
        log.err("Literal declaration lacks value", .{});
        return error.MalformedToken;
    }

    const val = std.fmt.parseInt(u64, trial_token[value_start..], base) catch |err| {
        if (err == error.Overflow) {
            log.err("Maximum signal value size currently limited to 64 bits", .{});
            return err;
        }
        if (err == error.InvalidCharacter) {
            log.err("Found unexpected character while parsing base {} value {s} bits", .{ base, trial_token[value_start..]});
            return err;
        }
        unreachable;
    };

    const floor = std.math.floor;
    const log2 = std.math.log2;
    const required_width = if (val == 0) 1 else blk: {
        const float_bits = floor(log2(@as(f32, @floatFromInt(val))));
        break :blk @as(u8, @intFromFloat(float_bits)) + 1;
    };
    if (width == null) {
        width = required_width;
    } else if (width.? < required_width) {
        log.err("Value {} requires signal width of {}, but width was specified as {}", .{ val, required_width, width.?});
        return error.LiteralWidthError;
    }

    return .{ .val = val, .width = width orelse unreachable };
}

const MultiCharToken = struct {
    token: Token,
    variable: ?[]const u8,
    value: ?IntegerWithWidth,
};

fn readMultiCharToken(trial_token: []const u8) !MultiCharToken {
    const keywords = [_][]const u8{ "signal", "proc", "comb", "input", "output", "module" };
    const tokens = [_]Token{ .KW_signal, .KW_proc, .KW_comb, .KW_input, .KW_output, .KW_module};
    assert(keywords.len == tokens.len); //Make sure we don't make an error while editing

    for (keywords, tokens) |kwd, tk| {
        if (std.mem.eql(u8, trial_token, kwd)) {
            return .{.token=tk, .variable=null, .value=null};
        }
    }

    //number
    //Starts with a number (value or width) or apostrophe
    if (trial_token[0] == '\'' or (trial_token[0] >= '0' and trial_token[0] <= '9')) {
        const vww = try readLiteralToken(trial_token);
        return .{.token=.VL_integer_literal, .variable=null, .value=vww};
    }

    //some variable
    for (trial_token) |c| {
        if (!(c >= '0' and c <= '9') and !(c >= 'A' and c <= 'Z') and !(c >= 'a' and c <= 'z') and c != '_') {
            log.err("Invalid character in variable '{c}'", .{ c });
            return error.UnknownTokenError;
        }
    }
    if (trial_token[0] >= '0' and trial_token[0] <= '9') {
        log.err("Cannot begin a variable name with a number", .{});
        return error.UnknownTokenError;
    }

    return .{.token=.VL_variable, .variable=trial_token, .value=null};
}

/// Consume as many characters as possible from the input buffer and tokenise, storing in the token buffer.
/// Returns the number of consumed characters from the input buffer.
pub fn tokenise(input_buffer: [] const u8, token_buffer: *TokenBuffer) !u32 {
    var line: usize = 0;
    var col: usize = 0;

    const State = enum {
        Idle, //Consume the next token
        InToken, //Currently within a token
        Skip, //Previous action consumed one extra character, so skip this iteration
        InComment, //Ignore inputs until new line
    };

    var state: State = .Idle;
    var multi_char_token_start_index: u32 = 0;
    var consumed_characters: u32 = 0;

    for (input_buffer, 0..) |c, i| {
        consumed_characters += 1;

        const c_n = if (i < input_buffer.len - 1) input_buffer[i + 1] else null;
        const last_char = c_n == null;

        // Position handling
        if (c == '\n') {
            assert(state == .Idle);
            line += 1;
            col = 0;
            continue;
        } else {
            col += 1;
        }

        //Handle various incoming states
        if (state == .Skip) {
            state = .Idle;
            continue;
        }

        if (state == .InComment) {
            if (c_n != null and c_n == '\n') {
                state = .Idle;
            }
            continue;
        }

        const sct = isSingleCharacterToken(c);
        const td = isTokenDelimiter(c);

        if (state != .InToken) {
            if (!sct and !td) {
                state = .InToken;
            }
            multi_char_token_start_index = @intCast(i);
        } 

        if (state == .InToken and (sct or td or last_char)) {
            const end = if (sct or td) i else i + 1;
            const mct = try readMultiCharToken(input_buffer[multi_char_token_start_index..end]);
            const loc: Location = .{.column = multi_char_token_start_index, .line = line};
            switch (mct.token) {
                .VL_variable => try token_buffer.addVariable(loc, mct.variable.?),
                .VL_integer_literal => try token_buffer.addLiteral(loc, mct.value.?),
                else => try token_buffer.addToken(loc, mct.token),
            }
            state = .Idle;
            multi_char_token_start_index = 0;
            if (token_buffer.size == token_buffer.capacity) {
                // We don't want to indicate we have consumed single character tokens if we are full
                // So decrement the counter so it will be consumed on the next pass
                if (sct) consumed_characters -= 1;
                break;
            }

        }

        //Process single character tokens, as well as ++ and == which are more easily treated as special cases
        if (sct) {
            assert(state == .Idle); //We assume one character lookahead has put us into idle before reaching here
            const loc: Location = .{.column = col, .line = line};
            //single character tokens:
            switch (c) {
                '{' => try token_buffer.addToken(loc, .SS_open_brace),
                '}' => try token_buffer.addToken(loc, .SS_close_brace),
                '[' => try token_buffer.addToken(loc, .SS_open_bracket),
                ']' => try token_buffer.addToken(loc, .SS_close_bracket),
                '(' => try token_buffer.addToken(loc, .SS_open_parenthesis),
                ')' => try token_buffer.addToken(loc, .SS_close_parenthesis),
                ';' => try token_buffer.addToken(loc, .SS_semi_colon),
                '=' => {
                    if (c_n != null and c_n.? == '=') {
                        try token_buffer.addToken(loc, .OP_equals);
                        state = .Skip;
                    } else {
                        try token_buffer.addToken(loc, .SS_assign);
                    }
                },
                '+' => {
                    if (c_n != null and c_n.? == '+') {
                        try token_buffer.addToken(loc, .OP_concat);
                        state = .Skip;
                    } else {
                        try token_buffer.addToken(loc, .OP_add);
                    }
                },
                '<' => {
                    if (c_n != null and c_n.? == '<') {
                        try token_buffer.addToken(loc, .OP_lsl);
                        state = .Skip;
                    }
                    else if (c_n != null and c_n.? == '=') {
                        try token_buffer.addToken(loc, .OP_lte);
                        state = .Skip;
                    } else {
                        try token_buffer.addToken(loc, .OP_lt);
                    }
                },
                '>' => {
                    if (c_n != null and c_n.? == '>') {
                        try token_buffer.addToken(loc, .OP_lsr);
                        state = .Skip;
                    } else if (c_n != null and c_n.? == '=') {
                        try token_buffer.addToken(loc, .OP_gte);
                        state = .Skip;
                    } else {
                        try token_buffer.addToken(loc, .OP_gt);
                    }
                },
                '-' => try token_buffer.addToken(loc, .OP_subtract),
                '&' => try token_buffer.addToken(loc, .OP_and),
                '|' => try token_buffer.addToken(loc, .OP_or),
                '^' => try token_buffer.addToken(loc, .OP_xor),
                '~' => try token_buffer.addToken(loc, .OP_negate),
                '#' => state = .InComment,
                else => unreachable,
            }

            if (token_buffer.size == token_buffer.capacity) break;
        }
    }

    return @intCast(consumed_characters);
}

test "Literal: Single bit parsing" {
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 0, .width = 1 }, readLiteralToken("1'd0"));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 1, .width = 1 }, readLiteralToken("1'd1"));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 0, .width = 1 }, readLiteralToken("'d0"));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 1, .width = 1 }, readLiteralToken("'d1"));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 0, .width = 1 }, readLiteralToken("1'h0"));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 1, .width = 1 }, readLiteralToken("1'h1"));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 0, .width = 1 }, readLiteralToken("'h0"));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 1, .width = 1 }, readLiteralToken("'h1"));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 0, .width = 1 }, readLiteralToken("1'b0"));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 1, .width = 1 }, readLiteralToken("1'b1"));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 0, .width = 1 }, readLiteralToken("'b0"));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 1, .width = 1 }, readLiteralToken("'b1"));
}

test "Literal: Binary parsing" {
    //Various correct forms
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 7, .width = 3 }, readLiteralToken("'b00111"));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 8, .width = 4 }, readLiteralToken("'b01000"));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 5, .width = 10 }, readLiteralToken("10'b00101"));

    //Invalid values
    testing.log_level = .err;
    try testing.expectError(error.InvalidCharacter, readLiteralToken("10'b00201"));
    try testing.expectError(error.LiteralWidthError, readLiteralToken("3'b1111"));
}

test "Literal: Decimal parsing" {
    //Various correct forms
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 109, .width = 7 }, readLiteralToken("'d109"));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 123, .width = 7 }, readLiteralToken("'d0123"));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 1023, .width = 10 }, readLiteralToken("10'd1023"));

    //Invalid values
    testing.log_level = .err;
    try testing.expectError(error.InvalidCharacter, readLiteralToken("10'd00f01"));
    try testing.expectError(error.LiteralWidthError, readLiteralToken("3'd8"));
}

test "Literal: Hex parsing" {
    //Various correct forms
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 0x109, .width = 9 }, readLiteralToken("'h109"));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 0x123, .width = 9 }, readLiteralToken("'h0123"));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 0x0321, .width = 16 }, readLiteralToken("16'h0321"));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 0x0321, .width = 10 }, readLiteralToken("10'h321"));

    //Invalid values
    testing.log_level = .err;
    try testing.expectError(error.InvalidCharacter, readLiteralToken("10'h00fkj"));
    try testing.expectError(error.LiteralWidthError, readLiteralToken("3'hF"));
}

test "Literal: Malformed structure errors" {
    //Invalid values
    testing.log_level = .err;
    try testing.expectError(error.MalformedToken, readLiteralToken("10h3ff"));
    try testing.expectError(error.MalformedToken, readLiteralToken("1d'h3ff"));
    try testing.expectError(error.MalformedToken, readLiteralToken("10'3ff"));
}

test "MultiCharTokeniser: keywords" {
    try testing.expectEqual(MultiCharToken{ .token = .KW_module, .value = null, .variable = null }, readMultiCharToken("module"));
    try testing.expectEqual(MultiCharToken{ .token = .KW_input, .value = null, .variable = null }, readMultiCharToken("input"));
    try testing.expectEqual(MultiCharToken{ .token = .KW_output, .value = null, .variable = null }, readMultiCharToken("output"));
    try testing.expectEqual(MultiCharToken{ .token = .KW_signal, .value = null, .variable = null }, readMultiCharToken("signal"));
    try testing.expectEqual(MultiCharToken{ .token = .KW_proc, .value = null, .variable = null }, readMultiCharToken("proc"));
    try testing.expectEqual(MultiCharToken{ .token = .KW_comb, .value = null, .variable = null }, readMultiCharToken("comb"));
}

test "MultiCharTokeniser: values" {
    try testing.expectEqual(MultiCharToken{ .token = .VL_integer_literal, .value = .{.val=0x123, .width=9}, .variable = null }, readMultiCharToken("9'h123"));
    try testing.expectEqual(MultiCharToken{ .token = .VL_integer_literal, .value = .{.val=0x123, .width=9}, .variable = null }, readMultiCharToken("'h123"));
    try testing.expectEqual(MultiCharToken{ .token = .VL_integer_literal, .value = .{.val=0x123, .width=10}, .variable = null }, readMultiCharToken("10'h123"));
}

test "MultiCharTokeniser: variables" {
    try testing.expectEqual(MultiCharToken{ .token = .VL_variable, .value = null, .variable = "someVar"}, readMultiCharToken("someVar"));
    try testing.expectEqual(MultiCharToken{ .token = .VL_variable, .value = null, .variable = "_var_with_underscore"}, readMultiCharToken("_var_with_underscore"));
    try testing.expectEqual(MultiCharToken{ .token = .VL_variable, .value = null, .variable = "_var_with_1234321"}, readMultiCharToken("_var_with_1234321"));

    testing.log_level = .err;
    try testing.expectError(error.UnknownTokenError, readMultiCharToken("_var_with_!_bad_char"));
}

fn expectEqualStringSlice(expected: []const ?[]const u8, actual: []const ?[]const u8) !void {
    if (expected.len != actual.len) {
        std.debug.print("Length mismatch: expected {} found {}\n", .{expected.len, actual.len});
        return error.TestExpectedEqual;
    }

    for (expected, actual, 0..) |e,a,i| {
        if (e == null and a == null) continue;

        if (e == null) {
            std.debug.print("Value mismatch at index {}. Expected null but found {s}\n", .{i, a.?});
            return error.TestExpectedEqual;
        }
        if (a == null) {
            std.debug.print("Value mismatch at index {}. Expected {s} but found null\n", .{i, e.?});
            return error.TestExpectedEqual;
        }

        testing.expectEqualStrings(e.?, a.?) catch |err| {
            std.debug.print("String mismatch at index {}\n", .{i});
            return err;
        };
    }
}

fn expectEqualTokens(input: []const u8, tokens: []const Token, variables: []const ?[]const u8, values: []const ?IntegerWithWidth) !void {
    var buffer = try TokenBuffer.init(1024, std.testing.allocator); defer buffer.deinit();
    const consumed = try tokenise(input, &buffer);

    try testing.expectEqual(input.len, consumed);
    try testing.expectEqualSlices(?IntegerWithWidth, values, buffer.integer_literal_values[0..tokens.len]);
    try expectEqualStringSlice(variables, buffer.variable_values[0..tokens.len]);
    try testing.expectEqualSlices(Token, tokens, buffer.tokens[0..tokens.len]);
}    


test "Tokeniser: single keyword" {
    const inp = "module";
    const exp_tok = [_]Token{.KW_module};
    const exp_var = [_]?[]const u8{null};
    const exp_val = [_]?IntegerWithWidth{null};

    try expectEqualTokens(inp, &exp_tok, &exp_var, &exp_val);
}

test "Tokeniser: single keyword with comment" {
    const inp = "module #and a comment";
    const exp_tok = [_]Token{.KW_module};
    const exp_var = [_]?[]const u8{null};
    const exp_val = [_]?IntegerWithWidth{null};

    try expectEqualTokens(inp, &exp_tok, &exp_var, &exp_val);
}

test "Tokeniser: multiple keywords" {
    const inp = "module input   output signal     proc comb";
    
    const exp_tok = [_]Token{.KW_module, .KW_input, .KW_output, .KW_signal, .KW_proc, .KW_comb};
    const exp_var = [_]?[]const u8{null,null,null,null,null,null};
    const exp_val = [_]?IntegerWithWidth{null,null,null,null,null,null};

    try expectEqualTokens(inp, &exp_tok, &exp_var, &exp_val);
}
test "Tokeniser: mix of tokens" {
    const inp = "module {signal ['d1] abc;}";
    
    const exp_tok = [_]Token{.KW_module, .SS_open_brace, .KW_signal, .SS_open_bracket, .VL_integer_literal, .SS_close_bracket, .VL_variable, .SS_semi_colon, .SS_close_brace};
    const exp_var = [_]?[]const u8{null,null,null,null,null,null,"abc",null,null};
    const exp_val = [_]?IntegerWithWidth{null,null,null,null,.{.val=1,.width=1},null,null,null,null};
    try expectEqualTokens(inp, &exp_tok, &exp_var, &exp_val);
}

test "Tokeniser: special case tokens" {
    const inp = "++ + == = << <= < >> >= >"; //Not valid code, but does include special case operators
    
    const exp_tok = [_]Token{.OP_concat, .OP_add, .OP_equals, .SS_assign,
                             .OP_lsl, .OP_lte, .OP_lt,
                             .OP_lsr, .OP_gte, .OP_gt};
    const exp_var = [_]?[]const u8{null,null,null,null,null,null,null,null,null,null};
    const exp_val = [_]?IntegerWithWidth{null,null,null,null,null,null,null,null,null,null};
    try expectEqualTokens(inp, &exp_tok, &exp_var, &exp_val);
}

test "Tokeniser: mix of tokens and newlines/comments" {
    const inp = 
\\module modmod { #module is called modmod
\\    # We need to modify the syntax to not require widthed integers in signals
\\    signal ['d1] abc;
\\}
;
    
    const exp_tok = [_]Token{.KW_module, .VL_variable, .SS_open_brace, .KW_signal, .SS_open_bracket, .VL_integer_literal, .SS_close_bracket, .VL_variable, .SS_semi_colon, .SS_close_brace};
    const exp_var = [_]?[]const u8{null,"modmod",null,null,null,null,null,"abc",null,null};
    const exp_val = [_]?IntegerWithWidth{null,null,null,null,null,.{.val=1,.width=1},null,null,null,null};
    try expectEqualTokens(inp, &exp_tok, &exp_var, &exp_val);
}

test "Tokeniser: fill small buffer" {
    const input = "module module+   \n module ";
    const token1 = [_]Token{.KW_module};
    const token2 = [_]Token{.OP_add};
    const variables = [_]?[]const u8{null};
    const values = [_]?IntegerWithWidth{null};

    var buffer = try TokenBuffer.init(1, std.testing.allocator); defer buffer.deinit();

    var consumed = try tokenise(input, &buffer);
    try testing.expectEqual(7, consumed);
    try testing.expectEqualSlices(?IntegerWithWidth, &values, buffer.integer_literal_values[0..1]);
    try expectEqualStringSlice(&variables, buffer.variable_values[0..1]);
    try testing.expectEqualSlices(Token, &token1, buffer.tokens[0..1]);
    buffer.clear();

    consumed += try tokenise(input[consumed..], &buffer);
    try testing.expectEqual(13, consumed);
    try testing.expectEqualSlices(?IntegerWithWidth, &values, buffer.integer_literal_values[0..1]);
    try expectEqualStringSlice(&variables, buffer.variable_values[0..1]);
    try testing.expectEqualSlices(Token, &token1, buffer.tokens[0..1]);
    buffer.clear();

    consumed += try tokenise(input[consumed..], &buffer);
    try testing.expectEqual(14, consumed);
    try testing.expectEqualSlices(?IntegerWithWidth, &values, buffer.integer_literal_values[0..1]);
    try expectEqualStringSlice(&variables, buffer.variable_values[0..1]);
    try testing.expectEqualSlices(Token, &token2, buffer.tokens[0..1]);
    buffer.clear();

    consumed += try tokenise(input[consumed..], &buffer);
    try testing.expectEqual(26, consumed);
    try testing.expectEqualSlices(?IntegerWithWidth, &values, buffer.integer_literal_values[0..1]);
    try expectEqualStringSlice(&variables, buffer.variable_values[0..1]);
    try testing.expectEqualSlices(Token, &token1, buffer.tokens[0..1]);
}