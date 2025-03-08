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

const Token = enum {
    //Keywords
    KW_module,
    KW_input,
    KW_output,
    KW_signal,
    KW_proc,
    KW_comb,

    //Structural Symbols
    SS_open_brace, //{
    SS_close_brace, //}
    SS_open_bracket, //[
    SS_close_bracket, //]
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
    OP_or,
    OP_xor,
    OP_negate,
    OP_equals,

    //Valued,
    VL_variable, // a-zA-Z0-9_
    VL_integer_literal, // Widthless binary: 0 or 1
    //
    //
    //
};

const IntegerWithWidth = struct { //TODO needs to be arbitrary
    val: u64,
    width: u8,
};

const Location = struct {
    line: usize,
    column: usize,
};

const ValuedToken = struct {
    token: Token,
    variable_index: ?usize,
    integer_literal_index: ?usize,
    location: Location,
};

const TokenisedBuffer = struct {
    tokens: ArrayList(ValuedToken),
    variable_values: ArrayList([]u8),
    integer_literal_values: ArrayList(IntegerWithWidth),

    const Self = @This();

    fn init(alloc: std.mem.Allocator) Self {
        return .{
            .tokens = ArrayList(ValuedToken).init(alloc),
            .variable_values = ArrayList([]u8).init(alloc),
            .integer_literal_values = ArrayList(IntegerWithWidth).init(alloc),
        };
    }

    fn deinit(self: Self) void {
        self.tokens.deinit();
        self.variable_values.deinit();
        self.integer_literal_values.deinit();
    }

    fn addToken(self: Self, line: usize, column: usize, token: Token) !void {
        if (token == .VL_variable or token == .VL_integer_literal)
            return error.InternalTokeniserError;
        self.tokens.append(.{
            .token = token,
            .variable_index = null,
            .integer_literal_index = null,
            .location = .{ .line = line, .column = column },
        });
    }

    fn addVariable(self: Self, line: usize, column: usize, variable: []u8) !void {
        self.tokens.append(.{
            .token = .VL_variable,
            .variable_index = self.variable_values.items.len,
            .integer_literal_index = null,
            .location = .{ .line = line, .column = column },
        });
        self.variable_values.append(variable);
    }

    fn addLiteral(self: Self, line: usize, column: usize, literal: u64, width: u8) !void {
        self.tokens.append(.{
            .token = .VL_variable,
            .variable_index = null,
            .integer_literal_index = self.integer_literal_values.items.len,
            .location = .{ .line = line, .column = column },
        });
        self.variable_values.append(.{ .val = literal, .width = width });
    }
};

fn isSingleCharacterToken(c: u8) bool {
    return switch (c) {
        '{', '}', '[', ']', ';', '=', '+', '-', '&', '|', '^', '~' => true,
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

fn readLiteralToken(filename: []const u8, trial_token: []const u8, line: usize, col: usize) !IntegerWithWidth {
    var width: ?u8 = null;
    var apostrophe: usize = 0;
    const start_col = col - (trial_token.len - 1);

    //Find width first
    if (trial_token[0] != '\'') {
        for (trial_token, 0..) |c, i| {
            if (c == '\'') {
                apostrophe = i;
                width = std.fmt.parseInt(u8, trial_token[0..i], 10) catch |err| {
                    if (err == error.Overflow) {
                        log.err("Maximum signal width currently limited to 255 at {s}:{}:{}", .{ filename, line, start_col + i });
                        return err;
                    }
                    unreachable;
                };
                break;
            } else if (c >= '0' and c <= '9') {
                //do nothing
            } else {
                log.err("Unexpected character '{}' found while parsing literal width at {s}:{}:{}", .{ c, filename, line, start_col + i });
                return error.MalformedToken;
            }
        } else {
            log.err("Could not find width delimited ' while parsing literal value at {s}:{}:{}", .{ filename, line, start_col});
            return error.MalformedToken;
        }
    }

    if (apostrophe == trial_token.len - 1) {
        log.err("Literal declaration lacks base specifier and value {s}:{}:{}", .{ filename, line, start_col + apostrophe });
        return error.MalformedToken;
    }

    const base_char: u8 = trial_token[apostrophe + 1];
    if (base_char != 'b' and base_char != 'h' and base_char != 'd') {
        log.err("Literal declaration base specifier must be one of h, d, or b. Found value '{}' at {s}:{}:{}", .{ base_char, filename, line, start_col + apostrophe + 1 });
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
        log.err("Literal declaration lacks value {s}:{}:{}", .{ filename, line, start_col + value_start - 1 });
        return error.MalformedToken;
    }

    const val = std.fmt.parseInt(u64, trial_token[value_start..], base) catch |err| {
        if (err == error.Overflow) {
            log.err("Maximum signal value size currently limited to 64 bits at {s}:{}:{}", .{ filename, line, start_col + value_start });
            return err;
        }
        if (err == error.InvalidCharacter) {
            log.err("Found unexpected character while parsing base {} value {s} bits at {s}:{}:{}", .{ base, trial_token[value_start..], filename, line, start_col + value_start });
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
        log.err("Value {} requires signal width of {}, but width was specified as {}. At {s}:{}:{}", .{ val, required_width, width.?, filename, line, col - trial_token.len + value_start });
        return error.LiteralWidthError;
    }

    return .{ .val = val, .width = width orelse unreachable };
}

fn readMultiCharToken(trial_token: []u8, line: usize, col: usize, token_buffer: *TokenisedBuffer) !void {
    const keywords = [_][]u8{ "signal", "proc", "comb", "input", "output", "module" };
    const tokens = [_]Token{ .KW_module, .KW_input, .KW_output, .KW_signal, .KW_proc, .KW_comb };
    assert(keywords.len == tokens.len); //Make sure we don't make an error while editing

    const filename = "somefilename.txt";

    for (keywords, tokens) |kwd, tk| {
        if (std.mem.eql(u8, trial_token, kwd)) {
            token_buffer.addToken(line, col - kwd.len, tk);
            return;
        }
    }

    //number
    //Starts with a number (value or width) or apostrophe
    if (trial_token == '\'' or (trial_token[0] >= '0' and trial_token[0] <= '9')) {
        const vww = try readLiteralToken(filename, trial_token, line, col);
        token_buffer.addLiteral(line, col, vww.val, vww.width);
        return;
    }

    //some variable
    for (trial_token, 0..) |c, i| {
        if (!(c >= '0' and c <= '9') and !(c >= 'A' and c <= 'Z') and !(c >= 'a' and c <= 'z')) {
            log.err("Invalid character in variable name {} at {s}:{}:{}", .{ c, filename, line, col - trial_token.len + i });
            return error.UnknownTokenError;
        }
    }
    if (trial_token[0] >= '0' and trial_token[0] <= '9') {
        log.err("Cannot begin a variable name with a number at {s}:{}:{}", .{ filename, line, col - trial_token.len });
        return error.UnknownTokenError;
    }

    token_buffer.addVariable(line, col - trial_token.len, trial_token);
}

pub fn tokenise(buffer: []u8, alloc: std.mem.Allocator) !TokenisedBuffer {
    var tokens = TokenisedBuffer.init(alloc);

    var line: usize = 0;
    var col: usize = 0;

    const State = enum {
        Idle, //Consume the next token
        InToken, //Currently within a token
        Skip, //Previous action consumed one extra character, so skip this iteration
        InComment, //Ignore inputs until new line
    };

    var state: State = .Idle;
    var multi_char_token_start_index = 0;

    for (buffer, 0..) |c, i| {
        const c_n = if (i < buffer.len - 1) buffer[i + 1] else null;
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

        if (!sct and !td) {
            state = .InToken;
            multi_char_token_start_index = i;
        }

        if (state == .InToken and (sct or td or last_char)) {
            const end = if (sct or td) i else i + 1;
            try readMultiCharToken(buffer[multi_char_token_start_index..end], line, col, &tokens);
            state = .Idle;
        }

        //Process single character tokens, as well as ++ and == which are more easily treated as special cases
        if (sct) {
            assert(state == .Idle); //We assume one character lookahead has put us into idle before reaching here
            //single character tokens:
            switch (c) {
                '{' => try tokens.addToken(line, col, .SS_open_brace),
                '}' => try tokens.addToken(line, col, .SS_close_brace),
                '[' => try tokens.addToken(line, col, .SS_open_brace),
                ']' => try tokens.addToken(line, col, .SS_close_brace),
                ';' => try tokens.addToken(line, col, .SS_semi_colon),
                '=' => {
                    if (c_n != null and c_n.? == '=') {
                        try tokens.addToken(line, col, .OP_equals);
                        state = .Skip;
                    } else {
                        try tokens.addToken(line, col, .SS_assign);
                    }
                },
                '+' => {
                    if (c_n != null and c_n.? == '+') {
                        try tokens.addToken(line, col, .OP_concat);
                        state = .Skip;
                    } else {
                        try tokens.addToken(line, col, .OP_add);
                    }
                },
                '-' => try tokens.addToken(line, col, .OP_subtract),
                '&' => try tokens.addToken(line, col, .OP_and),
                '|' => try tokens.addToken(line, col, .OP_or),
                '^' => try tokens.addToken(line, col, .OP_xor),
                '~' => try tokens.addToken(line, col, .OP_negate),
                '#' => {
                    state = .InComment;
                },
            }
        }
    }

    return tokens;
}

test "Single bit literal parsing" {
    const nm = "filename";
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 0, .width = 1 }, readLiteralToken(nm, "1'd0", 0, 3));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 1, .width = 1 }, readLiteralToken(nm, "1'd1", 0, 3));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 0, .width = 1 }, readLiteralToken(nm, "'d0", 0, 2));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 1, .width = 1 }, readLiteralToken(nm, "'d1", 0, 2));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 0, .width = 1 }, readLiteralToken(nm, "1'h0", 0, 3));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 1, .width = 1 }, readLiteralToken(nm, "1'h1", 0, 3));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 0, .width = 1 }, readLiteralToken(nm, "'h0", 0, 2));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 1, .width = 1 }, readLiteralToken(nm, "'h1", 0, 2));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 0, .width = 1 }, readLiteralToken(nm, "1'b0", 0, 3));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 1, .width = 1 }, readLiteralToken(nm, "1'b1", 0, 3));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 0, .width = 1 }, readLiteralToken(nm, "'b0", 0, 2));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 1, .width = 1 }, readLiteralToken(nm, "'b1", 0, 2));
}

test "Binary parsing" {
    const nm = "filename";
    //Various correct forms
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 7, .width = 3 }, readLiteralToken(nm, "'b00111", 0, 6));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 8, .width = 4 }, readLiteralToken(nm, "'b01000", 0, 6));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 5, .width = 10 }, readLiteralToken(nm, "10'b00101", 0, 8));

    //Invalid values
    testing.log_level = .err;
    try testing.expectError(error.InvalidCharacter, readLiteralToken(nm, "10'b00201", 0, 8));
}

test "Decimal parsing" {
    const nm = "filename";
    //Various correct forms
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 109, .width = 7 }, readLiteralToken(nm, "'d109", 0, 4));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 123, .width = 7 }, readLiteralToken(nm, "'d0123", 0, 5));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 1023, .width = 10 }, readLiteralToken(nm, "10'd1023", 0, 7));

    //Invalid values
    testing.log_level = .err;
    try testing.expectError(error.InvalidCharacter, readLiteralToken(nm, "10'd00f01", 0, 8));
}

test "Hex parsing" {
    const nm = "filename";
    //Various correct forms
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 0x109, .width = 9 }, readLiteralToken(nm, "'h109", 0, 4));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 0x123, .width = 9 }, readLiteralToken(nm, "'h0123", 0, 5));
    try testing.expectEqualDeep(IntegerWithWidth{ .val = 0x0321, .width = 16 }, readLiteralToken(nm, "16'h0321", 0, 7));
}
