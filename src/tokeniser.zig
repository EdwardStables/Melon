const std = @import("std");
const ArrayList = std.ArrayList;
const testing = std.testing;
const assert = std.debug.assert;

//Read source code from input buffer and turn into a list of tokens

const TokeniserError = error{
    UnknownTokenError,
    InputEndedError,
    InternalTokeniserError,
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

fn readLiteralToken(filename: []u8, trial_token: []u8, line: usize, col: usize, token_buffer: *TokenisedBuffer) !void {
    //TODO restructure this:
    // - first figure out the base
    // - then actually parse stuff
    //This approach for binary will error for valid inputs like 1'b0;


    //No leader binary literal
    if (trial_token[0] == '0' or trial_token[0] == '1') {
        for (trial_token, 0..) |c, i| {
            if (c != '0' and c != '1') {
                std.log.err("Invalid character in binary literal '{}'' at {}:{}:{}", .{c, filename, line, col-trial_token.len+i});
                return error.UnknownTokenError;
            }
            token_buffer.addLiteral(line, col - trial_token.len, std.fmt.parseInt(u64, trial_token, 2), trial_token.len);
        }
    }
}

fn readMultiCharToken(trial_token: []u8, line: usize, col: usize, token_buffer: *TokenisedBuffer) !void {
    const keywords = [_][]u8{"signal", "proc", "comb", "input", "output", "module"};
    const tokens = [_]Token{.KW_module, .KW_input, .KW_output, .KW_signal, .KW_proc, .KW_comb};
    assert(keywords.len == tokens.len); //Make sure we don't make an error while editing

    const filename = "somefilename.txt";

    for (keywords, tokens) |kwd, tk| {
        if (std.mem.eql(u8, trial_token, kwd)) {
            token_buffer.addToken(line, col - kwd.len, tk);
            return;
        }
    }

    //number
    if (trial_token[0] == '0' or trial_token[0] == '1' or trial_token == '\'') {
        try readLiteralToken(filename, trial_token, line, col, token_buffer);
        return;
    }

    //some variable
    for (trial_token, 0..) |c, i| {
        if (!(c >= '0' and c <= '9') and !(c >= 'A' and c <= 'Z') and !(c >= 'a' and c <= 'z')) {
            std.log.err("Invalid character in variable name {} at {}:{}:{}", .{c, filename, line, col-trial_token.len+i});
            return error.UnknownTokenError;
        }
    }
    if (trial_token[0] >= '0' and trial_token[0] <= '9') {
        std.log.err("Cannot begin a variable name with a number at {}:{}:{}", .{filename, line, col-trial_token.len+i});
        return error.UnknownTokenError;
    }

    token_buffer.addVariable(line, col - kwd.len, trial_token);
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
        const c_n = if (i < buffer.len-1) buffer[i+1] else null;
        const last_char = cn == null;

        //Handle various incoming states
        if (state == .Skip) {
            state = .Idle;
            continue;
        }

        if (state == .InComment) {
            if (c_n != null and c_n == '\n') {
                state = .Skip;
            }
            continue;
        }

        const sct = isSingleCharacterToken(c);

        if (state == .InToken and (sct or isTokenDelimiter(c) or last_char)) {
            try readMultiCharToken(buffer[multi_char_token_start_index..(i+1)], line, col, &tokens);
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
                }
            }
        }
    //Keywords

    //Valued,
    VL_variable, // a-zA-Z0-9_
    VL_integer_literal, // Widthless binary: 0 or 1
    }

    return tokens;
}
