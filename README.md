# Toy Logic Sim

This is just a little project to do logic simulation for a simple gate level simluation style language. Using a delta-cycle scheduling approach.

The language itself takes many ideas from Verilog/SystemVerilog but is significantly simpler.

## Example Program

Combinational logic:

```
# Three five bit signals
signal [4:0] a, b, c;

# A signal bit signal
signal d;

# Continuous blocks run each statement in-turn and must assign to all outputs
# This runs every time an input changes
cont {
    a = 0;
    if b > c {
        a = b;
    }
}

For a single statement the cont braces are not required, but the keyword is
cont d = |a;
```

`signal` is the basic data type, indicating a logical vector set to any width. It can be partially assigned and have verilog-style logical operations performed on it:
```
cont a = ~b; #Bitwise inversion
cont a = b + c; # Overflows are truncated automatically
cont a = b - c; # Likewise with underflows, no signed behavior exists for signals 
cont a = b[4:3] ++ c[2:1] ++ 0; # ++ is the concatenation operator, widths must match destination
cont a = b & c; # Bitwise and
cont a = b && c; # Error, && for logical and doesn't exist
cont a = if b and c {b} else {c} # Logic and uses the and keyword and can only be used in specific circumstances. Note the use of if as an expression
```

Literal values can be provided in binary, decimal, or hex. b, h, d signifiers must be used, unless a binary literal of exact width is given.
Literals do not need to be given a width in many cases (values too large will not compile, otherwise they will be 0-extended), but verilog-style widths can be set. If a width is set that does not match the destination width then it will error.
```
signal [2:0] a;
cont a = 0; # Error, 0 is a 1 bit signal and a is 3 bits
cont a = 010; # Acceptable
cont a = 3; # Error, only binary literals can be given without width
cont a = d3; # Acceptable
cont a = d10; # Error, literal too big for destination
cont a = 3'h5; # Acceptable, exact width matches
cont a = 3'hF; # Error, width too small for literal value
cont a = 2'h3; # Error, literal is 2 bits and a is 3 bits
```

Procedural logic is syntactically similar:

```
signal [4:0] a, b, c;
signal d, c;

# Procedural blocks take up to two arguments and have up to two bodies
# Argument one is the clock signal, argument two is an async reset
# If two arguments are provided then there must be two bodies.
#    The first is the reset operation, called whenever the reset condition is met
#    The second is the clock body, called whenever the clock condition is met

#Note the +- syntax, this indicates edge sensitivty, rising and falling respectively
proc(+clk, -resetn)
{
    a = 0;
}
# The clock body uses a non-blocking style assignment
# In this example the value assigned to d will be the or-reduction of the value of a before the block is called, not the value of b
{
    if b > c {
        a = b;
    }

    d = |a;
}

# Single line statements can be used, with a default clock (and no reset).
proc c = ~d;

# One statement per module can set procedural default sensitivities.
# This applies whenever a sensitivity is not included, but can be overridden
module_sens(+clk, -resetn);

```