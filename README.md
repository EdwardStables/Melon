# Toy Logic Sim

This is just a little project to do logic simulation for a simple gate level simluation style language. The simulation is cycle based, meaning it is not quite as accurate to reality as a more fully featured Verilog simulator, and cannot correctly handle things like CDC logic, that is beyond the scope of this project.

The language itself takes many ideas from Verilog/SystemVerilog but is significantly simpler. It is intended that this project will either simulate its own logic (suitable for module level/simple system level development), or output functionally identical SystemVerilog.

## Example Programs

### Combinational Logic

```
# Three five bit signals
signal [5] a, b, c;

# A single bit signal written two ways
signal d;
signal [1] e;

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
signal [3] a;
cont a = 0; # Error, 0 is a 1 bit signal and a is 3 bits
cont a = 010; # Acceptable
cont a = 3; # Error, only binary literals can be given without width
cont a = d3; # Acceptable
cont a = d10; # Error, literal too big for destination
cont a = 3'h5; # Acceptable, exact width matches
cont a = 3'hF; # Error, width too small for literal value
cont a = 2'h3; # Error, literal is 2 bits and a is 3 bits
```

### Procedural (Clocked) Logic

Procedural logic is syntactically similar:

```
signal [5] a, b, c;
signal d, c;

# Procedural blocks have up to two bodies
#    If there are two bodies then the first is the reset operation, called whenever the reset condition is met
#    The second is the clock body, called whenever the clock condition is met. This is always present.

proc {
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

# Single line statements can be used
proc c = ~d;

```

### Modules

All logic is organised into modules. Modules have input and output ports, and dedicated clk and reset ports which implicitly drive procedural blocks within the module.

```
module my_module
(
+clk,   #Clock listed after module name
-resetn #Reset after this
)
(
    input [5] data_up,
    input valid_up,
    output enable_up,

    output [5] data_down,
    output valid_down,
    input enable_down
) {
    # Module logic within
}
```

A purely combinational module does not need to list clk and reset signals. A module with no reset blocks can skip the reset signal. The compiler will error if a clocked/reset block is present without the corresponding port being defined.

The +/- prefix on the signals indicate sensitivity as rising/falling edges respectively.

Modules are instantiated within other modules. They use a named port scheme as with SystemVerilog. Clock and reset ports may be connected explicitly if the names match with the clock and reset ports of the outer module.

```
my_module i_my_module_instance(
    .valid_up(valid_signal),
    .valid_down(other_valid_signal),
);
```

Clock and resets can be connected explicitly, only intended for use if the naming scheme changes as normal signals do not drive clocks or resets.

```
my_module (clk,renamed_resetn) i_my_module_instance (...);
