Cynth
=====

Cynth is a simple C-to-Verilog compiler.  It is under development.

Building
========

Cynth is written in Scala.  Build it with
[sbt](http://www.scala-sbt.org/) by running:

```
$ sbt stage
```

Cynth will be built in `target/universal/stage`.  To invoke Cynth:

```
$ cynth design.c > design.v
```

Input
=====

Cynth input has the following restrictions:

 - All variables must be scalar integer types.

 - No multiply, divide or modulo.

 - No float, double, array, struct, union or pointers.  No address of
   or function pointers.

 - No global variables.

 - No preprocessor.  Use your favorite.

 - for, break, continue and switch still under development.

I plan to support memories (array, struct, union and pointers).

Interface
=========

Cynth generates a Verilog module for each defined function.  The
interface of the module is determined by the signature of the the
function.  Here is an example:

```
int f(int x);
```

generates the interface:

```
module f(
  input __clk,
  input __resetn,
  input [31:0] __p_x,
  output reg [31:0] __retval,
  input __start,
  output __idle,
  output __valid);
```

In addition to the arguments and the return value, there are five
control signals: the clock, reset, start, idle and valid.  When the
function is idle, it can be invoked by asserting start.  valid is
asserted when the function is complete.  The return value is omitted
if the return type is void.

Extensions
==========

Cynth supports arbitrary-width integer types with `__intN`, e.g.,
`__int4` or `__int1024`.

When compiling a function `f` which calls another function `g`, Cynth
normally instantiates `g` in the implementation of `f`.  If `g` is
declared with `__nonstd`, Cynth instead will instead add the control
signals for `g` to the interface for `f` and the module instantiating
`f` will also be responsible for instantiating `g`.  This is useful if
`g` is implemented in Verilog and has a non-standard interface.  See
the `write_leds` example below.

Example
=======

Here is a simple [example](example) that creates a roving eye pattern
on the Arty Artix-7 board.  The C design is:

```
__nonstd void write_leds(unsigned __int4 c);
void sleep(int ms);

void roving() {
  unsigned __int4 c = 1;
  unsigned __int1 dir = 1;
  
  while (1)
    {
      if (dir && c == 8)
        dir = 0;
      else if (!dir && c == 1)
        dir = 1;
      
      if (dir)
        c <<= 1;
      else
        c >>= 1;
      
      write_leds(c);
      
      sleep(1000);  // 1s
    }
}
```

`write_leds` and `sleep` are both implemented in Verilog.
`write_leds` is non-standard since it has signals to drive the LEDs.
