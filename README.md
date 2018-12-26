# PL0_Compiler

This is a simple pascal implementation of PL0 compiler. It can compile PL0 program into intermediate code and run them in its runtime.

## Usage

### Environment

Make sure you've install the Free Pascal Compiler to compile this compiler and config PATH to make it possible for us to use command `fpc` everywhere.

### Compile & Run

Use command below to compile the compiler and use it to compile the example PL0 program:

```bash
fpc compiler.pas
./compiler
> source.pl0
> os
> oi
> or
```

(`>` specify commands input while compiler's running.)