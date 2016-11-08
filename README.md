# NetList compiler

Compiles a netlist to gasm for 64bits Linux machines.

## How to use
Type `make` generates the compiler `compile-net` from Haskell. Then
you can use the script `compile.sh` to generate a program from a netlist.
It expects a path to the netlist as first argument. It also requires for
`helper.c` and `compile-net` to be in the same directory. The generated
program will have the same basename (minus eventually the `.net`) as the
netlist, with the `out` extension.

The generated program expects two arguments (they are NOT optional). The first
one is a path to a file, which will be loaded as the ROM. The second one is the
size of the RAM in bytes.

When they are inputs variables, their values must be entered in hexadecimal,
separated by returns, in the order the variables where in the netlist. The
output variables will have their values written, one per line, in hexadecimal,
in the order they were in the netlist.

## NetList syntax
The netlist file must begin with the `INPUT` token, followed by the names
of the input variables, separated by commas. Then it expects the `OUTPUT`
token, followed by the list of output variables.

Afterward there must be the `VAR` token, followed by the list of all the
variables used in the program. It is possible to specify here the size of the
variable by postponing its name by `: size`. Variables are of size 1 by
default. Variables have a maximum size of 64.

Then comes the IN token, followed by a list of statements, one per line. A
statement describes how to compute a variable. A statement has the form
`var = command arguments`. There must one and only one statement for every
variable. Because every arguments must be computed before var is computed 
(unless otherwise stated in the following list), there cannot be circular
dependencies.

Here is the list of accepted commands :
 - If there simply is the name of a variable or a constant after the `=`,
   the computed variable is set to its value. The two variables must have the
   same size and the constant must be small enough to be stored on the
   variable.
 - `AND|OR|XOR|NAND v1 v2` : computes a binary operator between two
   variables. They must be of the same size as the result.
 - `NOT v` : computes the bit by bit negation of `v`, which must be of
   the same size as the result.
 - `REG v` : computes the value of the variable `v` at the end of the
   previous pass. `v` must be of the same size as the result.
 - `MUX v1 v2 s` : `s` must be of size 1. Returns `v1` if `s`
   is 1 and `v2` otherwise. `v1` and `v2` must be of the same size
   as the result.
 - `CONCAT v1 v2` : The size of the result must be the sum of the sizes of
   `v1` and `v2`. `v1` will be the the least significant bits of
   the results and `v2` the most significant ones.
 - `SLICE i1 i2 v` : `i1` and `i2` must be integers, and the
   result must be of size `i2 - i1 + 1`. It returns a slice of `v`,
   in little endian order, ie `SLICE 0 1 v` returns the two least
   significant bits of `v`.
 - `SELECT i v` : the result must be of size 1. Selects the `i`-eme bit of
   `v`, starting with the least significant one.
 - `ROM as ws ra` : `as` and `ws` are integers. `as` is the address size, thus
   the size of the variable `ra`. `ws` is the size of the word read, thus the
   size of the result. To know how the rom is loaded, refer to the *How to use*
   section. Access is in little endian order : `ROM 2 4 0` will give the 4
   least significat bits of `ROM 2 8 0`. Furthermore, `ws` must always be a
   power of two. The address is in *word size* unit.
 - `RAM as ws ra e wa d` : `ws` is the word size, so the size of the result and
   of `d`. `as` is the address size, thus the size of `ra` and `wa`. Finally
   `e` is a bit. The first three arguments work exactly as with `ROM`. The
   three last ones allow to write to the RAM. `e` enables the writing if set
   to 1. `wa` is the write address and `d` is the data written. Please note
   that the write action only takes effect at the end of the pass. Because of
   that, the three last arguments are not dependencies of the command. The
   addresses are in *word size* unit.

The RAM and the registers are initialised to 0.

For examples of valid netlists, see the folder `test`.

## Dependencies
To compile `compile-net`, the following utilities are needed :
 - [make](https://www.gnu.org/software/make/)
 - [ghc](https://www.haskell.org/ghc/)
 - [parsec](https://hackage.haskell.org/package/parsec)
 - [fgl](https://hackage.haskell.org/package/fgl)

To use it, you will need :
 - [gcc](https://gcc.gnu.org/)
 - [bash](https://www.gnu.org/software/bash/) or any shell-compliant program

## Portability
While the compiler can be used on any system with an haskell installation, the
generated program is Linux specific : indeed, it relies on the Linux calling
convention. This means it may work on other UNIXes like \*BSD or Mac OS X but
not on Windows.

## Problems during the implementation
The main difficulty was getting the calling convention right (sending the right
arguments, saving the registers, placing `%rsp`, getting the return ...), which
is why only one calling convention is supported.

The other hurdle to overcome was getting clear on big-endian/little-endian when
reading or writing the memory (both ROM and RAM), and proposing a consistent
implementation.

