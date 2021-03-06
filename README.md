# coy

Coy is a small programming language with algebraic data types and pattern
matching; it has a Rust-like syntax and compiles to [LLVM IR].

[LLVM IR]: https://llvm.org/docs/LangRef.html

## Example

An extensive example is available at [`examples/lux.coy`](examples/lux.coy),
containing a partial implementation of [Ray Tracing in One Weekend][Shirley].

This example shows off most of Coy's features; it has no arrays, first-class
functions, mutable variables, references, etc.

[Shirley]: https://raytracing.github.io/books/RayTracingInOneWeekend.html

## Installation

Coy uses [`stack`] to manage its build process.

[`stack`]: https://www.haskellstack.org

## Usage

Coy compiles its input to LLVM IR, relying on Clang for code generation. It
targets LLVM 9, because that's what [`llvm-hs-pretty`] supports. The provided
[`clang.nix`](clang.nix) Nix expression is a convenient way of setting up an
environment with a compatible Clang version through [`nix-shell`].

An an example, let's compile [`examples/lux.coy`](examples/lux.coy).

First, build and run Coy:

    $ stack run -- --output=lux.ll examples/lux.coy

This produces a `lux.ll` file that contains the LLVM IR generated by Coy.

Now, open a shell that contains Clang:

    $ nix-shell clang.nix

… compile the LLVM IR generated by Coy:

    [nix-shell]$ clang -O3 -o lux lux.ll

… and run the generated executable:

    [nix-shell]$ ./lux > lux.ppm

This produces an image in [plain PPM format] that you can open in your favorite
image viewer.

[`llvm-hs-pretty`]: https://hackage.haskell.org/package/llvm-hs-pretty
[`nix-shell`]: https://nixos.org/manual/nix/stable/command-ref/nix-shell.html
[plain PPM format]: http://netpbm.sourceforge.net/doc/ppm.html
