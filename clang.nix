let
  pkgs = import <nixpkgs> {};
in
  pkgs.mkShell {
    # The `llvm-hs-pretty` package targets LLVM 9.
    packages = [pkgs.clang_9];
  }
