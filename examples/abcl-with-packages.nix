# To run this example from a nix repl, run:
#  $ nix repl
#  nix-repl> abcl-with-packages = import ./abcl-with-packages.nix
#  nix-repl> :b abcl-with-packages (p: [ p.cffi ])
#
# The import returns a function, which you can call to get access to
# thousands of libraries, like, cffi. This works in ABCL by closing
# over the JNA dependency:
#
#  nix-repl> awp = abcl-with-packages (p: [ p.cffi ])
#  nix-repl> awp.CLASSPATH
#  nix-repl> cffi = builtins.head (awp.lispLibs)
#  nix-repl> cffi.javaLibs

let

  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/21.11.tar.gz";
    sha256 = "162dywda2dvfj1248afxc45kcrg83appjd0nmdb541hl7rnncf02";
  }) {};

  abcl = "${pkgs.abcl}/bin/abcl --batch --load";

  nix-cl = import ../. {};

  abcl-with-packages = nix-cl.lispWithPackages abcl;

in abcl-with-packages
