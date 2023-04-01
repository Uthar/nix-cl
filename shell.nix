{ pkgs, lib, stdenv, ccl, abcl, clisp, clasp, sbcl, ecl, ... }@args:
let nix-cl = import ./. args;
in pkgs.mkShell {
  nativeBuildInputs = [
    (nix-cl.sbcl.withPackages
      (ps: with ps; [ alexandria str dexador cl-ppcre sqlite arrow-macros jzon lparallel ]))
  ];
}
