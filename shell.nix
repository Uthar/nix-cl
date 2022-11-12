{ pkgs, lib, stdenv, ccl, abcl, clisp, clasp, sbcl, ecl, ... }@args:
let nix-cl = import ./. args;
in pkgs.mkShell {
  nativeBuildInputs = [
    (nix-cl.sbclWithPackages
      (ps: with ps; [
        alexandria
        str
        dexador
        cl-ppcre
        sqlite
        arrow-macros
        jzon
        cl-tar
      ]))
  ];
}
