{ pkgs ? import <nixpkgs> { } }:
let nix-cl = import ./. { inherit pkgs; };
in pkgs.mkShell {
  nativeBuildInputs = [
    (nix-cl.sbclWithPackages
      (ps: with ps; [ alexandria str dexador cl-ppcre sqlite arrow-macros jzon ]))
  ];
}
