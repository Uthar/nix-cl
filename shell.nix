let
  pkgs = import <nixpkgs> {};
  nix-cl = import ./. { inherit pkgs; };
in

pkgs.mkShell {
  buildInputs = [
    (nix-cl.sbclWithPackages (ps: with ps; [ str dexador cl-ppcre sqlite ]))
  ];
}
