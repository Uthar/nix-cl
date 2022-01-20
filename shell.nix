let
  pkgs = import <nixpkgs> {};
  nix-cl = import ./nix-cl.nix { inherit (pkgs) pkgs lib stdenv; };
in

pkgs.mkShell {
  buildInputs = [
    (nix-cl.sbclWithPackages (ps: with ps; [ str dexador cl-ppcre cl-sqlite ]))
  ];
}
