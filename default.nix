let
  pkgs = import <nixpkgs> {};
in

pkgs.callPackage ./nix-cl.nix {}
