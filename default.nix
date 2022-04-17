{ pkgs ? import <nixpkgs> { }, ... }:

pkgs.callPackage ./nix-cl.nix { }
