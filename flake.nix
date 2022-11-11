{

  description = "Utilities for packaging ASDF Common Lisp systems";

  inputs.nixpkgs.url = "nixpkgs";
  inputs.dev.url = "github:uthar/dev";

  outputs = { self, nixpkgs, dev, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      clasp = dev.outputs.packages.${system}.clasp;
      lisps = pkgs.callPackage (import ./. { claspPkg = clasp; }) {};
    in
    {
      packages = { inherit (lisps) sbcl clasp abcl ecl clisp ccl; };
      lib = { inherit (lisps) makeLisp; };
      devShells.default = pkgs.callPackage ./shell.nix {};
    });

}
