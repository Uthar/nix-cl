{

  description = "Utilities for packaging ASDF Common Lisp systems";

  inputs.dev.url = "github:uthar/dev";

  outputs = { self, dev, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      nixpkgs = dev.inputs.nixpkgs;
      pkgs = nixpkgs.legacyPackages.${system};
      devpkgs = dev.outputs.packages.${system};
      callWithLisps = x: pkgs.callPackage x { inherit (devpkgs) abcl clasp sbcl; };
      lisps = callWithLisps ./.;
    in
    {
      packages = { inherit (lisps) abcl ccl clasp clisp ecl sbcl; };
      devShells.default = callWithLisps ./shell.nix;
    });

}
