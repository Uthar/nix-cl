{

  description = "Utilities for packaging ASDF Common Lisp systems";

  inputs.nixpkgs.url = "nixpkgs/master";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      callWithLisps = x: pkgs.callPackage x { inherit (pkgs) abcl sbcl; clasp = pkgs.clasp-common-lisp; };
      lisps = callWithLisps ./.;
    in
    {
      packages = { inherit (lisps) abcl ccl clasp clisp ecl sbcl; };
      devShells.default = callWithLisps ./shell.nix;
    });

}
