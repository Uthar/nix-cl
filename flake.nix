{

  description = "Utilities for packaging ASDF Common Lisp systems";

  inputs.dev.url = "github:uthar/dev";

  outputs = { self, dev, flake-utils }:
    let
      systems = ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"];
      hydraSystems = ["x86_64-linux" "aarch64-linux"];
      nixpkgs = dev.inputs.nixpkgs;
    in (flake-utils.lib.eachSystem systems (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        devpkgs = dev.outputs.packages.${system};
        callWithLisps = x: pkgs.callPackage x { inherit (devpkgs) abcl clasp sbcl; };
        lisps = callWithLisps ./.;
      in {
        packages = { inherit (lisps) abcl ccl clasp clisp ecl sbcl; };
        devShells.default = callWithLisps ./shell.nix;
      })) // (flake-utils.lib.eachSystem hydraSystems (system:
        let
          nixpkgs = dev.inputs.nixpkgs;
          pkgs = nixpkgs.legacyPackages.${system};
          lib = nixpkgs.lib;
          jobsFor = lisp:
            pkgs.linkFarmFromDrvs
              "${lisp}-packages"
              (builtins.attrValues
                (lib.filterAttrs
                  (n: v: lib.isDerivation v)
                  self.packages.${system}.${lisp}.pkgs));
        in {
          hydraJobs = lib.genAttrs ["ecl"] jobsFor;
        }));
}
