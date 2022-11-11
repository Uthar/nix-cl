{

  description = "Utilities for packaging ASDF Common Lisp systems";

  inputs.dev.url = "github:uthar/dev";

  outputs = { self, dev }:
    let
      nixpkgs = dev.inputs.nixpkgs;
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      clasp = dev.outputs.packages.x86_64-linux.clasp;
      abcl = dev.outputs.packages.x86_64-linux.abcl;
      sbcl = dev.outputs.packages.x86_64-linux.sbcl;
      callWithLisps = x: pkgs.callPackage x { inherit abcl clasp sbcl; };
    in
    {
      lib = callWithLisps  ./.;
      devShells.x86_64-linux.default = callWithLisps ./shell.nix;
    };

}
