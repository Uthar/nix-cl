{

  description = "Utilities for packaging ASDF Common Lisp systems";

  inputs.nixpkgs.url = "nixpkgs";

  outputs = { self, nixpkgs }:
    let pkgs = nixpkgs.legacyPackages.x86_64-linux; in
    {
      lib = import ./. { inherit pkgs; };
      devShells.x86_64-linux.default = import ./shell.nix { inherit pkgs; };
    };

}
