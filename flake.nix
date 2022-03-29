{

  description = "Common Lisp package sets and `withPackages` wrappers";

  inputs = {
    nixpkgs.url = "nixpkgs/21.11";
  };

  outputs = { self, nixpkgs }: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    legacyPackages.${system} = import ./default.nix { inherit pkgs; };
  };

}
