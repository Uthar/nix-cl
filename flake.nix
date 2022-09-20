{

  description = "Utilities for packaging ASDF Common Lisp systems";

  inputs.nixpkgs.url = "nixpkgs";

  outputs = { self, nixpkgs }: {
    lib = import ./. { pkgs = nixpkgs.legacyPackages.x86_64-linux; };
  };

}
