# CI

## Build all sbcl packages

nix build \
  --impure\
  --expr \
  "with builtins.getFlake \"$(pwd)\"; \
   inputs.nixpkgs.legacyPackages.\${builtins.currentSystem}.linkFarmFromDrvs \
   \"sbclPackages\" \
   (builtins.attrValues \
     (inputs.nixpkgs.klib.filterAttrs \
       (n: v: builtins.isAttrs v && v.meta.broken == false) \
       lib.sbcl.pkgs))"
