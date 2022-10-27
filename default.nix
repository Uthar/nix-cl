{ clasp }:
{ pkgs ? import <nixpkgs> { }, ... }:

let

  # The default Common Lisps.
  # To customize, use lispPackagesFor/lispWithPackages

  abcl = (pkgs.abcl.override {
    jdk = pkgs.jdk17;
    jre = pkgs.jdk17;
  }).overrideAttrs (o: {
    # Pull in fix for https://github.com/armedbear/abcl/issues/473
    version = "1.8.1-dev";
    src = pkgs.fetchFromGitHub {
      repo = "abcl";
      owner = "uthar";
      rev = "aba4f90ca75935886512dba79dd3565bf58cac76";
      hash = "sha256-k1apwbsGlKjbrRfhbSSqlM4xxxzvW0PB3DvuiTqEy+U=";
    };
    # Fix for https://github.com/armedbear/abcl/issues/484
    installPhase = pkgs.lib.replaceStrings
      ["${pkgs.jdk17}/bin/java"]
      ["${pkgs.jdk17}/bin/java --add-opens=java.base/java.util.jar=ALL-UNNAMED"]
      o.installPhase;
  });

  ecl = pkgs.ecl;

  ccl = pkgs.ccl;

  sbcl = pkgs.sbcl;

in pkgs.callPackage (import ./nix-cl.nix {
  inherit abcl ecl ccl clasp sbcl;
}) {
  inherit pkgs;
}
