{ claspPkg }:
{ pkgs ? import <nixpkgs> { }, ... }:

let

  # The default Common Lisps.
  # To customize, use lispPackagesFor/lispWithPackages

  abclPkg = (pkgs.abcl.override {
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

  abcl = {
    pkg = abclPkg;
    loadFlags = "--load";
    evalFlags = "--eval";
    faslExt = "abcl";
    program = "abcl";
  };

  ecl = {
    pkg = pkgs.ecl;
    loadFlags = "--load";
    evalFlags = "--eval";
    faslExt = "fas";
    program = "ecl";
  };

  ccl = {
    pkg = pkgs.ccl;
    loadFlags = "--load";
    evalFlags = "--eval";
    faslExt = "lx64fsl";
    program = "ccl";
  };

  sbcl = {
    pkg = pkgs.sbcl;
    loadFlags = "--load";
    evalFlags = "--eval";
    faslExt = "fasl";
    program = "sbcl";
  };

  clisp = {
    pkg = pkgs.clisp;
    flags = "-E UTF-8";
    loadFlags = "-i";
    evalFlags = "-x";
    faslExt = "fas";
    program = "clisp";
  };

  clasp = {
    pkg = claspPkg;
    loadFlags = "--load";
    evalFlags = "--eval";
    faslExt = "fasp";
    program = "clasp";
  };

  # TODO(kasper): precompile asdf.lisp per implementation?
  defaultAsdf = pkgs.stdenv.mkDerivation rec {
    pname = "asdf";
    version = "3.3.5.11";
    src = pkgs.fetchzip {
      url = "https://gitlab.common-lisp.net/asdf/asdf/-/archive/${version}/asdf-${version}.tar.gz";
      hash = "sha256-SGzuSP2A168JafG4GYiTOCVLA1anhOB9uZThO8Speik";
    };
    installPhase = ''
      cp build/asdf.lisp $out
    '';
  };

in pkgs.callPackage (import ./nix-cl.nix {
  inherit abcl ecl ccl clasp clisp sbcl defaultAsdf;
}) {
  inherit pkgs;
}
