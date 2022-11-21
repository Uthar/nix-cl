{ pkgs, lib, stdenv, abcl, ccl, clisp, clasp, ecl, sbcl, ... }:

let

  mkSpec = spec@{
    pkg
    , faslExt
    , program ? pkg.pname
    , flags ? ""
    , loadFlags ? "--load"
    , evalFlags ? "--eval"
    , asdf ? asdf_3_3_6
  }: { inherit pkg faslExt program flags loadFlags evalFlags asdf; };

  asdf_3_3_6 = pkgs.stdenv.mkDerivation rec {
    pname = "asdf";
    version = "3.3.6";
    src = pkgs.fetchzip {
      url = "https://gitlab.common-lisp.net/asdf/asdf/-/archive/${version}/asdf-${version}.tar.gz";
      hash = "sha256-GCmGUMLniPakjyL/D/aEI93Y6bBxjdR+zxXdSgc9NWo=";
    };
    installPhase = ''
      cp build/asdf.lisp $out
    '';
  };

  abclSpec = mkSpec {
    pkg = abcl;
    faslExt = "abcl";
  };

  eclSpec = mkSpec {
    pkg = ecl;
    faslExt = "fas";
  };

  cclSpec = mkSpec {
    pkg = ccl;
    faslExt = "lx64fsl";
  };

  sbclSpec = mkSpec {
    pkg = sbcl;
    faslExt = "fasl";
  };

  clispSpec = mkSpec {
    pkg = clisp;
    flags = "-E UTF-8";
    loadFlags = "-i";
    evalFlags = "-x";
    faslExt = "fas";
  };

  claspSpec = mkSpec {
    pkg = clasp;
    faslExt = "fasp";
  };

in pkgs.callPackage ./nix-cl.nix {
  inherit abclSpec eclSpec cclSpec claspSpec clispSpec sbclSpec;
}
 
