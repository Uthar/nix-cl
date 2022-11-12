{ pkgs, lib, stdenv, abcl, ccl, clisp, clasp, ecl, sbcl, ... }:

let

  mkSpec = spec@{
    pkg
    , faslExt
    , program ? pkg.pname
    , flags ? ""
    , loadFlags ? "--load"
    , evalFlags ? "--eval"
    , asdf ? asdf_3_3_5_11
  }: { inherit pkg faslExt program flags loadFlags evalFlags asdf; };

  asdf_3_3_5_11 = pkgs.stdenv.mkDerivation rec {
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
 
