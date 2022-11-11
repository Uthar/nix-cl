{ pkgs, lib, stdenv, abcl, ccl, clisp, clasp, ecl, sbcl, ... }:

let
 
  abclArgs = {
    pkg = abcl;
    faslExt = "abcl";
  };

  eclArgs = {
    pkg = ecl;
    faslExt = "fas";
  };

  cclArgs = {
    pkg = ccl;
    faslExt = "lx64fsl";
  };

  sbclArgs = {
    pkg = sbcl;
    faslExt = "fasl";
  };

  clispArgs = {
    pkg = clisp;
    flags = "-E UTF-8";
    loadFlags = "-i";
    evalFlags = "-x";
    faslExt = "fas";
  };

  claspArgs = {
    pkg = clasp;
    faslExt = "fasp";
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

in pkgs.callPackage ./nix-cl.nix {
  inherit abclArgs eclArgs cclArgs claspArgs clispArgs sbclArgs defaultAsdf;
}
 
