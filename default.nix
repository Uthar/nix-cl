{ pkgs, lib, stdenv, abcl, ccl, clisp, clasp, ecl, sbcl, ... }:

let

  mkSpec = spec@{
    pkg
    , program ? pkg.pname
    , flags ? ""
    , asdf ? asdf_3_3_6
  }: { inherit pkg program flags asdf; };

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
  };

  eclSpec = mkSpec {
    pkg = ecl;
  };

  cclSpec = mkSpec {
    pkg = ccl;
  };

  sbclSpec = mkSpec {
    pkg = sbcl;
    flags = "--dynamic-space-size 4GiB";
  };

  clispSpec = mkSpec {
    pkg = clisp;
    flags = "-E UTF-8";
  };

  claspSpec = mkSpec {
    pkg = clasp;
  };

in pkgs.callPackage ./nix-cl.nix {
  inherit abclSpec eclSpec cclSpec claspSpec clispSpec sbclSpec;
}
 
