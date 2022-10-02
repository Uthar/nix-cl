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

  clasp = let
    flakeUrl = "https://fossil.galkowski.xyz/nix-clasp/tarball/5b6a1e05461eb994/nix-clasp.tar.gz";
    nix-clasp = builtins.getFlake flakeUrl;
  in nix-clasp.defaultPackage.${builtins.currentSystem};

  sbcl = pkgs.sbcl;

  smokegen = pkgs.stdenv.mkDerivation rec {
    pname = "smokegen";
    version = "v4.14.3";
    src = pkgs.fetchzip {
      url = "https://invent.kde.org/unmaintained/${pname}/-/archive/${version}/${pname}-${version}.tar.gz";
      hash = "sha256-finsoruPeJZLawIjNUJ25Pq54eaCByfALVraNQJPk7c=";
    };
    buildInputs = [ pkgs.cmake pkgs.qt4 ];
    buildPhase = ''
      cmake .
    '';
  };

  smokeqt = pkgs.stdenv.mkDerivation rec {
    pname = "smokeqt";
    version = "v4.14.3";
    src = pkgs.fetchzip {
      url = "https://invent.kde.org/unmaintained/${pname}/-/archive/${version}/${pname}-${version}.tar.gz";
      hash = "sha256-8FiEGF8gduVw5I/bi2wExGUWmjIjYEhWpjpXKJGBNMg=";
    };
    cmakeFlags = [
      "-DCMAKE_CXX_STANDARD=98"
    ];
    buildInputs = [ pkgs.cmake pkgs.qt4 smokegen ];
  };


in pkgs.callPackage ./nix-cl.nix {
  pkgs = pkgs // { inherit smokegen smokeqt; };
  inherit abcl ecl ccl clasp sbcl;
}
