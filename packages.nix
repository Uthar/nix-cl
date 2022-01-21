{ build-asdf-system, lisp, quicklispPackagesFor, fixupFor, pkgs, ... }:

let

  ql = quicklispPackagesFor { inherit lisp; fixup = fixupFor packages; };

  packages = rec {

  asdf = with builtins; let
    version = "3.3.5.3";
    asdf-build = (build-asdf-system {
      inherit version;
      pname = "asdf-build";

      src = builtins.fetchTarball {
        url = "https://gitlab.common-lisp.net/asdf/asdf/-/archive/3.3.5.3/asdf-3.3.5.3.tar.gz";
        sha256 = "0aw200awhg58smmbdmz80bayzmbm1a6547gv0wmc8yv89gjqldbv";
      };

      systems = [ "asdf" "uiop" ];

    }).overrideAttrs(o: {
      buildPhase = ''
        mkdir __fasls
        export CL_SOURCE_REGISTRY=$CL_SOURCE_REGISTRY:$(pwd)//
        export ASDF_OUTPUT_TRANSLATIONS="$(pwd):$(pwd)/__fasls:${storeDir}:${storeDir}"
        ${o.lisp} ${o.buildScript}
      '';
      installPhase = ''
        mkdir -pv $out
        rm -rf __fasls
        cp -r * $out
      '';
    });

  in build-asdf-system {
    inherit version;
    pname = "asdf";
    src = asdf-build.out;
    systems = [ "asdf" "uiop" ];
  };

  uiop = asdf;

  cffi = let
    jna = pkgs.fetchMavenArtifact {
      groupId = "net.java.dev.jna";
      artifactId = "jna";
      version = "5.9.0";
      sha256 = "0qbis8acv04fi902qzak1mbagqaxcsv2zyp7b8y4shs5nj0cgz7a";
    };
  in build-asdf-system {
    src =  builtins.fetchTarball {
      url = "http://beta.quicklisp.org/archive/cffi/2021-04-11/cffi_0.24.1.tgz";
      sha256 = "17ryim4xilb1rzxydfr7595dnhqkk02lmrbkqrkvi9091shi4cj3";
    };
    version = "0.24.1";
    pname = "cffi";
    lispLibs = with ql; [
      alexandria
      babel
      trivial-features
    ];
    javaLibs = [ jna ];
  };

  cl-unicode = let
    version = "0.1.6";
    # cl-unicode generates lisp source files during compilation.
    #
    # Normally this fails because of an attempt to write to
    # storeDir. A workaround is to run the compilation first with
    # CL_SOURCE_REGISTRY set to pwd, discard the fasls, then use $out
    # of that as the $src of the next compilation
    cl-unicode-build = (build-asdf-system {
      inherit version;
      pname = "cl-unicode-build";

      src =  builtins.fetchTarball {
        url = "https://github.com/edicl/cl-unicode/archive/refs/tags/v0.1.6.tar.gz";
        sha256 = "0ykx2s9lqfl74p1px0ik3l2izd1fc9jd1b4ra68s5x34rvjy0hza";
      };

      systems = [ "cl-unicode" ];

      lispLibs = with ql; [
        cl-ppcre
        flexi-streams
      ];
    }).overrideAttrs(o: {
      buildPhase = with builtins; ''
        mkdir __fasls
        export CL_SOURCE_REGISTRY=$CL_SOURCE_REGISTRY:$(pwd)//
        export ASDF_OUTPUT_TRANSLATIONS="$(pwd):$(pwd)/__fasls:${storeDir}:${storeDir}"
        ${o.lisp} ${o.buildScript}
      '';
      installPhase = ''
        mkdir -pv $out
        rm -rf __fasls
        cp -r * $out
      '';
    });

  in build-asdf-system {
    inherit version;
    pname = "cl-unicode";
    src = cl-unicode-build.out;
    lispLibs = [
      ql.cl-ppcre
      # flexi-streams is only needed for cl-unicode/build
    ];
  };

  quri = build-asdf-system {
    src = pkgs.stdenv.mkDerivation {
      pname = "patched";
      version = "source";
      src =  builtins.fetchTarball {
        url = "http://beta.quicklisp.org/archive/quri/2021-04-11/quri-20210411-git.tgz";
        sha256 = "1pkvpiwwhx2fcknr7x47h7036ypkg8xzsskqbl5z315ipfmi8s2m";
      };

      # fix build with ABCL
      buildPhase = ''
        sed -i "s,[#][.](asdf.*,#P\"$out/data/effective_tld_names.dat\")," src/etld.lisp
      '';
      installPhase = ''
        mkdir -pv $out
        cp -r * $out
      '';
    };
    version = "20210411";
    pname = "quri";
    lispLibs = with ql; [
      alexandria
      babel
      cl-utilities
      split-sequence
    ];
  };

  jzon = build-asdf-system {
    src = builtins.fetchTarball {
      url = "https://github.com/Zulu-Inuoe/jzon/archive/6b201d4208ac3f9721c461105b282c94139bed29.tar.gz";
      sha256 = "01d4a78pjb1amx5amdb966qwwk9vblysm1li94n3g26mxy5zc2k3";
    };
    version = "0.0.0-20210905-6b201d4208";
    pname = "jzon";
    lispLibs = [
      ql.closer-mop
    ];
    systems = [ "com.inuoe.jzon" ];
  };

  cl-notify = build-asdf-system {
    pname = "cl-notify";
    version = "20080904-138ca7038";
    src = builtins.fetchTarball {
      url = "https://repo.or.cz/cl-notify.git/snapshot/138ca703861f4a1fbccbed557f92cf4d213668a1.tar.gz";
      sha256 = "0k6ns6fzvjcbpsqgx85r4g5m25fvrdw9481i9vyabwym9q8bbqwx";
    };
    lispLibs = [
      cffi
    ];
    nativeLibs = [
      pkgs.libnotify
    ];
  };

  };

in packages
