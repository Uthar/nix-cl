{ build-asdf-system, fixDuplicateAsds, lisp, quicklispPackagesFor, fixupFor, pkgs, ... }:

let

  inherit (pkgs.lib)
    head
    makeLibraryPath
    makeSearchPath
    setAttr
    hasAttr
    optionals
  ;

  build-with-fix-duplicate-asds = args:
    head
      (fixDuplicateAsds
        [(build-asdf-system args)]
        (ql // packages));

  ql = quicklispPackagesFor { inherit lisp; fixup = fixupFor packages; };


  # Used by builds that would otherwise attempt to write into storeDir.
  #
  # Will run build two times, keeping all files created during the
  # first run, exept the FASL's. Then using that directory tree as the
  # source of the second run.
  #
  # E.g. cl-unicode creating .txt files during compilation
  build-with-compile-into-pwd = args:
    let
      build = (build-with-fix-duplicate-asds (args // { version = args.version + "-build"; }))
        .overrideAttrs(o: {
          buildPhase = with builtins; ''
            mkdir __fasls
            export LD_LIBRARY_PATH=${makeLibraryPath o.nativeLibs}:$LD_LIBRARY_PATH
            export CLASSPATH=${makeSearchPath "share/java/*" o.javaLibs}:$CLASSPATH
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
    in build-with-fix-duplicate-asds (args // {
      # Patches are already applied in `build`
      patches = [];
      src = build;
    });

  packages = rec {

  asdf = build-with-compile-into-pwd {
    pname = "asdf";
    version = "3.3.5.3";
    src = builtins.fetchTarball {
      url = "https://gitlab.common-lisp.net/asdf/asdf/-/archive/3.3.5.3/asdf-3.3.5.3.tar.gz";
      sha256 = "0aw200awhg58smmbdmz80bayzmbm1a6547gv0wmc8yv89gjqldbv";
    };
    systems = [ "asdf" "uiop" ];
  };

  uiop = build-with-compile-into-pwd {
    inherit (asdf) version src systems;
    pname = "uiop";
  };

  cffi = let
    jna = pkgs.fetchMavenArtifact {
      groupId = "net.java.dev.jna";
      artifactId = "jna";
      version = "5.9.0";
      sha256 = "0qbis8acv04fi902qzak1mbagqaxcsv2zyp7b8y4shs5nj0cgz7a";
    };
  in build-with-fix-duplicate-asds {
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

  cl-unicode = build-with-compile-into-pwd {
    pname = "cl-unicode";
    version = "0.1.6";
    src =  builtins.fetchTarball {
      url = "https://github.com/edicl/cl-unicode/archive/refs/tags/v0.1.6.tar.gz";
      sha256 = "0ykx2s9lqfl74p1px0ik3l2izd1fc9jd1b4ra68s5x34rvjy0hza";
    };
    systems = [ "cl-unicode" ];
    lispLibs = with ql; [
      cl-ppcre
      flexi-streams
    ];
  };

  quri = build-with-fix-duplicate-asds {
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

  jzon = build-with-fix-duplicate-asds {
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

  cl-notify = build-with-fix-duplicate-asds {
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

  cl-fuse = build-with-compile-into-pwd {
    inherit (ql.cl-fuse) pname version src lispLibs;
    nativeBuildInputs = [ pkgs.fuse ];
    nativeLibs = [ pkgs.fuse ];
  };

  cl-containers = build-with-fix-duplicate-asds {
    inherit (ql.cl-containers) pname version src;
    lispLibs = ql.cl-containers.lispLibs ++ [ ql.moptilities ];
    systems = [ "cl-containers" "cl-containers/with-moptilities" ];
  };

  swank = build-with-compile-into-pwd {
    inherit (ql.swank) pname version src lispLibs;
    patches = [ ./patches/swank-pure-paths.patch ];
    postConfigure = ''
      substituteAllInPlace swank-loader.lisp
    '';
  };

  clx-truetype = build-with-fix-duplicate-asds {
    pname = "clx-truetype";
    version = "20160825-git";
    src = builtins.fetchTarball {
      url = "http://beta.quicklisp.org/archive/clx-truetype/2016-08-25/clx-truetype-20160825-git.tgz";
      sha256 = "079hyp92cjkdfn6bhkxsrwnibiqbz4y4af6nl31lzw6nm91j5j37";
    };
    lispLibs = with ql; [
      alexandria bordeaux-threads cl-aa cl-fad cl-paths cl-paths-ttf
      cl-store cl-vectors clx trivial-features zpb-ttf
    ];
  };

  mgl = build-with-fix-duplicate-asds {
    pname = "mgl";
    version = "2021-10-07";
    src = builtins.fetchTarball {
      url = "https://github.com/melisgl/mgl/archive/e697791a9bcad3b6e7b3845246a2aa55238cfef7.tar.gz";
      sha256 = "09sf7nq7nmf9q7bh3a5ygl2i2n0nhrx5fk2kv5ili0ckv7g9x72s";
    };
    lispLibs = with ql; [
      alexandria closer-mop array-operations lla cl-reexport mgl-pax
      named-readtables pythonic-string-reader
    ] ++ [ mgl-mat ];
    systems = [ "mgl" "mgl/test" ];
  };

  mgl-mat = build-with-fix-duplicate-asds {
    pname = "mgl-mat";
    version = "2021-10-11";
    src = builtins.fetchTarball {
      url = "https://github.com/melisgl/mgl-mat/archive/3710858bc876b1b86e50f1db2abe719e92d810e7.tar.gz";
      sha256 = "1aa2382mi55rp8pd31dz4d94yhfzh30vkggcvmvdfrr4ngffj0dx";
    };
    lispLibs = with ql; [
      alexandria bordeaux-threads cffi cffi-grovel cl-cuda
      flexi-streams ieee-floats lla mgl-pax static-vectors
      trivial-garbage cl-fad
    ];
    systems = [ "mgl-mat" "mgl-mat/test" ];
  };

  nyxt-gtk = build-with-fix-duplicate-asds {
    inherit (ql.nyxt) pname lisp;
    version = "2.2.4";

    lispLibs = ql.nyxt.lispLibs ++ (with ql; [
      cl-cffi-gtk cl-webkit2 mk-string-metrics
    ]);

    src = builtins.fetchTarball {
      url = "https://github.com/atlas-engineer/nyxt/archive/2.2.4.tar.gz";
      sha256 = "12l7ir3q29v06jx0zng5cvlbmap7p709ka3ik6x29lw334qshm9b";
    };

    buildInputs = [
      pkgs.makeWrapper

      # needed for GSETTINGS_SCHEMAS_PATH
      pkgs.gsettings-desktop-schemas pkgs.glib pkgs.gtk3

      # needed for XDG_ICON_DIRS
      pkgs.gnome.adwaita-icon-theme
    ];

    buildScript = pkgs.writeText "build-nyxt.lisp" ''
      (require :asdf)
      (asdf:load-system :nyxt/gtk-application)
      (sb-ext:save-lisp-and-die "nyxt" :executable t
                                       #+sb-core-compression :compression
                                       #+sb-core-compression t
                                       :toplevel #'nyxt:entry-point)
    '';

    installPhase = ql.nyxt.installPhase + ''
      rm -v $out/nyxt
      mkdir -p $out/bin
      cp -v nyxt $out/bin
      wrapProgram $out/bin/nyxt \
        --prefix LD_LIBRARY_PATH : $LD_LIBRARY_PATH \
        --prefix XDG_DATA_DIRS : $XDG_ICON_DIRS \
        --prefix XDG_DATA_DIRS : $GSETTINGS_SCHEMAS_PATH \
        --set WEBKIT_FORCE_SANDBOX 0 \
        --prefix GIO_EXTRA_MODULES ":" ${pkgs.dconf.lib}/lib/gio/modules/ \
        --prefix GIO_EXTRA_MODULES ":" ${pkgs.glib-networking}/lib/gio/modules/
    '';
  };

  nyxt = nyxt-gtk;

  s-sql_slash_tests = build-with-fix-duplicate-asds {
    inherit (ql.s-sql_slash_tests) pname version src systems asds;
    lispLibs = ql.s-sql_slash_tests.lispLibs ++ [
      ql.cl-postgres_slash_tests
    ];
  };

  simple-date_slash_postgres-glue = build-with-fix-duplicate-asds {
    inherit (ql.simple-date_slash_postgres-glue) pname version src systems asds;
    lispLibs = ql.simple-date_slash_postgres-glue.lispLibs ++ [
      ql.cl-postgres_slash_tests
    ];
  };

  };

in packages
