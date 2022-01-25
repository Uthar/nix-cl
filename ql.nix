{ pkgs, build-asdf-system, flattenedDeps, fixup ? pkgs.lib.id, ... }:

with pkgs;
with lib;
with lib.lists;
with lib.strings;

let

  # FIXME: automatically add nativeLibs based on conditions signalled

  extras = {
    "cl+ssl" = pkg: {
      nativeLibs = [ openssl ];
    };
    cl-cffi-gtk-glib = pkg: {
      nativeLibs = [ glib ];
    };
    cl-cffi-gtk-cairo = pkg: {
      nativeLibs = [ cairo ];
    };
    cl-cffi-gtk-gdk = pkg: {
      nativeLibs = [ gtk3 ];
    };
    cl-cffi-gtk-gdk-pixbuf = pkg: {
      nativeLibs = [ gdk-pixbuf ];
    };
    cl-cffi-gtk-pango = pkg: {
      nativeLibs = [ pango ];
    };
    cl-gobject-introspection = pkg: {
      nativeLibs = [ glib gobject-introspection ];
    };
    cl-mysql = pkg: {
      nativeLibs = [ mysql-client ];
    };
    clsql-postgresql = pkg: {
      nativeLibs = [ postgresql.lib ];
    };
    clsql-sqlite3 = pkg: {
      nativeLibs = [ sqlite ];
    };
    cl-webkit2 = pkg: {
      nativeLibs = [ webkitgtk ];
    };
    dbd-mysql = pkg: {
      nativeLibs = [ mysql-client ];
    };
    lla = pkg: {
      nativeLibs = [ openblas ];
    };
    cffi-libffi = pkg: {
      nativeBuildInputs = [ libffi ];
      nativeLibs = [ libffi ];
    };
    cl-rabbit = pkg: {
      nativeBuildInputs = [ rabbitmq-c ];
      nativeLibs = [ rabbitmq-c ];
    };
    trivial-ssh-libssh2 = pkg: {
      nativeLibs = [ libssh2 ];
    };
    sqlite = pkg: {
      nativeLibs = [ sqlite ];
    };
    cl-libuv = pkg: {
      nativeBuildInputs = [ libuv ];
      nativeLibs = [ libuv ];
    };
    cl-liballegro = pkg: {
      # build doesnt fail without this, but fails on runtime
      # weird...
      nativeLibs = [ allegro5 ];
    };
    classimp = pkg: {
      nativeLibs = [ assimp ];
    };
    sdl2 = pkg: {
      nativeLibs = [ SDL2 ];
    };
    lispbuilder-sdl-cffi = pkg: {
      nativeLibs = [ SDL ];
    };
    cl-opengl = pkg: {
      nativeLibs = [ libGL ];
    };
    cl-glu = pkg: {
      nativeLibs = [ libGLU ];
    };
    cl-glut = pkg: {
      nativeLibs = [ freeglut ];
    };
    lev = pkg: {
      nativeLibs = [ libev ];
    };
    cl-rdkafka = pkg: {
      nativeBuildInputs = [ rdkafka ];
      nativeLibs = [ rdkafka ];
    };
    cl-async-ssl = pkg: {
      nativeLibs = [ openssl ];
    };
    osicat = pkg: {
      LD_LIBRARY_PATH = "${pkg}/posix/";
    };
    iolib = pkg: {
      nativeBuildInputs = [ libfixposix ];
      nativeLibs = [ libfixposix ];
      systems = [ "iolib" "iolib/os" "iolib/pathnames" ];
    };
  };

  # NOTE:
  # You might need https://github.com/NixOS/nix/commit/cd44c0af71ace2eb8056c2b26b9249a5aa102b41
  # and a couple gigs of ram, as some derivations are HUGE
  #
  # So, if you get stack overflow with nix <=2.3, try again with `ulimit -s 65536`
  #
  # FIXME: mark those broken on nix.lisp level for speed
  broken = [
    # broken packaging in quicklisp: clml.data.r-datasets depends on
    # a non-existing system clml.data.r-datasets-package
    # FIXME: patch the clml.data.r-datasets-package system manually in
    # systems.txt before running nix.lisp
    # (n: v: hasPrefix "clml" n)

    # Broken upstream: see https://gitlab.common-lisp.net/antik/antik/-/issues/4
    # (n: v: hasPrefix "antik" n)
    # (n: v: any (dep: hasPrefix "antik" dep.pname) (flattenedDeps v.lispLibs))

    # FIXME: for cl-unicode (and other cases where the build process
    # generates new source files), try to rebuild any failing library
    # again with source translations into pwd, then rebuild normallny
    # with the previous one as source
  ];

  qlpkgs =
    if builtins.pathExists ./from-quicklisp.nix
    then filterAttrs (n: v: all (check: !(check n v)) broken) (import ./from-quicklisp.nix { inherit pkgs; })
    else {};

  build = pkg:
    let
      withLibs = pkg // { lispLibs = map build pkg.lispLibs; };
      builtPkg = build-asdf-system withLibs;
      withExtras = withLibs //
                   (optionalAttrs
                     (hasAttr pkg.pname extras)
                     (extras.${pkg.pname} builtPkg));
      fixedUp = fixup withExtras;
    in build-asdf-system fixedUp;

in mapAttrs (n: v: build v) qlpkgs
