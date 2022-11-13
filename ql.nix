{ pkgs, lib, build-asdf-system, ... }:

let

  # FIXME: automatically add nativeLibs based on conditions signalled

  overrides = (self: super: {
    cl_plus_ssl = super.cl_plus_ssl.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.openssl ];
    });
    cl-cffi-gtk-glib = super.cl-cffi-gtk-glib.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.glib ];
    });
    cl-cffi-gtk-cairo = super.cl-cffi-gtk-cairo.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.cairo ];
    });
    cl-cairo2 = super.cl-cairo2.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.cairo ];
    });
    cl-cffi-gtk-gdk = super.cl-cffi-gtk-gdk.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.gtk3 ];
    });
    cl-cffi-gtk-gdk-pixbuf = super.cl-cffi-gtk-gdk-pixbuf.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.gdk-pixbuf ];
    });
    cl-cffi-gtk-pango = super.cl-cffi-gtk-pango.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.pango ];
    });
    cl-gobject-introspection = super.cl-gobject-introspection.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.glib pkgs.gobject-introspection ];
    });
    cl-mysql = super.cl-mysql.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.mysql-client ];
    });
    clsql-postgresql = super.clsql-postgresql.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.postgresql.lib ];
    });
    clsql-sqlite3 = super.clsql-sqlite3.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.sqlite ];
    });
    cl-webkit2 = super.cl-webkit2.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.webkitgtk ];
    });
    dbd-mysql = super.dbd-mysql.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.mysql-client ];
    });
    lla = super.lla.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.openblas ];
    });
    cffi-libffi = super.cffi-libffi.overrideLispAttrs (o: {
      nativeBuildInputs = [ pkgs.libffi ];
      nativeLibs = [ pkgs.libffi ];
    });
    cl-rabbit = super.cl-rabbit.overrideLispAttrs (o: {
      nativeBuildInputs = [ pkgs.rabbitmq-c ];
      nativeLibs = [ pkgs.rabbitmq-c ];
    });
    trivial-ssh-libssh2 = super.trivial-ssh-libssh2.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.libssh2 ];
    });
    sqlite = super.sqlite.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.sqlite ];
    });
    cl-libuv = super.cl-libuv.overrideLispAttrs (o: {
      nativeBuildInputs = [ pkgs.libuv ];
      nativeLibs = [ pkgs.libuv ];
    });
    cl-liballegro = super.cl-liballegro.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.allegro5 ];
    });
    cl-ode = super.cl-ode.overrideLispAttrs (o: {
      nativeLibs = let
        ode' = pkgs.ode.overrideAttrs (o: {
          configureFlags = [
            "--enable-shared"
            "--enable-double-precision"
          ];
        });
      in [ ode' ];
    });
    classimp = super.classimp.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.assimp ];
    });
    sdl2 = super.sdl2.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.SDL2 ];
    });
    lispbuilder-sdl-cffi = super.lispbuilder-sdl-cffi.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.SDL ];
    });
    cl-opengl = super.cl-opengl.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.libGL ];
    });
    cl-glu = super.cl-glu.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.libGLU ];
    });
    cl-glut = super.cl-glut.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.freeglut ];
    });
    cl-glfw = super.cl-glfw.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.glfw ];
    });
    cl-glfw-opengl-core = super.cl-glfw-opengl-core.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.libGL ];
    });
    cl-glfw3 = super.cl-glfw3.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.glfw ];
    });
    lev = super.lev.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.libev ];
    });
    cl-rdkafka = super.cl-rdkafka.overrideLispAttrs (o: {
      nativeBuildInputs = [ pkgs.rdkafka ];
      nativeLibs = [ pkgs.rdkafka ];
    });
    cl-async-ssl = super.cl-async-ssl.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.openssl ];
    });
    iolib = super.iolib.overrideLispAttrs (o: {
      nativeBuildInputs = [ pkgs.libfixposix ];
      nativeLibs = [ pkgs.libfixposix ];
      systems = [ "iolib" "iolib/os" "iolib/pathnames" ];
    });
    cl-ana_dot_hdf-cffi = super.cl-ana_dot_hdf-cffi.overrideLispAttrs (o: {
      nativeBuildInputs = [ pkgs.hdf5 ];
      nativeLibs = [ pkgs.hdf5 ];
      NIX_LDFLAGS = [ "-lhdf5" ];
    });
    gsll = super.gsll.overrideLispAttrs (o: {
      nativeBuildInputs = [ pkgs.gsl ];
      nativeLibs = [ pkgs.gsl ];
    });
    cl-libyaml = super.cl-libyaml.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.libyaml ];
    });
    cl-libxml2 = super.cl-libxml2.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.libxml2 ];
    });
    cl-readline = super.cl-readline.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.readline ];
    });
    md5 = super.md5.overrideLispAttrs (o: {
      lispLibs = [ super.flexi-streams ];
    });
  });

  qlpkgs =
    if builtins.pathExists ./imported.nix
    then pkgs.callPackage ./imported.nix { inherit build-asdf-system; }
    else {};

in qlpkgs.overrideScope' overrides
