{ pkgs, lib, build-asdf-system, ... }:

let

  # FIXME: automatically add nativeLibs based on conditions signalled

  overrides = (super: self: {
    "cl+ssl" = self."cl+ssl".overrideLispAttrs (o: {
      nativeLibs = [ pkgs.openssl ];
    });
    cl-cffi-gtk-glib = self.cl-cffi-gtk-glib.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.glib ];
    });
    cl-cffi-gtk-cairo = self.cl-cffi-gtk-cairo.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.cairo ];
    });
    cl-cairo2 = self.cl-cairo2.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.cairo ];
    });
    cl-cffi-gtk-gdk = self.cl-cffi-gtk-gdk.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.gtk3 ];
    });
    cl-cffi-gtk-gdk-pixbuf = self.cl-cffi-gtk-gdk-pixbuf.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.gdk-pixbuf ];
    });
    cl-cffi-gtk-pango = self.cl-cffi-gtk-pango.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.pango ];
    });
    cl-gobject-introspection = self.cl-gobject-introspection.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.glib pkgs.gobject-introspection ];
    });
    cl-mysql = self.cl-mysql.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.mysql-client ];
    });
    clsql-postgresql = self.clsql-postgresql.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.postgresql.lib ];
    });
    clsql-sqlite3 = self.clsql-sqlite3.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.sqlite ];
    });
    cl-webkit2 = self.cl-webkit2.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.webkitgtk ];
    });
    dbd-mysql = self.dbd-mysql.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.mysql-client ];
    });
    lla = self.lla.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.openblas ];
    });
    cffi-libffi = self.cffi-libffi.overrideLispAttrs (o: {
      nativeBuildInputs = [ pkgs.libffi ];
      nativeLibs = [ pkgs.libffi ];
    });
    cl-rabbit = self.cl-rabbit.overrideLispAttrs (o: {
      nativeBuildInputs = [ pkgs.rabbitmq-c ];
      nativeLibs = [ pkgs.rabbitmq-c ];
    });
    trivial-ssh-libssh2 = self.trivial-ssh-libssh2.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.libssh2 ];
    });
    sqlite = self.sqlite.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.sqlite ];
    });
    cl-libuv = self.cl-libuv.overrideLispAttrs (o: {
      nativeBuildInputs = [ pkgs.libuv ];
      nativeLibs = [ pkgs.libuv ];
    });
    cl-liballegro = self.cl-liballegro.overrideLispAttrs (o: {
      # build doesnt fail without this, but fails on runtime
      # weird...
      nativeLibs = [ pkgs.allegro5 ];
    });
    cl-ode = self.cl-ode.overrideLispAttrs (o: {
      nativeLibs = let
        ode' = pkgs.ode.overrideAttrs (o: {
          configureFlags = [
            "--enable-shared"
            "--enable-double-precision"
          ];
        });
      in [ ode' ];
    });
    classimp = self.classimp.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.assimp ];
    });
    sdl2 = self.sdl2.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.SDL2 ];
    });
    lispbuilder-sdl-cffi = self.lispbuilder-sdl-cffi.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.SDL ];
    });
    cl-opengl = self.cl-opengl.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.libGL ];
    });
    cl-glu = self.cl-glu.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.libGLU ];
    });
    cl-glut = self.cl-glut.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.freeglut ];
    });
    cl-glfw = self.cl-glfw.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.glfw ];
    });
    cl-glfw-opengl-core = self.cl-glfw-opengl-core.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.libGL ];
    });
    cl-glfw3 = self.cl-glfw3.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.glfw ];
    });
    lev = self.lev.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.libev ];
    });
    cl-rdkafka = self.cl-rdkafka.overrideLispAttrs (o: {
      nativeBuildInputs = [ pkgs.rdkafka ];
      nativeLibs = [ pkgs.rdkafka ];
    });
    cl-async-ssl = self.cl-async-ssl.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.openssl ];
    });
    iolib = self.iolib.overrideLispAttrs (o: {
      nativeBuildInputs = [ pkgs.libfixposix ];
      nativeLibs = [ pkgs.libfixposix ];
      systems = [ "iolib" "iolib/os" "iolib/pathnames" ];
    });
    "cl-ana.hdf-cffi" = self."cl-ana.hdf-cffi".overrideLispAttrs (o: {
      nativeBuildInputs = [ pkgs.hdf5 ];
      nativeLibs = [ pkgs.hdf5 ];
      NIX_LDFLAGS = [ "-lhdf5" ];
    });
    gsll = self.gsll.overrideLispAttrs (o: {
      nativeBuildInputs = [ pkgs.gsl ];
      nativeLibs = [ pkgs.gsl ];
    });
    cl-libyaml = self.cl-libyaml.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.libyaml ];
    });
    cl-libxml2 = self.cl-libxml2.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.libxml2 ];
    });
    cl-readline = self.cl-readline.overrideLispAttrs (o: {
      nativeLibs = [ pkgs.readline ];
    });
  });

  qlpkgs =
    if builtins.pathExists ./imported.nix
    then pkgs.callPackage ./imported.nix { inherit build-asdf-system; }
    else {};

in qlpkgs.overrideScope' overrides
# in qlpkgs
