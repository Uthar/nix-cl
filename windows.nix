{ pkgs, lib, nix-cl, ... }:

let

  inherit (lib)
    getAttr
    concatStringsSep
  ;

  inherit (nix-cl)
    flattenedDeps
  ;

  # Map of nixpkgs packages to windows DLLs, for making nativeLibs work on Win32
  dllMap = {
    SDL = null;
    allegro5 = null;
    assimp = null;
    glfw = null;
    libev = null;
    libffi = null;
    libfixposix = null;
    libssh2 = null;
    libuv = null;
    mysql-client = null;
    openssl = null;
    postgresql = null;
    rabbitmq-c = null;
    rdkafka = null;
    sqlite = null;
  };

  sbclWindows = let
    system = "x86-64-windows";
    version = "2.2.3";

    dontUnpack = true;

    src = pkgs.fetchurl {
      url = "mirror://sourceforge/project/sbcl/sbcl/${version}/sbcl-${version}-${system}-binary.msi";
      hash = "sha256-IuLPBj9XFtEKSVGeLt5VKO/zciODiJ8SJD6zzfWrUpM=";
    };

  in pkgs.runCommand "sbcl-${version}" {} ''
    ${pkgs.msitools}/bin/msiextract ${src}
    mkdir $out
    cp -Tr "PFiles/Steel Bank Common Lisp" $out
  '';


  # Map of implementation names to windows executables
  implMap = {
    abcl = null;
    ccl = null;
    ecl = null;
    sbcl = { package = sbclWindows; exe = "sbcl.exe"; };
  };

  # creates a lispWithPackages.zip, for use outside of Nix, and, on Windows
  makeWindowsZipball = lwp:
    let
      # This is sbcl, abcl, etc. (version is 'with-packages')
      impl = lwp.pname;
      lispCmds = map
        (pkg: let
          src = getAttr "src" pkg;
          # FINE, since pnames must be unique anyway
          pname = getAttr "pname" pkg;
        in ''cp -rv ${src} ${lwp.name}/packages/${pname}'')
        (flattenedDeps lwp.lispLibs);
      dllCmds = map
        (nativeLib: let
          dll = getAttr nativeLib.pname dllMap;
        in ''cp -v ${dll} ${lwp.name}/${dll}'')
        lwp.nativeLibs;
      lisp = getAttr impl implMap;

      script =
"@echo off
set ASDF_OUTPUT_TRANSLATIONS=%cd%\\packages;%cd%\\fasl\r
set CL_SOURCE_REGISTRY=%cd%\\packages//\r
./lisp/${lisp.exe}";

    in
      pkgs.runCommand "${lwp.name}.zip" {} ''
        mkdir -pv ${lwp.name}
        mkdir -pv ${lwp.name}/packages
        ${concatStringsSep "\n" lispCmds}
        ${concatStringsSep "\n" dllCmds}
        mkdir -pv ${lwp.name}/lisp
        cp -rv ${lisp.package}/* ${lwp.name}/lisp
        echo -n '${script}' > ${lwp.name}/${impl}.bat
        ${pkgs.zip}/bin/zip -rv $out ${lwp.name}
      '';


in {
  inherit sbclWindows makeWindowsZipball;
}
