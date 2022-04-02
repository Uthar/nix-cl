{ pkgs, lib, nix-cl, ... }:

let

  inherit (lib)
    getAttr
    concatStringsSep
  ;

  inherit (nix-cl)
    flattenedDeps
  ;

  wine = (pkgs.winePackagesFor "wine64").minimal;

  sqliteWindows = let
    src = pkgs.fetchurl {
      url = "https://sqlite.org/2022/sqlite-dll-win64-x64-3380200.zip";
      hash = "sha256-Tgvkz/yADF9I/iBnnIV/9hXmrcJ14INBjMJEHJYqD+w=";
    };
  in pkgs.runCommand "sqlite-win32" {} ''
    ${pkgs.unzip}/bin/unzip ${src}
    mkdir -pv $out
    cp * $out
  '';

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
    sqlite = sqliteWindows;
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

  sbclWine = pkgs.writeShellScriptBin "sbcl" ''
    export WINEPREFIX=$(pwd)/.wine
    ${wine}/bin/wine64 cmd /c ${sbclWindows}/sbcl.exe $@
    rm -rf .wine
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
      lispLibs = (flattenedDeps lwp.lispLibs);
      lispCmds = map
        (pkg: let
          src = getAttr "src" pkg;
          # FINE, since pnames must be unique anyway
          pname = getAttr "pname" pkg;
        in ''cp -rv ${src} ${lwp.name}/packages/${pname}'')
        lispLibs;
      dllCmds = map
        (nativeLib: let
          dll = getAttr nativeLib.pname dllMap;
        in ''cp -rv ${dll}/* ${lwp.name}/'')
        (lib.concatMap (builtins.getAttr "nativeLibs") lispLibs);
      lisp = getAttr impl implMap;

      runBat =
"@echo off
set ASDF_OUTPUT_TRANSLATIONS=%cd%\\packages;%cd%\\fasl\r
set CL_SOURCE_REGISTRY=%cd%\\packages//\r
./lisp/${lisp.exe}";

      buildScript = pkgs.writeText "build-${impl}.lisp"
"(require :asdf)
(dolist (s '(${concatStringsSep " " (map (getAttr "pname") lispLibs)}))
  (asdf:load-system s))
(uiop:quit)"
      ;

      buildBat =
"set ASDF_OUTPUT_TRANSLATIONS=%cd%\\packages;%cd%\\fasl\r
set CL_SOURCE_REGISTRY=%cd%\\packages//\r
./lisp/${lisp.exe} --script build-${impl}.lisp";

    in
      pkgs.runCommand "${lwp.name}.zip" {} ''
        mkdir -pv ${lwp.name}
        mkdir -pv ${lwp.name}/packages
        ${concatStringsSep "\n" lispCmds}
        ${concatStringsSep "\n" dllCmds}
        mkdir -pv ${lwp.name}/lisp
        cp -rv ${lisp.package}/* ${lwp.name}/lisp
        echo -n '${runBat}' > ${lwp.name}/${impl}.bat
        echo -n '${buildBat}' > ${lwp.name}/build-${impl}.bat
        cp ${buildScript} ${lwp.name}/build-${impl}.lisp
        cd ${lwp.name}
        export WINEPREFIX=$(pwd)/.wine
        chmod a+x build-${impl}.bat
        ${wine}/bin/wine64 cmd /c build-${impl}.bat
        rm -rf .wine
        cd ..
        ${pkgs.zip}/bin/zip -rv $out ${lwp.name}
      '';


in {
  inherit
    sbclWindows
    sbclWine
    makeWindowsZipball
    ;
}
