{ pkgs, lib, nix-cl, ... }:

let

  inherit (lib)
    getAttr
    concatStringsSep
  ;

  nix-cl-lib = nix-cl.lib;
  inherit (nix-cl-lib)
    flattenedDeps
    mkSystemsRegex
  ;

  wine = let
    wine = (pkgs.winePackagesFor "wine64").minimal;
    mingw =
      pkgs.fetchurl {
        url = "https://sourceforge.net/projects/mingw-w64/files/Toolchains%20targetting%20Win64/Personal%20Builds/mingw-builds/8.1.0/threads-posix/seh/x86_64-8.1.0-release-posix-seh-rt_v6-rev0.7z/download";
        hash = "sha256-hTlwUntd5KVeyMpNP9cywArhxpl0zJMMgmBDltQ+efg=";
      };
    pkg-config =
      pkgs.fetchurl {
        url = "http://repo.msys2.org/mingw/x86_64/mingw-w64-x86_64-pkg-config-0.29.2-3-any.pkg.tar.zst";
        hash = "sha256-hnyUYXTCotufeDbt9oqjF8hFOxQX/3hR2ZT5kBXnqaU=";
      };
    libffi =
      pkgs.fetchurl {
        url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-libffi-3.3-4-any.pkg.tar.zst";
        hash = "sha256-cYGtFjc45fUKSTJyVmQYxXIjoeoNj9+2eragQ3F0XEc=";
      };
  in pkgs.writeShellScriptBin "wine64" ''
    export WINEPREFIX=$(pwd)/.wine
    ${pkgs.p7zip}/bin/7z x ${mingw} -o"$(pwd)/.wine/drive_c" -y
    export PATH=$PATH:${pkgs.zstd}/bin
    tar xvf ${pkg-config} -C "$WINEPREFIX/drive_c"
    tar xvf ${libffi} -C "$WINEPREFIX/drive_c"
    ls -l "$WINEPREFIX/drive_c/mingw64/lib"
    ${wine}/bin/wine64 cmd /c set PATH=%PATH%\;C:\\mingw64\\bin \& echo %PATH% \& gcc --version \& $@
  '';


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

  allegroWindows = let
    src = pkgs.fetchurl {
      url = "https://github.com/liballeg/allegro5/releases/download/5.2.7.0/allegro-x86_64-w64-mingw32-gcc-10.2.0-posix-seh-static-5.2.7.0.zip";
      hash = "sha256-naeOG7BBcjXyBXWwzoPfJP/8MGq4K08IL0NN5EVnM+8=";
    };
  in pkgs.runCommand "allegro-win32" {} ''
    ${pkgs.unzip}/bin/unzip ${src}
    mkdir -pv $out
    cp allegro/bin/* $out
  '';

  libffiWindows = let
    src = pkgs.fetchurl {
      url = "https://mirror.msys2.org/mingw/mingw64/mingw-w64-x86_64-libffi-3.3-4-any.pkg.tar.zst";
      hash = "sha256-cYGtFjc45fUKSTJyVmQYxXIjoeoNj9+2eragQ3F0XEc=";
    };
  in pkgs.runCommand "libffi-win32" {} ''
    export PATH=$PATH:${pkgs.zstd}/bin
    tar xf ${src}
    mkdir -pv $out
    cp mingw64/bin/* $out
  '';

  opensslWindows = let
    src = pkgs.fetchurl {
      url = "https://mirror.firedaemon.com/OpenSSL/openssl-1.1.1n.zip";
      hash = "sha256-cVQH2MRvcyoYruJ790hDRxfK6Y+nWHhl1gindqQzW4g";
    };
  in pkgs.runCommand "openssl-win32" {} ''
    ${pkgs.unzip}/bin/unzip ${src}
    mkdir -pv $out
    cp openssl-1.1/x64/bin/*.dll $out
  '';


  # Map of nixpkgs packages to windows DLLs, for making nativeLibs work on Win32
  dllMap = {
    # SDL = null;
    allegro = allegroWindows;
    # assimp = null;
    # glfw = null;
    # libev = null;
    libffi = libffiWindows;
    # libfixposix = null;
    # libssh2 = null;
    # libuv = null;
    # mysql-client = null;
    openssl = opensslWindows;
    # postgresql = null;
    # rabbitmq-c = null;
    # rdkafka = null;
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
          systems = getAttr "systems" pkg;
        in ''
          cp -rv ${src} ${lwp.name}/packages/${pname}
          # Remove all .asd files except for those in `systems`.
          find ${lwp.name}/packages/${pname} -name "*.asd" \
          | grep -v "/\(${mkSystemsRegex systems}\)\.asd$" \
          | xargs rm -fv || true
        '')
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
lisp/${lisp.exe} %*";

      buildScript = pkgs.writeText "build-${impl}.lisp"
"(require :asdf)
(dolist (s '(${concatStringsSep " " (lib.concatMap (getAttr "systems") lispLibs)}))
  (asdf:load-system s))
(uiop:quit)"
      ;

      buildBat =
"set ASDF_OUTPUT_TRANSLATIONS=%cd%\\packages;%cd%\\fasl\r
set CL_SOURCE_REGISTRY=%cd%\\packages//\r
set PATH=%PATH%;C:\\mingw64\\bin;C:\\mingw64\\usr\\bin\r
set PKG_CONFIG_PATH=C:\\mingw64\\lib\\pkgconfig\r
echo %PKG_CONFIG_PATH%
dir %PKG_CONFIG_PATH%
gcc --version\r
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
        chmod a+x build-${impl}.bat
        ${wine}/bin/wine64 cmd /c build-${impl}.bat
        cd ..
        rm -rf ${lwp.name}/.wine
        ${pkgs.zip}/bin/zip -rv $out ${lwp.name}
      '';


in {
  inherit sbclWindows makeWindowsZipball wine;
}
