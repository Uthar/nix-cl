
# TODO: it might be faster to create files in temp conf dir in /build, then set
# CL_SOURCE_REGISTRY to just (:import "/build/source-registry.conf.d")
# ASDF_OUTPUT_TRANSLATIONS to just (:import "/build/output-translations.conf.d")
#
# How about nix-shell/dev shell? Is it fine to create files there, somewhere like /tmp?

addAsdfSourceRegistry () {
  if test -d "$1/share/common-lisp"; then
    addToSearchPath CL_SOURCE_REGISTRY "$1/share/common-lisp/systems//"
  fi
}

addAsdfOutputTranslation () {
  if test -d "$1/share/common-lisp"; then
    addToSearchPath ASDF_OUTPUT_TRANSLATIONS "$1/share/common-lisp/systems/"
    addToSearchPath ASDF_OUTPUT_TRANSLATIONS "$(find $1/share/common-lisp/fasl/ -mindepth 1 -maxdepth 1 -print -quit)/"
  fi
}

addLibToLibraryPath () {
  if test -z "${LD_LIBRARY_PATH:-}"; then
    export LD_LIBRARY_PATH=""
  fi
  if test -z "${DYLD_LIBRARY_PATH:-}"; then
    export DYLD_LIBRARY_PATH=""
  fi
  if test -d "$1/lib"; then
    if test -n "$(find "$1/lib" -name '*.so' -print -quit)"; then
      if [[ ! "$LD_LIBRARY_PATH" =~ "$1/lib" ]]; then
        export LD_LIBRARY_PATH="${LD_LIBRARY_PATH:+$LD_LIBRARY_PATH:}$1/lib"
      fi
    fi
    if test -n "$(find "$1/lib" -name '*.dylib' -print -quit)"; then
      if [[ ! "$DYLD_LIBRARY_PATH" =~ "$1/lib" ]]; then
        export DYLD_LIBRARY_PATH="${DYLD_LIBRARY_PATH:+$DYLD_LIBRARY_PATH:}$1/lib"
      fi
    fi
  fi
}

addEnvHooks "$hostOffset" addAsdfSourceRegistry
addEnvHooks "$hostOffset" addAsdfOutputTranslation
addEnvHooks "$hostOffset" addLibToLibraryPath
