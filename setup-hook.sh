
# TODO: it might be faster to create files in temp conf dir in /build, then set
# CL_SOURCE_REGISTRY to just (:import "/build/source-registry.conf.d")
# ASDF_OUTPUT_TRANSLATIONS to just (:import "/build/output-translations.conf.d")
#
# How about nix-shell/dev shell? Is it fine to create files there, somewhere like /tmp?

addAsdfSourceRegistry () {
  if test -z "${CL_SOURCE_REGISTRY:-}"; then
    export CL_SOURCE_REGISTRY="(:source-registry :ignore-inherited-configuration)"
  fi
  if test -d "$1/share/common-lisp/source-registry.conf.d/"; then
    if [[ ! "$CL_SOURCE_REGISTRY" =~ "$1/share/common-lisp/source-registry.conf.d/" ]]; then
      export CL_SOURCE_REGISTRY="(:source-registry :ignore-inherited-configuration (:include \"$1/share/common-lisp/source-registry.conf.d/\")${CL_SOURCE_REGISTRY:49}"
    fi
  fi
}

addAsdfOutputTranslation () {
  if test -z "${ASDF_OUTPUT_TRANSLATIONS:-}"; then
    export ASDF_OUTPUT_TRANSLATIONS="(:output-translations :ignore-inherited-configuration)"
  fi
  if test -d "$1/share/common-lisp/asdf-output-translations.conf.d/"; then
    if [[ ! "$ASDF_OUTPUT_TRANSLATIONS" =~ "$1/share/common-lisp/asdf-output-translations.conf.d/" ]]; then
      export ASDF_OUTPUT_TRANSLATIONS="(:output-translations :ignore-inherited-configuration (:include \"$1/share/common-lisp/asdf-output-translations.conf.d/\")${ASDF_OUTPUT_TRANSLATIONS:53}"
    fi
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
