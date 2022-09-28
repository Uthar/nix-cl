# This setup hook adds every propagated lisp system to CL_SOURCE_REGISTRY

buildAsdfPath () {
    declare -A seen=()
    for system in @lispLibs@; do
        _addToAsdfPath $system
    done
}

_addToAsdfPath ()  {
    local system="$1"
    if [ -v seen[$system] ]; then
        return
    else
        seen[$system]=1
        local path="$system"
        # FIXME: search for .class
        if [ -d "$path/share/java" ]; then
            addToSearchPath "CLASSPATH" "$path/share/java/*"
        fi
        # FIXME: search for .so
        if [ -d "$path/lib" ]; then
            addToSearchPath "LD_LIBRARY_PATH" "$path/lib/"
        fi
        # FIXME: search for .asd
        if [ -d "$path" ]; then
            addToSearchPath "CL_SOURCE_REGISTRY" "$path//"
        fi
        local prop="$system/nix-support/propagated-build-inputs"
        if [ -e "$prop" ]; then
            local new_system
            for new_system in $(cat $prop); do
                _addToAsdfPath "$new_system"
            done
        fi
    fi
}

addEnvHooks "$hostOffset" buildAsdfPath
