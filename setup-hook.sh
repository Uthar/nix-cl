# This setup hook adds every propagated lisp system to CL_SOURCE_REGISTRY

buildAsdfPath () {
    declare -A seen=()
    for system in @lispLibs@; do
        _addToAsdfPath $system
    done
}

addFileToSearchPathWithCustomDelimiter() {
    local delimiter="$1"
    local varName="$2"
    local file="$3"
    if [[ -f "$file" && "${!varName:+${delimiter}${!varName}${delimiter}}" \
          != *"${delimiter}${file}${delimiter}"* ]]; then
        export "${varName}=${!varName:+${!varName}${delimiter}}${file}"
    fi
}

addFileToSearchPath() {
    addFileToSearchPathWithCustomDelimiter ":" "$@"
}

_addToAsdfPath ()  {
    local system="$1"
    if [ -v seen[$system] ]; then
        return
    else
        seen[$system]=1
        local path="$system"

        while read jar; do
            addFileToSearchPath "CLASSPATH" "$jar"
        done < <(find "$path" -type f,l -name '*.jar')

        while read class; do
            addToSearchPath "CLASSPATH" "${class%/*}"
        done < <(find "$path" -type f,l -name '*.class')

        while read so; do
            addToSearchPath "LD_LIBRARY_PATH" "${so%/*}"
        done < <(find "$path" -type f,l -name '*.so')

        while read dylib; do
            addToSearchPath "LD_LIBRARY_PATH" "${dylib%/*}"
        done < <(find "$path" -type f,l -name '*.dylib')

        while read asd; do
            addToSearchPath "CL_SOURCE_REGISTRY" "$path//"
        done < <(find "$path" -type f,l -name '*.asd' | head -1)

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
