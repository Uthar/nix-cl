
# Lisp with packages
lwp () {
    local lisp=$1;
    shift;
    local pkgs=$@;
    nix build \
        --impure \
        --expr "with builtins; with getFlake \"$(pwd)\"; (getAttr currentSystem packages).$lisp.withPackages (ps: with ps; [ $pkgs ])"
}

# Lisp with packages in Docker
lwp+docker () {
    local lisp=$1;
    shift;
    local pkgs=$@;
    nix bundle --bundler github:NixOS/bundlers#toDockerImage \
        --impure \
        --expr "with builtins; with getFlake \"$(pwd)\"; (getAttr currentSystem packages).$lisp.withPackages (ps: with ps; [ $pkgs ])"
}

# Python with packages
pwp () {
    local python=$1;
    shift;
    local pkgs=$@;
    nix build \
        --impure \
        --expr "with builtins; with getFlake \"nixpkgs\"; (getAttr currentSystem legacyPackages).$python.withPackages (ps: with ps; [ $pkgs ])"    
}

# Python with packages
pwp+docker () {
    local python=$1;
    shift;
    local pkgs=$@;
    nix bundle --bundler github:NixOS/bundlers#toDockerImage \
        --impure \
        --expr "with builtins; with getFlake \"nixpkgs\"; (getAttr currentSystem legacyPackages).$python.withPackages (ps: with ps; [ $pkgs ])"    
}
