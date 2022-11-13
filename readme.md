# nix-cl

Utilities for packaging ASDF systems using Nix.

## Warning
This library is **EXPERIMENTAL** and everything can change

## Quick start

#### Build an ASDF system:

```
nix build .#sbcl.pkgs.bordeaux-threads
ls result/src
```

#### Build a lisp with packages:

```
nix build --impure --expr "with builtins.getFlake \"$(pwd)\"; sbcl.withPackages (ps: [ ps.alexandria ])"
result/bin/sbcl
```

#### Re-import Quicklisp packages:

```
nix develop
sbcl --script ql-import.lisp
```

#### Run tests:

``` shell
sbcl --script test-lisp.lisp
```

## Documentation

See `doc` directory.

## Supported Common Lisp implementations

- ABCL
- CCL
- Clasp
- CLISP
- ECL
- SBCL

## Supported systems

- aarch64-darwin
- aarch64-linux
- x86_64-darwin
- x86_64-linux

## Other Nix+CL projects

- [ql2nix](https://github.com/SquircleSpace/ql2nix)
- [lisp-modules](https://github.com/NixOS/nixpkgs/tree/master/pkgs/development/lisp-modules)
- [ql2nix](https://github.com/jasom/ql2nix)
- [cl2nix](https://github.com/teu5us/cl2nix)
- [clnix](https://git.sr.ht/~remexre/clnix)

## License

FreeBSD - see COPYING
