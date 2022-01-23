# nix-cl

Nix library for Common Lisp things

Warning: This library is __EXPERIMENTAL__ and everything can change

## purpose

The purpose of this library is to:

- package ASDF systems
- build `${lisp}WithPackages` wrappers

...in a fully reproducible way by leveraging the [Nix](https://nixos.org/guides/how-nix-works.html) package manager and the great [nixpkgs](https://github.com/nixos/nixpkgs) library.

In addition, it includes a mechanism to generate Nix expressions from Quicklisp releases.

## use cases

Use cases include:

- pinning down the exact commits of lisp libraries
- modifying libraries with patches
- using libraries not available in quicklisp
- using historical quicklisp releases
- reproducible development environments
- testing across CL implementations
- continuous integration

## usage

### packaging systems

Packages are declared using `build-asdf-system` - its API is documented in `nix-cl.nix`. A simple example of packaging `bordeaux-threads` would be:

```
bordeaux-threads = build-asdf-system {
  pname = "bordeaux-threads";
  version = "0.8.8";
  src = builtins.fetchTarball {
    url = "http://github.com/sionescu/bordeaux-threads/archive/v0.8.8.tar.gz";
    sha256 = "19i443fz3488v1pbbr9x24y8h8vlyhny9vj6c9jk5prm702awrp6";
  };
  lispLibs = [ alexandria ];
};
```


### building lisp wrappers

Lisp wrappers are built with `${lisp}WithPackages`. For example, to open a shell with SBCL + hunchentoot + sqlite in PATH:
```
nix-shell -p 'with import <nix-cl> {}; sbclWithPackages (ps: [ ps.hunchentoot ps.sqlite ])'
```

Then, inside `sbcl`, it's possible to load the libraries:
```
(require :asdf)
(asdf:load-system :hunchentoot)
(asdf:load-system :sqlite)
```

Note that there is no local state in the user's home directory as the wrapper and all the FASL files reside in the Nix store:

```
[nix-shell:~]$ which sbcl
/nix/store/58dxvhkhmcncadz021b5pijlghmmv6ca-sbcl-with-packages/bin/sbcl
```

The wrapper works by setting the required ASDF related environment variables before starting the lisp:
```
#! /nix/store/xvvgw9sb8wk6d2c0j3ybn7sll67s3s4z-bash-4.4-p23/bin/bash -e
export CL_SOURCE_REGISTRY='/nix/store/a3qj93r27zm32ip7zz9k5mysjxipba4f-hunchentoot-1.3.0...
export ASDF_OUTPUT_TRANSLATIONS='/nix/store/a3qj93r27zm32ip7zz9k5mysjxipba4f-hunchentoot-1.3.0...
export LD_LIBRARY_PATH='/nix/store/5fzyj1yc7vki7kxi9xp1jvdmj6ijmkqi-sqlite-3.35.5/lib...
exec "/nix/store/kaqv0iqv5kwrh3qq8z5n74x46sj2ra8k-sbcl-2.2.0/bin/sbcl"  "$@"
```

ASDF is then able to find the system definition files and load the precompiled FASLs.

### Supported Lisps

Supported `${lisp}`s are:

- `sbclWithPackages`
- `eclWithPackages`
- `abclWithPackages`
- `cclWithPackages`
- `claspWithPackages`

Other lisps can be achieved by passing an implementation-dependent "run-a-script-noninteractively" command to `lispWithPackages`:
```
myLispWithPackages = lispWithPackages "${pkgs.my-lisp}/bin/my-lisp --batch --load"
```

To just get the package set, use `lispPackagesFor`:
```
myLispPackages = lispPackagesFor "${pkgs.my-lisp}/bin/my-lisp --batch --load"
```

### Supported OS

Tested only on Linux, but should work on MacOS too.

### importing from Quicklisp

To generate Nix expressions from a Quicklisp release, run:

```
nix-shell
sbcl --script nix-quicklisp.lisp
```
This produces a `from-quicklisp.nix` containing Nix expressions for all packages in Quicklisp, which will be automatically picked up by `${lisp}WithPackages`.

This is going to **take a while** because it needs to fetch the source code of each system to compute its sha256 hash (quicklisp provides a sha1 hash but Nix's `builtins.fetchTarball` requires a sha256.). During the first run, the sha256s are cached in `quicklisp.sqlite` and are reused in subsequent invocations. (Though feel free to save the earth some electricity and download a **pre-filled database** from https://galkowski.xyz/quicklisp.sqlite)

Quicklisp release url's are currently hard-coded and can be changed directly in the source code.

At the moment native and Java libraries need to be added manually to imported systems in `ql.nix` on an as-needed basis.

Also worth noting is that systems imported this way will prefer things from `packages.nix` as dependencies, so that custom versions can be provided or broken versions replaced. This works by rewriting the dependency trees of the packages using the Nix language.

#### nix <=2.3
To instantiate some of the imported expressions, you might need https://github.com/NixOS/nix/commit/cd44c0af71ace2eb8056c2b26b9249a5aa102b41 and a couple gigs of ram, as some of them are HUGE

So, if you experience stack overflow with nix <=2.3, try again with `ulimit -s 65536`

In particular I had such problems with `cl-ana`, `generic-cl`, among others.

## Quirks

- `+` in names are converted to `_plus{_,}`: `cl+ssl`->`cl_plus_ssl`, `alexandria+`->`alexandria_plus`
- `.` to `_dot_`: `iolib.base`->`iolib_dot_base`
- names starting with a number have a `_` prepended (`3d-vectors`->`_3d-vectors`)

## other nix+CL projects

- https://github.com/SquircleSpace/ql2nix
- https://github.com/NixOS/nixpkgs/tree/master/pkgs/development/lisp-modules
- https://github.com/jasom/ql2nix

## license

FreeBSD - see COPYING
