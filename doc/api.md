# The API

This page documents the Nix API of nix-cl.

## Overview

The core API is:

- `buildASDFSystem`
- `withPackages`
- `overrideLispAttrs`

The library flake exposes its supported lisp implementations as packages which
have the above functions as attributes:

- `abcl`
- `ccl`
- `clasp`
- `clisp`
- `ecl`
- `sbcl`

You can also create new lisps by `overriding` the spec of any of the provided lisps.
 

## Packaging systems - `buildASDFSystem`

Packages are declared using `buildASDFSystem`. This function takes
the following arguments and returns a Lisp package derivation.

### Example

```nix
sbcl.buildASDFSystem {
  pname = "alexandria";
  version = "v1.4";
  src = pkgs.fetchFromGitLab {
    domain = "gitlab.common-lisp.net";
    owner = "alexandria";
    repo = "alexandria";
    rev = "v1.4";
    hash = "sha256-1Hzxt65dZvgOFIljjjlSGgKYkj+YBLwJCACi5DZsKmQ=";
  };
}
```


### Required arguments

#### `pname`
Name of the package/library

#### `version`
Version of the package/library

#### `src`
Source of the package/library (`fetchTarball`, `fetchGit`, `fetchMercurial` etc.)

## Optional arguments

#### `patches ? []`

Patches to apply to the source code before compiling it. This is a
list of files.

#### `nativeLibs ? []`

Native libraries, will be appended to the library
path. (`pkgs.openssl` etc.)

#### `javaLibs ? []`

Java libraries for ABCL, will be appended to the class path.

#### `lispLibs ? []`

Lisp dependencies These must themselves be packages built with
`buildASDFSystem`

#### `systems ? [ pname ]`

Some libraries have multiple systems under one project, for example,
[cffi] has `cffi-grovel`, `cffi-toolchain` etc.  By default, only the
`pname` system is build.

`.asd's` not listed in `systems` are removed before saving the library
to the Nix store. This prevents ASDF from referring to uncompiled
systems on run time.

Also useful when the `pname` is differrent than the system name, such
as when using [reverse domain naming]. (see `jzon` ->
`com.inuoe.jzon`)

[cffi]: https://cffi.common-lisp.dev/
[reverse domain naming]: https://en.wikipedia.org/wiki/Reverse_domain_name_notation

#### `asds ? systems`

The .asd files that this package provides. By default, same as
`systems`.

### Return value

A `derivation` that, when built, contains the sources and pre-compiled
FASL files (Lisp implementation dependent) alongside any other
artifacts generated during compilation.

## Building a Lisp with packages: `withPackages`

### Example

`sbcl.withPackages (ps: [ ps.alexandria ])`

### Required Arguments

#### `pkgfn`:

A function of one argument that takes an attribute set and returns a list;
    
### Return value

A lisp derivation that knows how to load some packages with `asdf:load-system`.
