# nix-cl

Utilities for maintaining Lisp packages and environments with Nix

#### Warning
This library is **EXPERIMENTAL** and everything can change

## Functionality

- Packaging [ASDF] systems using [Nix]
- Wrapping Lisp executables with ASDF configuration for loading pre-compiled FASL's
- Importing ASDF system definitions from existing sources (e.g. [Quicklisp])

[ASDF]: https://asdf.common-lisp.dev
[Nix]: https://nixos.org/guides/how-nix-works.html
[Quicklisp]: https://www.quicklisp.org

## Use cases

#### Pinning down the exact commits of libraries

Sometimes, a bug is fixed upstream but is not yet available in package
repositories such as Quicklisp or Ultralisp. The users have to wait
for the repository maintainer to update it, or download and compile
the patched sources themselves. This is a manual and hard to reproduce
process. By leveraging Nix, users of `nix-cl` can essentially "run
their own package repository", specified fully as Nix code, with all
the benefits of that (shareability, cacheability, reproducibility,
version-controllable etc.)


#### Modifying libraries with patches

Other times, a bug in a library is not fixed upstream, but fixed it
yourself. Or, you would like a change to the internals that the
maintainers don't like. Sure, you could fork the code or maintain
patches manually, but that becomes hard to manage with a lot of
patches. It also doesn't have the benefits mentioned in the previous
section. `nix-cl` provides a way of applying version-controlled
patches to any package.


#### Using libraries not available in repositories

There are useful and working libraries out there, that are nontheless
unavailable to users of package managers such as Quicklisp or
Ultralisp. Two real-world examples are [jzon] and [cl-tar]. `nix-cl`
is not tied to any particular package source: instead, packages are
written as a Nix expression, which can be written manually or
generated/imported. This frees the user to have any package they want,
and not be constrained by a central repository.

#### Reproducible environments

The usual way to develop a project involves several steps, such as:
1. Installing a Lisp implementation
2. Installing a package manager
3. Installing the chosen libraries

This, of course, is not reproducible (i.e. one cannot easily come back
a year later and develop the project using the exact same versions of
the dependencies), and can break between attempts a different points
in time (e.g. because the repository updated the versions in the
meantime, or the source url's have become unreachable). Even worse,
this can happen between builds and deployments of production software.

`nix-cl`, via the `lispWithPackages` function, provides a way to
create reproducible enviroments for development or deployment, which
are, again, specified *declaratively* using [Nix expressions].

[Nix expressions]: https://nixos.org/manual/nix/stable/expressions/expression-language.html

#### Testing across CL implementations

One can manually download different Lisp implementations (i.e.
pre-compiled binaries of them), and run tests of a package, for
example in CI. This works well in most cases, but it is fundamentally
limited in what you can **know** and how you can **change** the
software. Some practical examples are:

- Statically compiling [zlib] into [SBCL]
- Building [SBCL] with the low-level debugger (ldb) stripped out
- Compiling [ECL] as a static library

And so on. These are usually hard to do manually, and are not
available from package managers. With Nix it's easy, plus, you get
*almost* complete trace of how the software came to be (down to the
level of [bootstrap tools] provided by [nixpkgs]). For example, you
can query the source code, compiler flags, the C standard library
used, the compiler flags used to *compile the C compiler* used to
compile the Lisp, etc.

[zlib]: https://zlib.net
[SBCL]: https://sbcl.org
[ECL]: https://ecl.common-lisp.dev/
[Ultralisp]: https://ultralisp.org/
[jzon]: https://github.com/Zulu-Inuoe/jzon
[cl-tar]: https://gitlab.common-lisp.net/cl-tar/cl-tar
[bootstrap tools]: https://github.com/NixOS/nixpkgs/tree/master/pkgs/stdenv/linux/bootstrap-files
[nixpkgs]: https://github.com/NixOS/nixpkgs

#### Continuous integration

As mentioned, by having as much control as Nix gives you over how
software is built , it could be useful in a CI context, perhaps to:

- test against all possible compiler flag combinations
- libc versions
- JDK versions (for [ABCL])

[ABCL]: https://abcl.org

#### Windows note

Note that all of this still only applies to Unix systems - primarily because Nix doesn't work on Windows.

If you have an idea how to port some of the functionality to Windows, get in touch.

## The API

The core API functions are `build-asdf-system` and `lispWithPackagesInternal`.

They are considered more low-level that the rest of the API, which
builds on top of them to provide a more convenient interface with sane
defaults.

The higher-level API provides a lot of pre-configured packages,
including all of Quicklisp, and consists of the functions:
- `lispPackagesFor`
- `lispWithPackages`

Finally, there are functions that provide pre-defined Lisps, for
people who don't need to customize that:
- `abclPackages`, `eclPackages`, `cclPackages`, `claspPackages`, `sbclPackages`
- `abclWithPackages`, `eclWithPackages`, `cclWithPackages`, `claspWithPackages`, `sbclWithPackages`

The following is an attempt to document all of this.

### Packaging systems - `build-asdf-system`

Packages are declared using `build-asdf-system`. This function takes
the following arguments and returns a `derivation`.

#### Required arguments

##### `pname`
Name of the package/library

##### `version`
Version of the package/library

##### `src`
Source of the package/library (`fetchTarball`, `fetchGit`, `fetchMercurial` etc.)

##### `lisp`
This command must load the provided file (`$buildScript`) then exit
immediately. For example, SBCL's --script flag does just that.

#### Optional arguments

##### `patches ? []`
Patches to apply to the source code before compiling it. This is a
list of files.

##### `nativeLibs ? []`
Native libraries, will be appended to the library
path. (`pkgs.openssl` etc.)

##### `javaLibs ? []`
Java libraries for ABCL, will be appended to the class path.

##### `lispLibs ? []`
Lisp dependencies
These must themselves be packages built with `build-asdf-system`

##### `systems ? [ pname ]`
Some libraries have multiple systems under one project, for example,
[cffi] has `cffi-grovel`, `cffi-toolchain` etc.  By default, only the
`pname` system is build.

`.asd's` not listed in `systems` are removed before saving the library
to the Nix store. This prevents ASDF from referring to uncompiled
systems on run time.

Also useful when the `pname` is differrent than the system name, such
as when using [reverse domain naming]. (see `jzon` -> `com.inuoe.jzon`)

[cffi]: https://cffi.common-lisp.dev/
[reverse domain naming]: https://en.wikipedia.org/wiki/Reverse_domain_name_notation

##### `asds ? systems`
The .asd files that this package provides. By default, same as
`systems`.

#### Return value

A `derivation` that, when built, contains the sources and pre-compiled
FASL files (Lisp implementation dependent) alongside any other
artifacts generated during compilation.

#### Example

[bordeaux-threads.nix] contains a simple example of packaging `alexandria` and `bordeaux-threads`.

[bordeaux-threads.nix]: /examples/bordeaux-threads.nix

### Building a closure generator: `lispWithPackagesInternal`

Generators of runnable closures configured to find pre-compiled
libraries on run-time are built with `lispWithPackagesInternal`.

#### Required Arguments

##### `clpkgs`
an attribute set of `derivation`s returned by `build-asdf-system`

#### Return value

`lispWithPackagesInternal` returns a function (the "closure generator")
that takes one argument: a function `(lambda (clpkgs) packages)`,
that, given a set of packages, returns a list of package
`derivation`s to be included in the closure.

#### Example

The [sbcl-with-bt.nix] example creates a runnable Lisp where the
`bordeaux-threads` defined in the previous section is precompiled and
loadable via `asdf:load-system`:

[sbcl-with-bt.nix]: /examples/sbcl-with-bt.nix

### Reusing pre-packaged Lisp libraries: `lispPackagesFor`

`lispPackagesFor` is a higher level version of
`lispPackagesForInternal`: it only takes one argument - a Lisp command
to use for compiling packages. It then provides a bunch of ready to
use packages (assuming the package is supported by the chosen Lisp
implementation)

#### Required Arguments

##### `lisp`
The Lisp command to use in calls to `build-asdf-system` while building
the library-provided Lisp package declarations.

#### Return value

A set of packages built with `build-asdf-system`.

#### Example

The [abcl-package-set.nix] example generates a set of thousands of packages for ABCL.

[abcl-package-set.nix]: /examples/abcl-package-set.nix

### Reusing pre-packaged Lisp libraries, part 2: `lispWithPackages`

This is simply a helper function to avoid having to call
`lispPackagesFor` if all you want is a Lisp-with-packages wrapper.

#### Required Arguments

##### `lisp`
The Lisp command to pass to `lispPackagesFor` in order for it to
generate a package set. That set is then passed to
`lispWithPackagesInternal`.

#### Return value

A Lisp-with-packages generator function (see sections above).

#### Example

The [abcl-with-packages.nix] example generates a closure generator for ABCL.

[abcl-with-packages.nix]: /examples/abcl-with-packages.nix

### Using the default Lisp implementations

This is the easiest way to get going with `nix-cl` in general. Choose
the CL implementation of interest and a set of libraries, and get
a lisp-with-packages wrapper with those libraries pre-compiled.

#### `abclPackages`, `eclPackages`, `cclPackages`, `claspPackages`, `sbclPackages`

Ready to use package sets.

#### `abclWithPackages`, `eclWithPackages`, `cclWithPackages`, `claspWithPackages`, `sbclWithPackages`

Ready to use wrapper generators.

#### Example

For example, to open a shell with SBCL + hunchentoot + sqlite in PATH:
```
nix-shell -p 'with import ./. {}; sbclWithPackages (ps: [ ps.hunchentoot ps.sqlite ])'
```

## Importing package definitions from Quicklisp

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

#### Nix <=2.3
To instantiate some of the imported expressions, you might need https://github.com/NixOS/nix/commit/cd44c0af71ace2eb8056c2b26b9249a5aa102b41 and a couple gigs of ram, as some of them are HUGE

So, if you experience stack overflow with nix <=2.3, try again with `ulimit -s 65536`

In particular I had such problems with `cl-ana`, `generic-cl`, among others.

## Quirks

- `+` in names are converted to `_plus{_,}`: `cl+ssl`->`cl_plus_ssl`, `alexandria+`->`alexandria_plus`
- `.` to `_dot_`: `iolib.base`->`iolib_dot_base`
- names starting with a number have a `_` prepended (`3d-vectors`->`_3d-vectors`)

## Other nix+CL projects

- [ql2nix](https://github.com/SquircleSpace/ql2nix)
- [lisp-modules](https://github.com/NixOS/nixpkgs/tree/master/pkgs/development/lisp-modules)
- [ql2nix](https://github.com/jasom/ql2nix)
- [cl2nix](https://github.com/teu5us/cl2nix)
- [clnix](https://git.sr.ht/~remexre/clnix)

## License

FreeBSD - see COPYING
