## Importing package definitions from Quicklisp

This page documents how to import packages from Quicklisp.

## Nix dumper

Run:

```
$ nix-shell
$ sbcl --script ql-import.lisp
```

This command runs a program that dumps a `imported.nix` file
containing Nix expressions for all packages in Quicklisp. They will be
automatically picked up by the `lispPackagesFor` and
`lispWithPackages` API functions.

## Tarball hashes

This is going to take a while because it needs to fetch the source
code of each system to compute its SHA256 hash. Quicklisp provides a
SHA1 hash, but Nix's `builtins.fetchTarball` requires a SHA256.

During the first run, the hashes are cached in `packages.sqlite`, and
are reused in subsequent invocations.

## Choosing a Quicklisp release

Quicklisp release url's are currently hard-coded and can be changed
directly in the source code. See the `import` directory.

## Native and Java libraries

At the moment, native and Java libraries need to be added manually to
imported systems in `ql.nix` on an as-needed basis.

## Dependencies from packages.nix

Also worth noting is that systems imported from Quicklisp will prefer
packages from `packages.nix` as dependencies, so that custom versions
can be provided or broken versions replaced.
