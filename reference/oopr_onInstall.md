# Load oopr in Packages

Correctly install and load `oopr` classes when developing a package.

## Usage

``` r
oopr_onInstall(ns, refhook = NULL)

oopr_onLoad(libname, pkgname, refhook = NULL)
```

## Arguments

- ns:

  `namespace`  
  The namespace to serialise into, can be left blank.

- refhook:

  `function`  
  See [`serialize`](https://rdrr.io/r/base/serialize.html).

- libname:

  `character(1L)`  
  Package path from `.onLoad`, can be left blank.

- pkgname:

  `character(1L)`  
  Package name from `.onLoad`, can be left blank.

## Details

Active bindings are not preserved during package installation (see
[`bindenv`](https://rdrr.io/r/base/bindenv.html)), threatening the
functionality of `oopr` classes.

Proposed solution is to serialise all `ooprC` objects within the package
namespace during installation, then unserialise them upon package
loading.

The `ooprC` objects are serialised together during `oopr_onInstall` so
they maintain any references between them (see
[`serialize`](https://rdrr.io/r/base/serialize.html)). Note that using
environments assigned outside the classes will lose their reference.

Serializing has two major draw-backs:

1.  References to classes from other packages are *not* preserved, which
    will break any static members.

2.  Objects no longer share bindings, and each binding is provided its
    own memory address (i.e. copied), which increases RAM usage.

Running `oopr_onLoad` within a packages
[`.onLoad()`](https://rdrr.io/r/base/ns-hooks.html) will reverse the two
items above. Inherited classes and class members from a different
packages are taken from their respective originating namespace, and any
inherited functions (including active binding functions) are
de-duplicated.

## Examples

``` r
if (FALSE) { # \dontrun{
# add to zzz.R
.onLoad <- \(libname, pkgname)
{
  oopr_onLoad();
}
oopr_onInstall();} # }
```
