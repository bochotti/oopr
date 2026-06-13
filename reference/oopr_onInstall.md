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
[`bindenv`](https://rdrr.io/r/base/bindenv.html)), threatening some
functionality of `oopr` classes.

Proposed solution is to serialise all `ooprC` objects within the package
namespace during installation, then unserialise them upon package
loading.

The `ooprC` objects are serialised together during `oopr_onInstall` so
they maintain any references between them (see
[serialize](https://rdrr.io/r/base/serialize.html)). However, defining
any environments from outside the classes will lose its reference.

Inherited classes and class members from a different package are taken
from their respective originating namespace during `oopr_onLoad`.

TODO: would it be better to convert all active bindings back to their
functions and save their location on install, then convert back to
active bindings onLoad?

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
