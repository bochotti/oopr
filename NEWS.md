# oopr (development version)

## Added

* Documentation for RStudio help completion with class members and method 
  arguments.
  
* Lists of members in roxygen documentation include some specifiers.

## Changed

* `OoprMap$keys` now returns `character(0L)` instead of `NULL` if container is 
  empty.

* Using roxygen2 `@inherits class$member` can now pull documentation from
  within the same class. Inherited `@description` is no longer appended.

## Fixed

* `OoprCovr$class` identifies filename of correct class.

# oopr 0.0.0.9000 (2026-06-20)

* Initial public repo
