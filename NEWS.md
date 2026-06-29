# oopr 999.999 (development version)

## Added

* Documentation for RStudio help completion with class members and method 
  arguments.
  
* Lists of members in roxygen documentation include some specifiers.

* Non-documented fields are now be copied from constructor `@param`.

* Ability to document protected members.

## Changed

* `OoprMap$keys` now returns `character(0L)` instead of `NULL` if container is 
  empty.

* Using roxygen2 `@inherits class$member` can now pull documentation from
  within the same class.

* When inheriting fields that are also inherited, the active binding is used
  instead of creating a new one. This speeds up access.

## Fixed

* `OoprCovr$class` identifies filename of correct class.

* Multiple `@param` for methods now ordered by the methods signature.

* Inherited `@description` is no longer appended in the methods list.


# oopr 0.0.0.9000 (2026-06-20)

* Initial public repo
