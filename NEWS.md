# oopr 0.2.0 (2026-09-06)

## Added

* Constructor methods can now be used with any access specifier, allowing
  for inherit-only classes and static-only classes.

## Changed

* `is.oopr` and `is.ooprC` now also validates the structure of their objects.

* Instancing of classes now ~20% quicker.

* Constructor methods are now forced to be braced, preventing errors when 
  inheriting or having class members.

* Access specifiers are now enforced to be listed first.

* Generated constructor usage for roxygen documentation will now separate 
  arguments over multiple lines if too wide.

## Fixed

* Method source reference no longer covers the access specifier, and no longer
  includes any comments in between.
  
* Documentation now uses `\command` instead of `\code` macro for escaping
  roxygen markdown. Using `\if` inside `\code` macro had `R CMD CHECK` warning.

* Constructors & destructors can no longer have non-access specifiers.

* Constructors & destructors can no longer be referred to inside methods.


# oopr 0.0.1 (2026-07-28)

## Added

* Documentation for RStudio help completion with class members and method 
  arguments.
  
* Lists of members in roxygen documentation include some specifiers.

* Hyperlinks to jump to class sections and their methods.

* `.Rbuildignore` can be added to `@keywords` tag to build ignore the Rd file.

* Vignette for using Roxygen2 to document `oopr` classes.

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
