## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @name oopr_onInstall
#' @title Load oopr in Packages
#' @export
#' @description
#' Correctly install and load `oopr` classes when developing a package.
#'
#' @param ns      `namespace` \cr
#'                The namespace to serialise into, can be left blank.
#'
#' @param refhook `function` \cr
#'                See [`serialize`].
#'
#' @details
#' Active bindings are not preserved during package installation (see
#' [`bindenv`]), threatening the functionality of `oopr` classes.
#'
#' Proposed solution is to serialise all `ooprC` objects within the
#' package namespace during installation, then unserialise them upon package
#' loading.
#'
#' The `ooprC` objects are serialised together during `oopr_onInstall` so
#' they maintain any references between them (see [`serialize`]). Note that
#' using environments assigned outside the classes will lose their reference.
#'
#' Serializing has two major draw-backs:
#'
#'   1. References to classes from other packages are *not* preserved, which
#'      will break any static members.
#'   2. Objects no longer share bindings, and each binding is provided its own
#'      memory address (i.e. copied), which increases RAM usage.
#'
#' Running `oopr_onLoad` within a packages [`.onLoad()`] will reverse the two
#' items above. Inherited classes and class members from a different packages
#' are taken from their respective originating namespace, and any inherited
#' functions (including active binding functions) are de-duplicated.
#'
#' @examples
#' \dontrun{
#' # add to zzz.R
#' .onLoad <- \(libname, pkgname)
#' {
#'   oopr_onLoad();
#' }
#' oopr_onInstall();}
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr_onInstall <- \(ns, refhook = NULL)
{
  if(missing(ns)) ns <- topenv(parent.frame());
  if(!isNamespace(ns)) stop("`ns` must be a namespace");
  env <- new.env(parent = emptyenv());
  for(nm in names(ns))
  {
    if(is.ooprC(ns[[nm]]))
    {
      env[[nm]] <- ns[[nm]];
    }
  }
  rm(list = names(env), envir = ns);
  ns[[".__OOPR__."]] <- serialize(env, NULL, refhook = refhook);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname oopr_onInstall
#' @export
#'
#' @param libname `character(1L)` \cr
#'                Package path from `.onLoad`, can be left blank.
#'
#' @param pkgname `character(1L)` \cr
#'                Package name from `.onLoad`, can be left blank.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr_onLoad <- \(libname, pkgname, refhook = NULL)
{
  if(missing(libname)) libname <- get("libname", envir = parent.frame());
  if(missing(pkgname)) pkgname <- get("pkgname", envir = parent.frame());
  ns <- if(isNamespace(pkgname)) pkgname else asNamespace(pkgname);
  if(!exists(".__OOPR__.",, ns,, "raw", FALSE)) return();
  env <- unserialize(ns[[".__OOPR__."]], refhook = refhook);
  out <- .Call(Cpp_on_load, env, ns);
  rm(list = ".__OOPR__.", envir = ns);
  return(out);
}
