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
#' [`bindenv`]), threatening some functionality of `oopr` classes.
#'
#' Proposed solution is to serialise all `ooprC` objects within the
#' package namespace during installation, then unserialise them upon package
#' loading.
#'
#' The `ooprC` objects are serialised together during `oopr_onInstall` so
#' they maintain any references between them (see [serialize]). However,
#' defining any environments from outside the classes will lose its reference.
#'
#' Inherited classes and class members from a different package are taken
#' from their respective originating namespace during `oopr_onLoad`.
#'
#' TODO: would it be better to convert all active bindings back to their
#'       functions and save their location on install, then convert back to
#'       active bindings onLoad?
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
  ns <- asNamespace(pkgname);
  if(!exists(".__OOPR__.",, ns,, "raw", FALSE)) return();
  env <- unserialize(ns[[".__OOPR__."]], refhook = refhook);
  out <- .Call(Cpp_on_load, env, ns);
  rm(list = ".__OOPR__.", envir = ns);
  return(out);
}
