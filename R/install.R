## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @name oopr_onInstall
#' @title Load oopr in Packages
#' @include utils.R
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
#' Proposed solution is to serialise all `ooprC` objects within the a
#' package namespace during installation, then unserialise them upon package
#' loading.
#'
#' All objects within a package are serialised together so they maintain any
#' references between them. However, saving any environments as a member will
#' lose its reference.
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
  if(!utils::hasName(ns, ".__OOPR__.")) return();

  env <- unserialize(ns[[".__OOPR__."]], refhook = refhook);
  out <- .Call("on_load", env, ns, PACKAGE = "oopr");
  rm(list = ".__OOPR__.", envir = ns);
  return(out);
}
