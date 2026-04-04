## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @useDynLib oopr, .registration = TRUE, .fixes = "Cpp_"
#' @include utils.R
#' @include meta.R
#' @include error.R
#' @include evaluate.R
#' @include property.R
#' @include static.R
#' @include S3.R
#' @include classmem.R
#' @include inherit.R
#' @include specifiers.R
#' @include definition.R
#' @include reference.R
#' @include enclosure.R
#' @include construct.R
#' @include oopr.R
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
NULL
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' This is called so I have access to foreign functions for use when creating
#' `oopr` classes within this package. `useDynLib` will load after R code is
#' evaluated but before `.onLoad`, so I need to unload it before then.
#'
#' Installing a package vs using `devtools` has a different call stack.
#' Ultimately I need the path of the `.so` file to load the foreign
#' functions, and call unloading at the right moment.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
do.call(list(), what = \( )
{
  top  <- topenv();
  pkg  <- environmentName(top);
  if(pkg == "R_GlobalEnv") return();
  if(identical(sys.call(1L)[[1L]], quote(tools:::makeLazyLoading)))
  {
    pos  <- 6L;
    path <- file.path(get("pkgpath", envir = sys.frame(1L)), "libs");
  }
  else
  {
    pos  <- -12L
    path <- file.path(get("path", envir = sys.frame(pos)), "src");
  }
  path <- file.path(path, sprintf("%s%s", pkg, .Platform$dynlib.ext));
  dll  <- dyn.load(path);
  funs <- getDLLRegisteredRoutines(dll);
  syms <- character(0L);
  lapply(funs, lapply, \(x)
  {
    sym  <- sprintf("Cpp_%s", x$name);
    syms[[length(syms) + 1L]] <<- sym;
    assign(sym, x, envir = top);
  })
  expr <- substitute({rm(list = syms, envir = top); dyn.unload(path)})
  do.call(on.exit, list(expr, TRUE, FALSE), envir = sys.frame(pos));
})
