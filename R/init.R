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
#' This is called so I have access to foreign functions for creating `oopr`
#' classes while installing this package. `useDynLib` will load after R code
#' is evaluated but before `.onLoad`, so I need to unload it before then.
#' Path is different when using `devtools`, so some logic is applied.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
do.call(list(), what = \( )
{
  top <- topenv();
  pkg <- environmentName(top);
  if(pkg == "R_GlobalEnv") return();
  if(nzchar(Sys.getenv("DEVTOOLS_LOAD")))
  {
    path <- getwd();
    while(basename(path) != pkg && !file.exists(file.path(path, "DESCRIPTION")))
    {
      path <- dirname(path);
    }
    path <- file.path(path, "src");
  }
  else
  {
    path <- file.path(Sys.getenv("R_PACKAGE_DIR"), "libs");
    if(nzchar(.Platform$r_arch))
    {
      path <- file.path(path, .Platform$r_arch);
    }
  }
  path <- file.path(path, sprintf("%s%s", pkg, .Platform$dynlib.ext));
  dll  <- dyn.load(path, now = FALSE);
  funs <- getDLLRegisteredRoutines(dll);
  syms <- character(0L);
  lapply(funs, lapply, \(x)
  {
    sym  <- sprintf("Cpp_%s", x$name);
    syms[[length(syms) + 1L]] <<- sym;
    assign(sym, x, envir = top);
  })
  expr <- substitute({rm(list = syms, envir = top); dyn.unload(path)});
  do.call(on.exit, list(expr, TRUE, FALSE), envir = sys.frame(-11L));
})
