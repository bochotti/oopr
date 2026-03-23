## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @useDynLib oopr, .registration = TRUE, .fixes = "Cpp_"
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Used to defer classes created in this package.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
.__DEFERRED__. <- new.env(parent = emptyenv());
.__DEFERRED__.$.__ORDER__. <- character(0L);
DEFER <- \(expr, eval.env = parent.frame(), assign.env = .__DEFERRED__.)
{
  expr <- substitute(expr);
  expr <- match.call(oopr, expr);
  expr$parent <- expr$parent %||% eval.env;
  assign.env$.__ORDER__.[[length(assign.env$.__ORDER__.) + 1L]] <- expr$name;
  do.call(delayedAssign, list(expr$name, expr, eval.env, assign.env));
  return(invisible(NULL))
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Prompt deferred classes when package is being loaded.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
.onLoad <- \(libname, pkgname)
{
  for(sym in .__DEFERRED__.$.__ORDER__.) { .__DEFERRED__.[[sym]]; }
  assign(x = ".__DEFERRED__.", value = NULL, envir = getNamespace(pkgname));
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
