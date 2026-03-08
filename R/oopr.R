## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @name oopr
#' @title oopr
#' @export
#' @description
#' Create a class generator.
#'
#' @param name       `character(1L)` \cr
#'                   The name of the class.
#'
#' @param inherits   `expression` \cr
#'                   Other classes to inherit.
#'
#' @param definition `expression` \cr
#'                   An expression defining members.
#'
#' @param parent     `environment` \cr
#'                   The environment to assign this class generator to, and
#'                   to act as the parent environment of each method.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr <- \(name, inherits = NULL, definition, parent = parent.frame())
{
  stopifnot(
    is.character(name) && length(name) == 1L && !is.na(name) && nzchar(name)
   ,is.environment(parent)
   ,!missing(definition)
  );

  expr <- substitute(definition);
  if(class(expr)[1L] != '{')
  {
    stop("`definition` must be enclosed in brackets, e.g. { ... }");
  }

  err <- error(call("oopr", name = name, quote(`...`)));
  env <- evaluate(name, expr, parent, err);

  specifiers(env, err);
  definitions(env, err);
  references(env, err);

  if(err$size) err$throw();

  env$meta$rmve(1L)$lock();
  encl <- enclosure(env, parent);

  list(
    name = name
   ,inhr = character(0L)
   ,meta = env$meta
   ,encl = encl
   ,src  = NULL
  );
  # return(env);
}
