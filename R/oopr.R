## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @name oopr
#' @title oopr
#' @export
#' @description
#' Create a class generator.
#'
#' @usage
#' oopr(
#'   name
#'  ,inherits   = NULL
#'  ,definition
#'  ,parent     = parent.frame()
#' )
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
#'                   the parent environment of each member.
#'
#' @details
#' Each assignment inside `definition` will become members of the class,
#' see **Examples**.
#'
#' Specifiers can be prefixed to the name of each member, separated by `:`.
#' For more information on specifiers see `...`.
#'
#' @returns
#' An `ooprC` object with slots below. To construct a new class instance,
#' call the object as a normal function.
#'
#' @examples
#' ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' oopr(
#'   name = "human"
#'  ,definition =
#'   {
#'   ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#'   human <- \(first, last, age)
#'   {
#'     stopifnot(
#'       this$isScalar(first, "character")
#'      ,this$isScalar(last,  "character")
#'      ,this$isScalar(age,   "integer")
#'     );
#'     this$first_ <- first;
#'     this$last_  <- last;
#'     this$age_   <- age;
#'   }
#'   ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#'   public:
#'     ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#'     get:name <- \( )
#'     {
#'       sprintf("%s %s", this$first_, this$last_)
#'     }
#'     ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#'     greet <- \( )
#'     {
#'       cat(sprintf(
#'         "Hello, my name is %s %s. My age is %i."
#'        ,this$first_, this$last_, this$age_
#'       ));
#'       return(invisible(this));
#'     }
#'   ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#'   private:
#'     first_ <- character(1L);
#'     last_  <- character(1L);
#'     age_   <- integer(0L);
#'     ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#'     isScalar <- \(x, type)
#'     {
#'       typeof(x) == type && length(x) == 1L;
#'     }
#'   ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#'   }
#' )
#' john <- human("john", "smith", 50L);
#' print(john);
#' john$greet();
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

  out <- constructor(name, character(0L), env$meta, encl, NULL, parent);
  assign(name, out, envir = parent);
  return(out);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @name is.oopr
#' @title Is it an oopr?
#' @export
#' @description
#' Check whether an object is `oopr` or `ooprC`.
#'
#' @param x    Any object.
#' @param name `character()` \cr
#'             Check for any class name.
#'
#' @returns
#' `logical(1L)`
#'
#' @examples
#' oopr('a',, {})
#' obj <- a();
#'
#' is.ooprC(a);
#' is.ooprC(a, 'a');
#' is.ooprC(a, 'b');
#' is.ooprC(obj);
#'
#' is.oopr(obj);
#' is.oopr(obj, 'a');
#' is.oopr(obj, 'b');
#' is.oopr(a);
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
is.oopr <- \(x, name = character(0L))
{
  stopifnot(is.character(name));
  test <- inherits(x, c("oopr", name), which = TRUE) > 0L;
  return(test[1L] && (!length(name) || any(test[-1L])));
}
