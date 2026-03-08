## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname oopr
#'
#' @slot name `character(1L)` \cr
#'            The name of the class.
#'
#' @slot inhr `character()` \cr
#'            Base classes, if applicable.
#'
#' @slot meta `environment` \cr
#'            Information on all members of the class.
#'
#' @slot encl `environment` \cr
#'            A template of what the class looks like.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
ooprC <- setClass("ooprC", contains = "function", slots = c(
  name = "character"
 ,inhr = "character"
 ,meta = "environment"
 ,encl = "environment"
));

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @export
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
`@<-.ooprC` <- \(object, name, value)
{
  stop("ooprC objects are immutable", call. = FALSE);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @export
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
`$.ooprC` <- \(x, name)
{
  return(x@encl$.this[[name]]);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @export
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
`$<-.ooprC` <- \(x, name, value)
{
  x@encl$.this[[name]] <- value;
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname is.oopr
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
is.ooprC <- \(x, name = character(0L))
{
  stopifnot(is.character(name));
  if(!inherits(x, "ooprC")) return(FALSE);
  return(!length(name) || any(match(name, x@name, 0L) > 0L));
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Creates the S4 class constructor. The function embedded in object has
#' its arguments changed to match the constructor method.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
constructor <- \(name, inhr, meta, encl, src = NULL, parent)
{
  fun <- construct_fun;
  args <- formals(encl$this[[name]]);
  formals(fun) <- args;
  body <- body(fun);
  body <- do.call(substitute, list(body, list(name = name, parent = parent)));
  body[[4L]] <- as.call(c(body[[4L]], lapply(names(args), as.name)));
  body(fun)  <- body;
  attr(fun, "srcref") <- src;
  ooprC(.Data = fun, name = name, inhr = inhr, meta = meta, encl = encl);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' The constructor function that goes into the S4 class.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
construct_fun <- \(...)
{
  .  <- base::get(name, envir = parent); #<- any unintended consequences?
  .. <- base::.Call("construct_make", ., PACKAGE = "oopr");
  ..$this[[name]];
  .. <- base::.Call("construct_clean", ., .., PACKAGE = "oopr");
  return(..);
}
