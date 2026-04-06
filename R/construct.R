## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
setOldClass("oopr_meta");
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname oopr
#' @importFrom methods new
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
 ,meta = "oopr_meta"
 ,encl = "environment"
));

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @exportS3Method "@<-" ooprC
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
`@<-.ooprC` <- \(object, name, value)
{
  sym  <- substitute(value);
  call <- call("<-", call('@', as.name(object@name), name), sym);
  stop(simpleError("ooprC objects are immutable", call));
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @exportS3Method "$" ooprC
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
`$.ooprC` <- \(x, name)
{
  comp  <- OoprCompletion();
  if(comp$isRStudioCompletion())
  {
    if(comp$isClassMember(x, name) || comp$isInheritedClass(x, name))
    {
      return(.subset2(comp$obj, name));
    }
  }
  .this <- x@encl$.this;
  if(!exists(name, envir = .this, inherits = FALSE))
  {
    msg  <- sprintf("`%s` is not a public static member", name);
    call <- call('$', as.name(x@name), name);
    stop(simpleError(msg, call));
  }
  if(nzchar(x@meta$subs("property", names = name)))
  {
    op <- options(show.error.messages = FALSE, error = \() {
      msg  <- geterrmessage();
      call <- deparse1(call("$", as.name(x@name), name));
      msg  <- sub("(Error in )(.*?)( :.*?$)", sprintf("\\1%s\\3", call), msg);
      cat(msg);
    });
    on.exit(options(op));
  }
  return(.this[[name]]);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @exportS3Method "$<-" ooprC
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
`$<-.ooprC` <- \(x, name, value)
{
  sym <- substitute(value);
  `$.ooprC`(x, name);
  op <- options(show.error.messages = FALSE, error = \() {
    msg  <- geterrmessage();
    call <- deparse1(call("<-", call("$", as.name(x@name), name), sym));
    msg  <- sub(".this[[name]] <- value", fixed = TRUE, call, msg);
    cat(msg);
  });
  on.exit(options(op));
  .this <- x@encl$.this;
  .this[[name]] <- value;
  return(invisible(x));
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @exportS3Method "[" ooprC
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
`[.ooprC` <- \(x, name)
{
  comp  <- OoprCompletion();
  if(comp$isRStudioCompletion())
  {
    if(comp$isContainerMember(x, name))
    {
      return(comp$obj);
    }
  }
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @exportS3Method base::names ooprC
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
names.ooprC <- \(x) { return(names(x@encl$.this)); }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @exportS3Method utils::.DollarNames ooprC
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
.DollarNames.ooprC <- \(x, pattern)
{
  comp <- OoprCompletion();
  if(comp$isRStudioCompletion())
  {
    if(comp$isClassMember(x) || comp$isInheritedClass(x))
    {
      return(comp$names)
    }
  }
  NextMethod();
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @exportS3Method base::format ooprC
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
format.ooprC <- \(x, ...)
{
  return(sub(sprintf("(%s)", x@name), "\\1 ooprC", format(x@encl$.this)));
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
show <- methods::show;
setMethod("show", c(object = "ooprC"), \(object)
{
  bot <- capture.output(str.oopr(object@encl$.this));
  top <- format(object);
  usg <- deparse(object@.Data, getOption("width"), nlines = 1L);
  usg <- sub("function ", object@name, usg);
  cat(sprintf("%s\nUsage:\n  %s\n", top, usg));
  if(length(bot) > 1L)
  {
    bot[1L] <- "Static Members:";
    cat(bot, sep = '\n');
  }
  return(invisible(object))
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @exportS3Method base::print ooprC
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
print.ooprC <- \(x, ...) show(x);

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname is.oopr
#' @export
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
  fun  <- construct_fun;
  args <- formals(encl$this[[name]]);
  formals(fun) <- args;
  body <- body(fun);
  body <- do.call(substitute, list(body, list(class = name, within = parent)));
  body[[4L]] <- as.call(c(body[[4L]], lapply(names(args), as.name)));
  body(fun)  <- body;
  attr(fun, "srcref") <- src;
  ooprC(.Data = fun, name = name, inhr = inhr, meta = meta, encl = encl);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' The constructor function that goes into the S4 class.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
construct_fun <- \(...)
{
  .  <- base::get(class, envir = within); #<- any unintended consequences?
  .. <- base::.Call(Cpp_oopr_make, .);
  ..$this[[class]];
  .. <- base::.Call(Cpp_oopr_tidy, ., .., base::sys.frames());
  return(..);
}
