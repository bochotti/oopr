## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname oopr
#' @title oopr
#' @include error.R
#' @include evaluate.R
#' @include specifiers.R
#' @include definition.R
#' @include inherit.R
#' @include reference.R
#' @include enclosure.R
#' @include construct.R
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
#' simply call the object as a normal function.
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
#'         "Hello, my name is %s %s, aged %i.\n"
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
  inhr <- substitute(inherits);
  if(class(expr)[1L] != '{')
  {
    stop("`definition` must be enclosed in brackets, e.g. { ... }");
  }

  err <- error(call("oopr", name = name, quote(`...`)));
  env <- evaluate(name, expr, parent, err);

  specifiers(env, err);
  inheritance(env, inhr, parent, err);
  definitions(env, err);
  references(env, err);

  if(err$size) err$throw();

  env$meta$rmve(1L)$lock();
  encl <- enclosure(env, parent);

  inhr <- env$inhr$meta$names$data[-1L]; #env$inhr$meta$subs("names", TRUE, names = "");
  out  <- constructor(name, inhr, env$meta, encl, env$wsrc, parent);
  assign(name, out, envir = parent);
  return(out);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @exportS3Method base::names oopr
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
names.oopr <- \(x, ...)
{
  names <- NextMethod();
  class <- class(x)[1L];
  gen   <- get0(class, envir = x);
  if(is.ooprC(gen, class))
  {
    names <- gen@meta$subs("names", names = names);
  }
  return(names);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @exportS3Method base::format oopr
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
format.oopr <- \(x, ...)
{
  return(sub("environment", class(x)[1L], format.default(x)))
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @exportS3Method utils::str oopr
#' @importFrom utils str capture.output
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
str.oopr <- \(
  object
 ,max.level     = 5
 ,give.attr     = FALSE
 ,width         = getOption("width")
 ,nest.lev      = 0L
 ,indent.str    = " "
 ,comp.str      = "$"
 ,strict.width  = "cut"
 ,deparse.lines = 1L
 ,...
)
{
  short <- \(x)
  {
    cut    <- nchar(x) > width;
    x[cut] <- sprintf("%s .. ", substr(x[cut], 1L, width - 4L));
    return(x);
  }
  str.function <- \(x, ...)
  {
    cat(sub("function ", "\\\\", deparse(x, 500L, nlines = 1L)), '\n');
    return(invisible(x));
  }
  out <- capture.output({
    top <- format(object);
    cat(top, '\n', sep = "");
    if(nest.lev < max.level)
    {
      nms  <- names(object);
      wnms <- format(nms);
      len  <- 1L + nchar(comp.str) + nchar(wnms[1]) + 1L;
      for(i in seq_along(nms))
      {
        if(i == length(nms))
        {
          pre <- "\u2514\u2500";
          ind <- "  "
        }
        else
        {
          pre <- "\u251c\u2500";
          ind <- "\u2502 ";
        }
        cat(indent.str, pre, comp.str, wnms[i], ":", sep = "");
        obj <- tryCatch(object[[nms[i]]], error = identity);
        if(inherits(obj, "error")) { cat("<error>\n"); next; }
        pre <- sprintf("%s%s%s", indent.str, ind, strrep(' ', len));
        str(
          obj
         ,max.level    = max.level
         ,give.attr    = give.attr
         ,width        = width
         ,nest.lev     = nest.lev + 1L
         ,indent.str   = pre
         ,comp.str     = comp.str
         ,strict.width = strict.width
        );
      }
    }
  });
  if(nest.lev == 0L)
  {
    out <- short(out);
  }
  cat(out, sep = '\n');
  return(invisible(object));
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @exportS3Method base::print oopr
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
print.oopr <- \(x, ...)
{
  if(hasName(x, "print")) x$print(...) else str(x);
  return(invisible(x))
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
