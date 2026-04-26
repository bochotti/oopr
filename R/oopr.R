## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname oopr
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
#'                   The environment to assign this class to, and acts as
#'                   a parent environment for each member.
#'
#' @details
#' Each assignment inside `definition` will become members of the class,
#' see **Examples**.
#'
#' Specifiers can be prefixed to the name of each member, separated by `:`.
#' For more information on specifiers see `...`.
#'
#' @returns
#' `NULL` invisibly. An `ooprC` object with the slots below is assigned to
#' symbol `name` in `parent`.
#'
#' To construct a new class instance, simply call the `ooprC` object as a
#' normal function.
#'
#' **DO NOT** use an assignment operator.
#'
#' @examples
#' ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' #  human as a class
#' ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' oopr("human",,
#' {
#' ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' human <- \(first, last, age)
#' {
#'   stopifnot(
#'     this$isScalar(first, "character")
#'    ,this$isScalar(last,  "character")
#'    ,this$isScalar(age,   "integer")
#'   );
#'   this$first_ <- first;
#'   this$last_  <- last;
#'   this$age_   <- age;
#' }
#' ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' public:
#'   ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#'   get:name <- \( )
#'   {
#'     sprintf("%s %s", this$first_, this$last_)
#'   }
#'   ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#'   greet <- \( )
#'   {
#'     cat(sprintf(
#'       "Hello, my name is %s %s, aged %i.\n"
#'      ,this$first_, this$last_, this$age_
#'     ));
#'     return(invisible(this));
#'   }
#' ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' private:
#'   first_ <- character(1L);
#'   last_  <- character(1L);
#'   age_   <- integer(0L);
#'   ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#'   isScalar <- \(x, type)
#'   {
#'     typeof(x) == type && length(x) == 1L;
#'   }
#' ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' })
#' ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
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
  if(match(name, c("this", ".this"), 0L))
  {
    stop(sprintf("`name` cannot be \"%s\"", name));
  }

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

  inhr <- env$inhr$meta$names$data[-1L];
  out  <- constructor(name, inhr, env$meta, encl, env$wsrc, parent);
  assign(name, out, envir = parent);
  return(invisible(NULL));
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @exportS3Method base::names oopr
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
names.oopr <- \(x)
{
  names <- NextMethod();
  class <- class(x)[1L];
  ooprC <- get0(class, parent.env(parent.env(x)), inherits = FALSE);
  if(is.ooprC(ooprC, class))
  {
    names <- ooprC@meta$subs("names", names = names);
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
 ,max.level     = 5L
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
  if(is.na(max.level))
  {
    max.level <- 5L;
  }
  if(is.null(dynGet(".nest.lev", NULL)))
  {
    .nest.lev <- nest.lev;
  }
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
  str.functionWithTrace <- \(x, ...)
  {
    cat("X");
    if(isS4(x))
    {
      x <- x@original;
    }
    NextMethod();
    # str.function(x@original, ...);
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
  if(nest.lev == dynGet(".nest.lev", 0L))
  {
    out <- short(out);
  }
  cat(out, sep = '\n');
  return(invisible(object));
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @exportS3Method base::print oopr
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
print.oopr <- \(x, max.level = 5L, ...)
{
  str.oopr(x, max.level = max.level, ...);
  return(invisible(x))
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @exportS3Method utils::.DollarNames
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
.DollarNames.oopr <- \(x, pattern = "", names = NULL)
{
  names <- names %||% names(x);
  names <- grep(pattern, names, value = TRUE);
  if(match("tools:rstudio", search(), 0L))
  {
    attributes(names) <- dollar_attr(x, names);
  }
  return(names);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
dollar_attr <- \(x, names)
{
  types <- integer(length(names));
  meta  <- character(length(names));
  t <- get(".rs.getCompletionType", envir = as.environment("tools:rstudio"));
  for(i in seq_along(names)) tryCatch(
  {
    mem      <- .subset2(x, names[i]);
    types[i] <- if(is.null(mem)) 22L else t(mem);
    if(match(types[i], 0:1, 0L))
    {
      if(inherits(mem, "Date"))
      {
        meta[i] <- "dte";
      }
      else if(inherits(mem, "POSIXt"))
      {
        meta[i] <- "dtm";
      }
      else
      {
        meta[i] <- abbreviate(typeof(mem), minlength = 3L);
        meta[i] <- switch(meta[i], lgc = "lgl", meta[i]);
      }
    }
    if(isS4(mem) && inherits(mem, "functionWithTrace"))
    {
      types[i] <- 6L;
      meta[i]  <- "X";
    }
  }
  ,error = \(e)
  {
    types[i] <<- 19L;
  });

  #TODO: delete this
  ins <- get_in_stack(".rs.getCompletionsDollar");
  if(!is.null(ins))
  {
    expr <- substitute({
      ret <- returnValue();
      ret$type <- types[match(names, ret$results, 0L)];
      return(ret)
    }, list(types = types, names = names));
    do.call(on.exit, list(expr, TRUE, FALSE), envir = ins);
  }

  help <- OoprCompletionHelp@encl$.this$makeHelpHandler(x);
  return(list(types = as.integer(types), meta = meta, helpHandler = help));
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
