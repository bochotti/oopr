## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @name OoprSource
#' @title Source files for oopr
#' @keywords internal
#' @include init.R
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
NULL

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname OoprSource
#' @description
#' Takes a file and parses the oopr definitions inside it.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprSource",,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field file `character(1L)` \cr
  #'             An R source file.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:file <- \( )
  {
    return(this$file_);
  }
  set:file <- \(x)
  {
    stopifnot(is.character(x) && length(x) == 1L && file.exists(x));
    this$file_ <- x;
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field text `character()` \cr
  #'             A vector of lines of an R source file.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:text <- \( )
  {
    return(this$text_);
  }
  set:text <- \(x)
  {
    stopifnot(is.character(x));
    this$text_ <- x;
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field row  `integer(1L)` \cr
  #'             Line of a text cursor position.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:row  <- \( )
  {
    return(this$row_);
  }
  set:row  <- \(x)
  {
    stopifnot(is.numeric(x) && length(x) == 1L && x %% 1L == 0L);
    this$row_ <- as.integer(x);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field col  `integer(1L)` \cr
  #'             Line of a text cursor position.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:col  <- \( )
  {
    return(this$col_);
  }
  set:col  <- \(x)
  {
    stopifnot(is.numeric(x) && length(x) == 1L && x %% 1L == 0L);
    this$col_ <- as.integer(x);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field expr `expression` \cr
  #'             A parsed expressed of `$file` or `$text` from `$parse()`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:expr <- \( )
  {
    return(this$expr_);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field defs `list()` \cr
  #'             A named list of `oopr` definitions from `$parse()`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:defs <- \( )
  {
    return(this$defs_);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field objs `list()` \cr
  #'             A named list of `oopr` object from `$eval()`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:objs <- \( )
  {
    return(this$objs_);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Parse `$file` or `$text`.
  #'
  #' @details
  #' If `$file` is set, then it acts as the `srcfile` of the parsed
  #' object.
  #'
  #' @returns
  #' Saves the entire parse to `$expr` and the `oopr` calls to `$defs`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  parse <- \( )
  {
    op <- options(keep.source = TRUE);
    on.exit(options(op));
    if(length(this$file_) && length(this$text_))
    {
      expr <- parse(text = this$text_, srcfile = srcfile(this$file_));
    }
    else if(length(this$file_))
    {
      expr <- parse(this$file_);
    }
    else if(length(this$text_))
    {
      expr <- parse(text = this$text_);
    }
    else
    {
      return(invisible(this));
    }

    ats  <- findInExpr(expr, \(e) is.ooprcall(e));
    defs <- rep_len(list(), length(ats));
    for(i in seq_along(ats))
    {
      defs[[i]]      <- expr[[ats[[i]]]];
      names(defs)[i] <- match.call(oopr, defs[[i]])$name;
      attr(defs[[i]], "srcref") <- attr(expr, "srcref", TRUE)[[ats[[i]][1L]]];
    }
    this$expr_ <- expr;
    this$defs_ <- defs;

    return(invisible(this));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Evaluate the `oopr`s in `$defs`.
  #'
  #' @param top `environment` \cr
  #'            An environment to be the top-level of the `oopr` objects.
  #'
  #' @details
  #' If `$row` & `$col` are set, then evaluation stops at that `oopr` call.
  #'
  #' @returns
  #' Saves the `oopr` objects to `$objs`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  eval <- \(top = globalenv())
  {
    env  <- new.env(parent = top);
    defs <- this$defs_;
    objs <- rep_len(list(), length(defs));
    pos  <- length(this$row_) && length(this$col_);
    for(i in seq_along(defs))
    {
      eval(defs[[i]], env, NULL);
      objs[[i]]                 <- env[[names(defs)[i]]];
      attr(defs[[i]], "srcref") <- attr(objs[[i]], "srcref", TRUE);
      if(pos && this$posIsInSrcRef(def = defs[[i]])) break;
    }
    names(objs) <- names(defs);
    this$defs_   <- defs;
    this$objs_   <- objs;
    return(invisible(this));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field obj `ooprC`
  #'            If `$row` and `$col` are set, then the `oopr` object that
  #'            is at that location.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:obj <- \( )
  {
    if(!(length(this$row_) && length(this$col_))) return(NULL);
    objs <- this$objs_;
    defs <- this$defs_;

    for(i in seq_along(defs))
    {
      if(this$posIsInSrcRef(def = defs[[i]]))
      {
        if(i > length(objs)) break;
        return(objs[[i]]);
      }
    }
    return(NULL);
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  file_ <- character(0L);
  text_ <- character(0L);
  row_  <- integer(0L);
  col_  <- integer(0L);
  expr_ <- expression();
  defs_ <- list();
  objs_ <- list();

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  posIsInSrcRef <- \(row = this$row_, col = this$col_, def)
  {
    src <- attr(def, "srcref", TRUE);
    if(is.null(src))                     return(FALSE);
    if(src[1L]  < row && row <  src[3L]) return(TRUE);
    if(src[1L] == row && col >= src[5L]) return(TRUE);
    if(src[3L] == row && col <= src[6L]) return(TRUE);
    return(FALSE);
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
})
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname OoprSource
#' @description
#' Tries to parse and evaluate a file containing ooprs, intended for
#' completion.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprSourceTry", public:OoprSource,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
OoprSourceTry <- \(file = NULL, text = NULL, row = NULL, col = NULL)
{
  if(!is.null(file))
  {
    OoprSource$file <- file;
    text <- text %||% readLines(file, warn = FALSE);
  }
  if(!is.null(text))
  {
    OoprSource$text <- text;
  }
  if(!is.null(row))
  {
    OoprSource$row  <- row;
  }
  if(!is.null(col))
  {
    OoprSource$col  <- col;
  }
}
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field call `call` \cr
  #'             The evaluation string/call at the position `row` & `col` in
  #'             the file, e.g. `this$a$b`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  call <- NULL;

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Try to parse the file.
  #'
  #' @details
  #' When completing, the contents of a file may fail to parse:
  #'    1. `$` with no rhs, e.g. `this$`
  #'    2. Incomplete control-flow, e.g. `if(i in this$)`
  #'
  #' Additionally, evaluation may fail due to member names or method
  #' signatures:
  #'    1. `this$mem` instead of `this$member`
  #'    2. `this$method()` instead of `this$method(x = 1L)`
  #'
  #' As such, `this` (or an inherited class) is replaced with `list` so the
  #' class members are not reference checked.
  #'
  #' @returns
  #' See `OoprSource$parse()`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  parse <- \( )
  {
    text  <- paste(this$text, collapse = '\n');
    parts <- .Call(Cpp_eval_context, text, this$row, this$col);
    text  <- this$replaceThis(text, parts);
    if(length(parts) > 1)
    {
      text  <- this$collectCall(text, parts[[1L]] %||% parts[[2L]]);
    }
    else
    {
      text  <- this$collectCall(text, parts[[1L]]);
    }
    this$text <- text <- strsplit(text, '\n')[[1L]];
    this$tryParse(
      # , # add { ... } after control flows
      {
        line <- text[this$row];
        ptrn <- "((if|for|while)\\s*(?'p'\\(([^()]|(?&p))*\\)))[ \t\r]*$";
        line <- sub(ptrn, "\\1 { }", line, perl = TRUE);
        text[this$row] <- line;
      }
    );
    return(invisible(this));
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  tryParse <- \(...)
  {
    dots <- as.list.default(substitute(list(...)))[-1L];
    err  <- NULL;
    for(i in seq_along(dots))
    {
      text <- this$text;
      eval(dots[[i]]);
      this$text <- text;
      tryCatch(
      {
        OoprSource$parse();
        break;
      }
      ,error = \(e)
      {
        if(is.null(err))
        {
          err <<- e;
        }
        if(i == length(dots)) stop(err);
      })
    }
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  replaceThis <- \(text, parts)
  {
    for(part in parts)
    {
      if(is.null(part)) next;
      # skip over `for` because of `in`
      if(grepl("^for(\\s|$)", part$ctx)) next;
      # adjust the line number if we are removing a row
      this$row <- this$row - sum(charToRaw(part$ctx) == as.raw(10));
      # replace `this`, ensuring the length remains unchanged
      substr(text, part$stt, part$end) <- format.default(
        "c", width = 1L + part$end - part$stt
      );
    }
    return(text);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  collectCall <- \(text, part)
  {
    if(is.null(part)) return(text)

    # collect the evaluation call
    this$call <- str2lang(part$ctx);

    # we have already replaced the LHS of `$`, but if there is nothing on
    # the RHS, then we will have a problem parsing
    pos <- part$end + 1L;
    one <- substr(text, pos, pos);
    if(one != "$")                     return(text);
    two <- substr(text, pos + 1L, pos + 1L);
    if(grepl("[[:alpha:].`\"']", two)) return(text);

    # remove the dollar
    substr(text, pos, pos) <- " ";
    return(text);
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
})
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

