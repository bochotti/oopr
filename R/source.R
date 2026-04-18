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
    this$row_ <- x;
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
    this$col_ <- x;
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
  #' @returns
  #' Saves the `oopr` objects to `$objs`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  eval <- \(top)
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
    text <- character(0L);
    this$tryParse(
      # remove $
      {
        line <- text[this$row];
        if(grepl("\\$([^[:alpha:].`]|$)", substr(line, this$col - 1, this$col)))
        {
          substr(line, this$col - 1L, this$col - 1L) <- " ";
          this$col <- this$col - 1L;
        }
        text[this$row] <- line;
      }
      , # add { ... } after control flows
      {
        ptrn <- "((if|for|while)\\s*(?'p'\\(([^()]|(?&p))*\\)))";
        line <- sub(ptrn, "\\1 { }", line, perl = TRUE);
        text[this$row] <- line;
      }
    );
    this$replaceThis();
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
  replaceThis <- \( )
  {
    pd  <- utils::getParseData(this$expr);
    row <- this$row;
    col <- this$col - 1L;
    csr <- with(pd, {
      line1 <= row & row <= line2 & col1 <= col & col <= col2 & terminal
    });
    if(!any(csr)) return();
    id <- pd[csr, "id"];
    at <- this$walkParseData(pd, id);
    list <- format("list", width = 1L + at$col2 - at$col1);
    substr(this$text[row], at$col1, at$col2) <- list;
    OoprSource$parse();
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  walkParseData <- \(pd, id, up = TRUE)
  {
    i   <- match(id, pd$id, 0L);
    if(up)
    {
      j <- match(pd[["parent"]][i], pd$id, 0L);
    }
    else
    {
      j <- match(pd[["id"]][i], pd$parent, 0L);
      if(pd[j, "terminal"]) return(pd[j, ])
    }
    up <- pd[j, "terminal"];
    id <- pd[j, "id"];
    this$walkParseData(pd, id, up);
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
})
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname OoprSource
#' @description
#' RStudio specific source, relying on the output from
#' [`rstudioapi::getSourceEditorContext`].
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprSourceRStudio", public:OoprSourceTry,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
OoprSourceRStudio <- \(id = NULL)
{
  if(!is.null(id))
  {
    this$id <- id;
  }
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field id `character(1L)` \cr
  #'           The RStudio document ID.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:id <- \( )
  {
    return(this$id_);
  }
  set:id <- \(x)
  {
    stopifnot(is.character(x) && length(x) == 1L);
    if(!this$rStudioIsAvailable())
    {
      stop("RStudio and rstudioapi package must be available");
    }
    context <- rstudioapi::getSourceEditorContext(x);
    if(is.null(context))
    {
      stop(sprintf("id %s is not a valid document ID", deparse1(x)));
    }
    this$file <- context$path;
    this$text <- context$contents;
    this$row  <- context$selection[[1]]$range$start[[1]];
    this$col  <- context$selection[[1]]$range$start[[2]];
    this$id_  <- x;
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Check if RStudio is in session.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  rStudioIsAvailable <- \( )
  {
    return(
         requireNamespace("rstudioapi", quietly = TRUE)
      && identical(.Platform$GUI, "RStudio")
    );
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
  id_ <- character(0L);

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
})
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

