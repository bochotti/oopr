## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @name oopr_completion
#' @title Completion for oopr
#' @include init.R
#' @export
#' @description
#' Code-completion / intellisense for `oopr` classes in RStudio.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
this <- new.env(parent = baseenv());
class(this) <- c("oopr_this", "oopr");

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @exportS3Method utils::.DollarNames oopr_this
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
.DollarNames.oopr_this <- \(x, pattern)
{
  comp <- OoprCompletion();
  if(comp$isRStudioCompletion())
  {
    return(comp$getNames());
  }
  NextMethod();
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @exportS3Method "$" oopr_this
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
`$.oopr_this` <- \(x, name)
{
  comp <- OoprCompletion();
  if(comp$isRStudioCompletion())
  {
    return(comp$obj@encl$this[[name]]);
  }
  NextMethod();
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Takes a file and parses its contents. It only evaluates oopr definitions.
#' Method to pull a class out using cursor position in document.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprSourceContext",,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
OoprSourceContext <- \(id = NULL, file = NULL, top = topenv(parent.frame()))
{
  if(!is.null(id))
  {
    this$loadFromId(id);
  }
  else if(!is.null(file))
  {
    this$loadFromFile(file);
  }
  else
  {
    return();
  }
  this$sourceFile(top);
}
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  get:id   <- \( ) { return(this$id_);   }
  set:id   <- \(x) { this$loadFromId(x);   return(x); }
  get:file <- \( ) { return(this$file_); }
  set:file <- \(x) { this$loadFromFile(x); return(x); }
  get:row  <- \( ) { return(this$row_);  }
  get:col  <- \( ) { return(this$col_);  }
  get:defs <- \( ) { return(this$defs_); }
  get:objs <- \( ) { return(this$objs_); }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  sourceFile <- \(top = topenv(parent.frame()), try = FALSE)
  {
    stopifnot(
      is.environment(top)
     ,is.logical(try) && length(try) == 1L && !is.na(try)
    );
    this$parseFile(try);
    this$evalExprs(top);
    return(invisible(this));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  getByPos <- \(row = NULL, col = NULL, which = c("at", "b4"), stop = TRUE)
  {
    if(is.null(row)) row <- this$row_;
    if(is.null(col)) col <- this$col_;
    stopifnot(
      is.numeric(row)  && length(row)  == 1L && row %% 1L == 0L
     ,is.numeric(col)  && length(col)  == 1L && col %% 1L == 0L
     ,is.logical(stop) && length(stop) == 1L && !is.na(stop)
    );
    which <- match.arg(which);

    objs <- this$objs_;
    defs <- this$defs_;

    for(i in seq_along(defs))
    {
      if(this$posIsInSrcRef(row, col, defs[[i]]))
      {
        if(which == "at") return(objs[[i]]) else return(objs[seq_len(i)]);
      }
    }

    if(stop)
    {
      stop(sprintf("row = %i & col = %i does not cover any oopr", row, col));
    }
    return(NULL);
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
protected:
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
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  id_   <- character(0L);
  file_ <- character(0L);
  text_ <- character(0L);
  row_  <- 1L;
  col_  <- 1L;
  defs_ <- list();
  objs_ <- list();

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  resetMembers <- \( )
  {
    this$id_   <- character(0L);
    this$file_ <- character(0L);
    this$row_  <- 1L;
    this$col_  <- 1L;
    this$defs_ <- list();
    this$objs_ <- list();
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  loadFromId <- \(id)
  {
    stopifnot(
      is.character(id) && length(id) == 1L
     ,this$rStudioIsAvailable()
    );
    context <- rstudioapi::getSourceEditorContext(id);
    if(is.null(context)) stop("`id` is not a valid document ID");
    this$resetMembers();
    this$id_   <- id;
    this$file_ <- context$path;
    this$text_ <- context$contents;
    this$row_  <- context$selection[[1]]$range$start[[1]];
    this$col_  <- context$selection[[1]]$range$start[[2]];
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  loadFromFile <- \(file)
  {
    stopifnot(is.character(file) && length(file) == 1L && file.exists(file));
    this$resetMembers();
    this$file_ <- file;
    this$text_ <- readLines(file, warn = FALSE);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  parseFile <- \(try = FALSE)
  {
    if(try)
    {
      parsed <- this$tryParse(
        # remove name after $
      {
        line <- text[this$row_];
        pos  <- gregexpr("\\$", line)[[1L]];
        pos  <- pos[which.max(pos >= this$col_)] - 1L;
        ptrn <- sprintf("(^.{%i})\\$(`(.*?)`|[[:alnum:]_.$]*)*", pos);
        line <- sub(ptrn, "\\1", line);
        text[this$row_] <- line;
      }
      , # add { ... } after control flows
      {
        ptrn <- "((if|for|while)\\s*(?'p'\\(([^()]|(?&p))*\\)))";
        line <- sub(ptrn, "\\1 { }", line, perl = TRUE);
        text[this$row_] <- line;
      }
      );
    }
    else if(nzchar(this$file_))
    {
      parsed <- parse(file = this$file_, keep.source = TRUE);
    }
    else
    {
      parsed <- parse(text = this$text_, keep.source = TRUE);
    }
    ats  <- findInExpr(parsed, \(e) is.ooprcall(e));
    defs <- rep_len(list(), length(ats));
    for(i in seq_along(ats))
    {
      defs[[i]]      <- parsed[[ats[[i]]]];
      names(defs)[i] <- match.call(oopr, defs[[i]])$name;
    }
    this$defs_ <- defs;
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  tryParse <- \(..., text = this$text_)
  {
    dots       <- as.list(substitute(list(...)))[-1L];
    err        <- NULL;
    for(i in seq_along(dots))
    {
      eval(dots[[i]]);
      tryCatch(
      {
        out <- parse(text = text, keep.source = TRUE);
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
    return(out)
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  evalExprs <- \(top)
  {
    env  <- new.env(parent = top);
    defs <- this$defs_;
    objs <- rep_len(list(), length(defs));
    for(i in seq_along(defs))
    {
      eval(defs[[i]], env, NULL);
      objs[[i]]                 <- env[[names(defs)[i]]];
      attr(defs[[i]], "srcref") <- attr(objs[[i]], "srcref", TRUE);
    }
    names(objs) <- names(defs);
    this$defs_  <- defs;
    this$objs_  <- objs;
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  posIsInSrcRef <- \(row, col, def)
  {
    src <- attr(def, "srcref", TRUE);
    if(src[1L]  < row && row <  src[3L]) return(TRUE);
    if(src[1L] == row && col >= src[5L]) return(TRUE);
    if(src[3L] == row && col <= src[6L]) return(TRUE);
    return(FALSE);
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#'
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprCompletion", private:OoprSourceContext,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
OoprCompletion <- \( )
{
  rm(list = ls(this$this), envir = this$this);
}
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:this <- \( ) { get0("this", envir = topenv()); }
  get:obj  <- \( ) { return(this$obj_); }
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  isRStudioCompletion <- \( )
  {
    if(!OoprSourceContext$rStudioIsAvailable()) return(FALSE);
    for(i in rev(seq_len(sys.nframe())))
    {
      if(iscall(sys.call(i), ".rs.getCompletionsDollar"))
      {
        return(this$cursorInClass(i));
      }
    }
    return(FALSE);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  getNames <- \( )
  {
    oopr  <- this$obj_@encl$this;
    class <- this$obj_@name;
    for(name in names(oopr))
    {
      # if(match(name, sprintf("%s%s", c("", "~"), class), 0L)) next;
      # symlink(oopr, "this", this$this, name);
    }
    class(oopr) <- c(class, "oopr");
    return(.DollarNames.oopr(oopr, sprintf("^~?%s$", class), TRUE));
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
  obj_  <- NULL;
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  cursorInClass <- \(pos)
  {
    if(!iscall(sys.call(pos - 1L), ".rs.rpc.get_completions")) return(FALSE);
    env <- sys.frame(pos - 1L);
    OoprSourceContext$id  <- env$documentId;
    OoprSourceContext$sourceFile(env$envir, try = TRUE);
    this$obj_ <- OoprSourceContext$getByPos(stop = FALSE);
    return(!is.null(this$obj_));
  }

})
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
