## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @name OoprBreakpointsImpl
#' @include init.R
#' @include container.R
#' @include source.R
#' @keywords internal .Rbuildignore
#' @title Breakpoint Internals
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
NULL

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname OoprBreakpointsImpl
#' @description
#' Map srcref line numbers.
#'
#' @details
#' Utilises a longest common subsequence algorithm.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprBreakpointsMap",,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @param old `function` \cr
  #'            Old function.
  #'
  #' @param new `function` \cr
  #'            New function.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  OoprBreakpointsMap <- \(old, new)
  {
    this$load(old, new);
    return(invisible(this));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field map `integer()` \cr
  #'            The lines which can be set.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:map <- \( )
  {
    return(this$map_ + this$nst_);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprBreakpointsMap$OoprBreakpointsMap
  #' @description
  #' Add a new map.
  #'
  #' @returns
  #' `this` invisibly.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  load <- \(old, new)
  {
    old <- attr(old, "srcref", TRUE);
    new <- attr(new, "srcref", TRUE);
    this$ost_ <- (old[1L] %||% 1L) - 1L;
    this$nst_ <- (new[1L] %||% 1L) - 1L;
    if(identical(old, new) && !is.null(old) && !is.null(new))
    {
      this$map_ <- seq_len(old[3L] - old[1L] + 1);
    }
    else
    {
      this$map_ <- .Call(Cpp_LCS, as.character(old), as.character(new));
    }
    return(invisible(this));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Map the new lines to the old lines.
  #'
  #' @param lines `integer()` \cr
  #'        The new line numbers.
  #'
  #' @returns
  #' `integer(length(lines))`. If a new line does not exist in the old lines,
  #' then `NA`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  toOld <- \(lines)
  {
    return(match(lines - this$nst_, this$map_) + this$ost_);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Map the old lines to the new lines.
  #'
  #' @param lines `integer()` \cr
  #'        The new old numbers.
  #'
  #' @returns
  #' `integer(length(lines))`. If an old line does not exist in the new lines,
  #' then `NA`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  toNew <- \(lines)
  {
    return(this$map_[lines - this$ost_] + this$nst_);
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  map_ <- integer(0L);
  ost_ <- integer(1L);
  nst_ <- integer(1L);

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
}) ## OoprBreakpointsMap
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname OoprBreakpointsImpl
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprBreakpointsBreak",,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @param body `language` \cr
  #'             The body of the function to set the breakpoint.
  #' @param line `integer(1L)` \cr
  #'             The line number.
  #' @param cond `langauge` \cr
  #'             The condition that triggers the breakpoint.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  OoprBreakpointsBreak <- \(body, line, cond)
  {
    this$line_  <- line;
    this$steps_ <- this$findSteps(body, line);
    this$cond_  <- cond;
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:line <- \( )
  {
    return(this$line_);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:cond <- \( )
  {
    return(this$cond_);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field steps `integer()` \cr
  #'              The steps required to set breakpoint at `$line`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:steps <- \( )
  {
    return(this$steps_);
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  line_  <- integer(0L);
  steps_ <- integer(0L);
  cond_  <- logical(0L);

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' The srcrefs inside the `body` are searched to find where the line
  #' can be found. Steps is the integer position to enter the body of the
  #' function, e.g. `body(fun)[[steps]]`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  findSteps <- \(x, line)
  {
    if(!is.call(x)) { return(integer(0L)); }
    srcs <- attr(x, "srcref", TRUE);
    for(i in seq_along(x))
    {
      src <- srcs[[i]];
      if(!is.null(src) && !(src[1L] <= line && line <= src[3L])) { next; }
      nest <- this$findSteps(x[[i]], line);
      if(length(nest))                          { return(c(i, nest)); }
      if(!is.null(src) && !isname(x[[i]], "{")) { return(i); }
    }
    return(integer(0L));
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
}) ## OoprBreakpointsBreak
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname OoprBreakpointsImpl
#' @description
#' Used to track changes to breakpoints among files/classes/functions.
#'
#' @details
#' Holds vector of value, and/or container of the same class.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprBreakpointsCond",, { })
oopr("OoprBreakpointsCond",,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Add a value.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  push <- \(x)
  {
    this$value_ <- c(this$value_, x);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Clear all values & children.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  clear <- \( )
  {
    this$value_ <- NULL;
    this$child_$resize();
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Create a child class.
  #'
  #' @param ... `character()` \cr
  #'            Key of the new class in the container, pasted together.
  #'
  #' @returns
  #' The new child class.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  child <- \(...)
  {
    key <- paste(...);
    if(!this$child_$exists(key))
    {
      this$child_$emplace(key);
    }
    return(this$child_[key]);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Convert contents to a last
  #'
  #' @returns
  #' `list()`
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  list <- \( )
  {
    out <- this$value_;
    if(this$child_$size)
    {
      out <- c(out, this$child_$apply(\(k, v) v$list()));
    }
    return(out);
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
  value_  <- NULL;
  child_  <- OoprBreakpointsCond[[]];
  parent_ <- NULL;

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
}) ## OoprBreakpointsCond
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname OoprBreakpointsImpl
#' @description
#' Represents a function object inside a class.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprBreakpointsFunction",,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @param fun `function` \cr
  #'            The function to set breakpoints for.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  OoprBreakpointsFunction <- \(fun)
  {
    stopifnot(is.function(fun));
    this$fun_ <- fun;
    this$map_(fun, fun);
  }
  ~OoprBreakpointsFunction <- \( )
  {
    if(this$breaks_$size) { try(this$rm(this$breaks$lines), TRUE); }
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field fun `function` \cr
  #'            The method or active binding.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:fun <- \( )
  {
    return(this$fun_);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field body `expression` \cr
  #'             Returns the body of `$fun` without any breakpoints.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:body <- \( )
  {
    fun  <- this$fun;
    body <- body(fun);
    body <- attr(body, "orig", TRUE) %||% body;
    return(body);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field srcref `srcref` \cr
  #'               The srcref for `$fun`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:srcref <- \( )
  {
    return(attr(this$fun, "srcref", TRUE));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field breaks `data.frame` \cr
  #'               The `line`, `steps` and `cond` of each set breakpoint.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:breaks <- \( )
  {
    nrow <- this$breaks_$size;
    out  <- list(
      line  = integer(nrow)
     ,steps = rep_len(list(), nrow)
     ,cond  = rep_len(list(), nrow)
    );
    i <- 1L;
    for(brk in this$breaks_$data)
    {
      for(col in names(out))
      {
        out[[col]][[i]] <- brk[[col]];
      }
      i <- i + 1L;
    }
    out$line <- this$map_$toNew(out$line);
    return(list2DF(out));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Check whether `$srcref` matches the source file.
  #'
  #' @details
  #' The source file is read, and cut by the positions of `$srcref`.
  #'
  #' @returns
  #' `logical(1L)`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  synced <- \( )
  {
    srcref <- this$srcref;
    if(is.null(srcref)) { return(FALSE); }
    file <- attr(srcref, "srcfile", TRUE)$filename;
    if(is.null(file) || !file.exists(file)) { return(FALSE); }

    inClass <- as.character(srcref);
    inFile                 <- readLines(file, warn = FALSE);
    inFile                 <- inFile[srcref[1L]:srcref[3L]];
    inFile[length(inFile)] <- substr(inFile[length(inFile)], 1L, srcref[4L]);
    inFile[1L]             <- substr(inFile[1L], srcref[2L], nchar(inFile[1L]));

    return(identical(inFile, inClass));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Check whether a line number is inside the function.
  #'
  #' @param lines `integer()` \cr
  #'              The line numbers.
  #'
  #' @param old   `logical(1L)` \cr
  #'              Whether the lines have already been mapped to old lines.
  #'
  #' @returns
  #' `logical(length(lines))`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  has <- \(lines, old = FALSE)
  {
    out    <- logical(length(lines));
    srcref <- this$srcref;
    if(is.null(srcref) || this$nonCurl) { return(out); }

    lines <- suppressWarnings(as.numeric(lines));
    if(!old)
    {
      lines <- this$map_$toOld(lines);
    }
    body <- this$body;
    for(i in seq_along(lines))
    {
      line   <- lines[i];
      out[i] <-
      (
           !is.na(line)
        && srcref[1L] <= line && line <= srcref[3L]
        && length(OoprBreakpointsBreak(body, line, TRUE)$steps)
      );
    }
    return(out);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprBreakpointsFunction$has
  #' @description
  #' Add breakpoints to the function
  #'
  #' @param lines `integer()` \cr
  #'              Line numbers to add.
  #'
  #' @param conds `list()` \cr
  #'              A list of unevaluated expressions representing the
  #'              condition of each breakpoint. Expressions must evaluate to
  #'              `logical(1L)`. Defaults to all lines being `TRUE`.
  #'
  #' @details
  #' Existing breakpoints are removed and reapplied.
  #'
  #' @returns
  #' `this` invisibly.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  add <- \(lines, conds = list(), old = FALSE)
  {
    if(!length(conds))
    {
      conds <- !logical(length(lines));
    }
    else if(length(lines) != length(conds))
    {
      stop("`lines` and `conds` must be the same length")
    }
    if(this$nonCurl)
    {
      stop("cannot set a breakpoint for a non { body");
    }
    if(!old)
    {
      has <- this$has(lines);
      if(!all(has))
      {
        stop(sprintf(
          "cannot set `lines` %s, available lines are %s"
         ,deparse(lines[!has]), deparse1(this$map_$map)
        ));
      }
      lines <- this$map_$toOld(lines);
    }
    this$rm(lines, old = TRUE);
    body <- this$body;
    for(i in seq_along(lines))
    {
      line <- lines[i];
      key  <- as.character(line);
      this$breaks_$emplace(key, body, lines[i], conds[[i]]);
      steps <- this$breaks_$apply(\(k, v) v$steps);
      if(anyDuplicated.default(steps) || !length(steps[[key]]))
      {
        this$breaks_$erase(key);
      }
    }
    this$setBreaks();
    return(invisible(this));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprBreakpointsFunction$has
  #' @description
  #' Remove breakpoints.
  #'
  #' @param lines `integer()` \cr
  #'              Line numbers to remove. The default unsets all lines.
  #'
  #' @returns
  #' `this` invisibly.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  rm <- \(lines = integer(), old = FALSE)
  {
    if(!length(lines))
    {
      lines <- unlist(this$breaks_$apply(\(k, v) { v$line; }));
    }
    else if(!old)
    {
      lines <- this$map_$toOld(lines);
    }

    lines <- as.character(lines);
    for(line in lines) if(this$breaks_$exists(line))
    {
      this$breaks_$erase(line);
    }
    this$setBreaks();
    return(invisible(this));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Update the srcref mapping or load a new function.
  #'
  #' @param fun  `function` \cr
  #'             The function to load.
  #'
  #' @param new  `logical(1L)` \cr
  #'             Whether to over-write `$fun`.
  #'
  #' @param cond `OoprBreakpointsCond` \cr
  #'             Condition to track changes.
  #'
  #' @returns
  #' `this` invisibly.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  load <- \(fun, new = FALSE, cond = OoprBreakpointsCond())
  {
    lines <- as.integer(unlist(this$breaks_$apply(\(k, v) { v$line; })));
    old   <- this$map_$toNew(lines);
    this$map_$load(this$fun, fun);
    map   <- this$map_$toNew(lines);
    if(new)
    {
      conds <- this$breaks_$apply(\(k, v) { v$cond; });
      this$rm();
      this$fun_ <- fun;
      this$map_$load(fun, fun);
      if(this$nonCurl)
      {
        map[seq_along(map)] <- NA_integer_;
      }
      else if(!all(is.na(map)))
      {
        this$add(map[!is.na(map)], conds[!is.na(map)]);
      }
    }
    else if(any(is.na(map)))
    {
      this$rm(lines[is.na(map)], old = TRUE);
    }
    if(!identical(old, map))
    {
      cond$push(this$makeChangeChar(old, map));
    }
    return(invisible(this));
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  fun_    <- NULL;
  breaks_ <- OoprBreakpointsBreak[[]];
  map_    <- OoprBreakpointsMap;

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:nonCurl <- \( )
  {
    return(!iscall(this$body, "{"))
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  setBreaks <- \( )
  {
    fun   <- this$fun;
    ats   <- this$breaks_$apply(\(k, v) v$steps);
    conds <- this$breaks_$apply(\(k, v) v$cond);
    .Call(Cpp_set_breakpoints, fun, ats, conds);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  makeChangeChar <- \(old, new)
  {
    lo <- max(nchar(old), 0L);
    ln <- max(nchar(new[!is.na(new)]), 0L);
    old <- sprintf("%*i", lo, old);
    new <- ifelse(is.na(new), "[removed]", sprintf("-> %*i", ln, new));
    return(sprintf("%s %s", old, new));
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
}) ## OoprBreakpointsFunction
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname OoprBreakpointsImpl
#' @description
#' Represents a class inside a file.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprBreakpointsClass",,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @param ooprC `ooprC` \cr
  #'              The `ooprC` object.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  OoprBreakpointsClass <- \(ooprC)
  {
    this$load(ooprC, TRUE);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:ooprC <- \( )
  {
    return(this$ooprC_);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field name `character(1L)` \cr
  #'        The name of the class.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:name <- \( )
  {
    return(this$ooprC@name);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprBreakpointsFunction$breaks
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:breaks <- \( )
  {
    out <- this$funs_$apply(\(k, v)
    {
      breaks <- v$breaks;
      breaks$name <- if(nrow(breaks)) k else character(0L);
      return(breaks);
    });
    out <- list2DF(.mapply(c, out, list(use.names = FALSE)));
    names(out) <- c("line", "steps", "cond", "name");
    return(out)
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Check if functions are in sync.
  #'
  #' @param names `character()` \cr
  #'              Functions to check. By default, checks all functions.
  #'
  #' @returns
  #' `logical(1L)`
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  synced <- \(names = character(0L))
  {
    if(!length(names))
    {
      names <- this$funs_$keys;
    }
    for(name in names)
    {
      if(this$funs_$exists(name) && !this$funs_[name]$synced())
      {
        return(FALSE);
      }
    }
    return(TRUE);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprBreakpointsFunction$has
  #' @description
  #' Check if a any class' functions contains a line.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  has <- \(lines)
  {
    out <- logical(length(lines));
    for(i in seq_along(lines))
    {
      for(name in this$funs_$keys)
      {
        if(out[i]) { next; }
        out[i] <- this$funs_[name]$has(lines[i]);
      }
    }
    return(out);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' See which function contains the line.
  #'
  #' @param lines `integer()` \cr
  #'              The line numbers.
  #'
  #' @return
  #' `character(length(lines))`, being the function names. If `line` is not
  #' covered, then a zero-character string.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  where <- \(lines)
  {
    out <- character(length(lines));
    for(i in seq_along(lines))
    {
      for(name in this$funs_$keys)
      {
        if(nzchar(out[i])) { next; }
        if(this$funs_[name]$has(lines[i]))
        {
          out[i] <- name;
        }
      }
    }
    return(out);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprBreakpointsFunction$add
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  add <- \(lines, conds)
  {
    i <- split.default(seq_along(lines), this$where(lines));
    i <- i[nzchar(names(i))];
    for(nm in names(i))
    {
      this$funs_[nm]$add(lines[i[[nm]]], conds[i[[nm]]]);
    }
    return(invisible(this));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprBreakpointsFunction$rm
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  rm <- \(lines = integer(0L))
  {
    if(!length(lines))
    {
      this$funs_$apply(\(k, v) v$rm(lines));
      return();
    }
    i <- split.default(seq_along(lines), this$where(lines));
    i <- i[nzchar(names(i))];
    for(nm in names(i))
    {
      this$funs_[nm]$rm(lines[i[[nm]]]);
    }
    return(invisible(this));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprBreakpointsFunction$load
  #' @description
  #' Update the srcref mapping or load a new class.
  #'
  #' @param ooprC `ooprC` \cr
  #'              The function to load.
  #'
  #' @param new   `logical(1L)` \cr
  #'              Whether to over-write `$ooprC`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  load <- \(ooprC, new = FALSE, cond = OoprBreakpointsCond())
  {
    stopifnot(is.ooprC(ooprC));
    meta <- ooprC@meta;
    thiz <- ooprC@encl$this;
    keys <- this$funs_$keys;
    for(key in keys)
    {
      i   <- which(meta$names$subs(key));
      fun <- this$getFun(i, key, meta, thiz);
      if(is.null(fun))
      {
        this$funs_$erase(key);
        cond$child(key, "[removed]");
      }
      else
      {
        this$funs_[key]$load(fun, new, cond$child(key, "[changed]"));
      }
    }
    if(new)
    {
      for(i in seq_len(meta$size))
      {
        name <- meta$names$get(i);
        if(match(name, keys, 0L)) { next; }
        fun <- this$getFun(i, name, meta, thiz);
        if(!is.null(fun))
        {
          this$funs_$emplace(name, fun);
          cond$child(name, "[added]");
        }
      }
      this$ooprC_ <- ooprC;
    }
    return(invisible(this));
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  ooprC_ <- NULL;
  funs_  <- OoprBreakpointsFunction[[]];

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  getFun <- \(i, name, meta, thiz)
  {
    if(!length(i) || nzchar(meta$inherit$get(i))) { return(NULL); }
    if(nzchar(meta$property$get(i)))
    {
      return(tryCatch(activeBindingFunction(name, thiz), error = \(e) NULL));
    }
    else if(meta$method$get(i))
    {
      return(get0(name, thiz, "function", FALSE));
    }
    return(NULL);
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
}) ## OoprBreakpointsClass
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname OoprBreakpointsImpl
#' @description
#' Represents a source file that contains `oopr` classes.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprBreakpointsFile", OoprSource,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @param file `character(1L)` \cr
  #'             The name of the source file. Will be normalized
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  OoprBreakpointsFile <- \(file)
  {
    OoprSource$file <- this$apath(file);
    this$pkg_       <- this$getPackageName(file);
    this$load(TRUE);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field file `character(1L)` \cr
  #'             The file path relative to the working directory.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:file <- \( )
  {
    tryCatch(
    {
      split <- \(x) strsplit(x, "/")[[1L]];
      path <- split(OoprSource$file);
      rltv <- split(getwd());

      if(.Platform$OS.type == "windows" && path[1L] != rltv[1L]) { stop(); }

      dist <- min(length(path), length(rltv));
      for(i in seq_len(dist))
      {
        if(rltv[i] != path[i])
        {
          dist <- max(1L, i - 1L);
          break;
        }
      }

      out <- c(".", rep("..", length(rltv) - dist), path[-seq_len(dist)]);
      return(do.call(file.path, as.list(out)));
    }
    ,error = \(e)
    {
      return(OoprSource$file);
    })
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @exportS3Method "==" OoprBreakpointsFile
  #' @description
  #' Check file equivalence.
  #'
  #' @param e2 `character(1L)` \cr
  #'           A file path.
  #'
  #' @returns
  #' `logical(1L)`
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  S3:`==` <- \(e2)
  {
    return(identical(OoprSource$file, this$apath(e2)));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field pkg `character(1L)` \cr
  #'            The package name of `$file`, if applicable.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:pkg <- \( )
  {
    return(this$pkg_);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field top `environment` \cr
  #'            If `nzchar(pkg)` then the namespace, otherwise the global
  #'            environment. `getNamespace` is called with each access, as to
  #'            not carry references to (older) namespaces if re-loaded.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:top <- \( )
  {
    pkg <- this$pkg_;
    if(nzchar(pkg) && requireNamespace(pkg, quietly = TRUE))
    {
      return(getNamespace(pkg));
    }
    else
    {
      return(globalenv());
    }
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field reloaded `logical(1L)` \cr
  #'                 Whether `$top` namespace differs.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:reloaded <- \( )
  {
    if(!this$classes_$size) { return(TRUE); }
    if(nzchar(this$pkg_))
    {
      top <- topenv(this$classes_$data[[1L]]$ooprC@encl);
      return(!identical(top, this$top));
    }
    for(class in this$classes_$keys)
    {
      ooprC1 <- this$classes_[class]$ooprC;
      ooprC2 <- get0(class, this$top, inherits = FALSE)
      if(!identical(ooprC1, ooprC2)) { return(TRUE); }
    }
    return(FALSE);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field timestamp `POSIXct(1L)` \cr
  #'                  The last time the source file was read.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  timestamp <- .POSIXct(0);

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field modified `logical(1L)` \cr
  #'                 Whether `$file` has been modified since the last load.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:modified <- \( )
  {
    return(file.mtime(this$file) > this$timestamp);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprBreakpointsClass$breaks
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:breaks <- \( )
  {
    out <- this$classes_$apply(\(k, v)
    {
      breaks <- v$breaks;
      breaks$class <- if(nrow(breaks)) k else character(0L);
      return(breaks);
    });
    out <- list2DF(.mapply(c, out, list(use.names = FALSE)));
    names(out) <- c("line", "steps", "cond", "name", "class");
    return(out)
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprBreakpointsClass$has
  #'
  #' @description
  #' Check if any class inside the file contains a function and/or a line.
  #'
  #' @returns
  #' Named `logical()` the same length as `lines`, indicating whether any
  #' class holds the line.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  has <- \(lines)
  {
    out <- logical(length(lines));
    for(i in seq_along(lines))
    {
      for(class in this$classes_$keys)
      {
        if(out[i]) { next; }
        out[i] <- this$classes_[class]$has(lines[i]);
      }
    }
    return(out);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprBreakpointsClass$where
  #' @description
  #' See which class contains a line.
  #'
  #' @returns
  #' `character(length(lines))`, being the class name/s. If a line is not
  #' covered, then a zero character string.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  where <- \(lines)
  {
    out <- character(length(lines));
    for(i in seq_along(lines))
    {
      for(class in this$classes_$keys)
      {
        if(nzchar(out[i])) { next; }
        if(this$classes_[class]$has(lines[i]))
        {
          out[i] <- class;
        }
      }
    }
    return(out);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprBreakpointsClass$synced
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  synced <- \(names = character(0L))
  {
    synced <- this$classes_$apply(\(key, val) { val$synced(names); });
    return(unlist(synced));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprBreakpointsClass$add
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  add <- \(lines, conds)
  {
    i <- split.default(seq_along(lines), this$where(lines));
    i <- i[nzchar(names(i))];
    for(nm in names(i))
    {
      this$classes_[nm]$add(lines[i[[nm]]], conds[i[[nm]]]);
    }
    return(invisible(this));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprBreakpointsClass$rm
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  rm <- \(lines = integer(0L))
  {
    if(!length(lines))
    {
      this$classes_$apply(\(k, v) v$rm(lines));
    }
    else
    {
      i <- split.default(seq_along(lines), this$where(lines));
      i <- i[nzchar(names(i))];
      for(nm in names(i))
      {
        this$classes_[nm]$rm(lines[i[[nm]]]);
      }
    }
    return(invisible(this));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprBreakpoints$class$load
  #'
  #' @param new `logical(1L)` \cr
  #'            Whether to obtain new `ooprC`s from `$env`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  load <- \(new = this$reloaded, cond = OoprBreakpointsCond())
  {
    tryCatch(
    {
      OoprSource$parse();
    }
    ,error = \(e)
    {
      class(e) <- c("OoprBreakpointsParseError", class(e));
      stop(e);
    });

    if(!new) tryCatch(
    {
      OoprSource$eval(top = this$top);
    }
    ,error = \(e)
    {
      class(e) <- c("OoprBreakpointsCompileError", class(e));
      stop(e);
    });

    names <- names(OoprSource$defs);
    objs  <- OoprSource$objs;
    keys  <- this$classes_$keys;
    for(key in keys)
    {
      if(!match(key, names, 0L))
      {
        this$classes_$erase(key);
        cond$child(key, "[removed]");
      }
      else
      {
        ooprC <- if(new) get0(key, this$top, "any", FALSE) else objs[[key]];
        if(!is.ooprC(ooprC, key))
        {
          this$classes_$erase(key);
          cond$child(key, "[removed]");
        }
        else
        {
          this$classes_[key]$load(ooprC, new, cond$child(key, "[changed]"));
        }
      }
    }
    if(new) for(name in names)
    {
      if(match(name, keys, 0L)) { next; }
      ooprC <- get0(name, this$top, "any", FALSE);
      if(is.ooprC(ooprC, name))
      {
        this$classes_$emplace(name, ooprC);
        cond$child(name, "[added]");
      }
    }
    this$timestamp <- file.mtime(this$file);
    return(invisible(this));
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  pkg_     <- character(1L);
  classes_ <- OoprBreakpointsClass[[]];

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  apath <- \(path)
  {
    tryCatch(
    {
      return(suppressWarnings(normalizePath(path, "/")));
    }
    ,error = \(e)
    {
      return(path);
    });
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  getPackageName <- \(file)
  {
    file <- this$apath(file);
    dir  <- dirname(file);
    if(basename(dir) != "R") { return(character(1L)); }
    root <- dirname(dir);
    desc <- file.path(root, "DESCRIPTION");
    if(!file.exists(desc))   { return(character(1L)); }
    pkg  <- as.vector(read.dcf(desc, fields = "Package"));
    if(!length(pkg))         { return(character(1L)); }
    return(pkg);
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
}) ## OoprBreakpointsFile
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname OoprBreakpointsImpl
#' @description
#' Implementation of `oopr` breakpoints.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprBreakpointsImpl",,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field volatile `logical(1L)` \cr
  #'                 Whether to automatically update breakpoints when changes
  #'                 to source files or environments occur. Updates only
  #'                 occur when this class is interacted with.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:get:volatile <- \( )
  {
    return(this$volatile_);
  }
  static:set:volatile <- \(x)
  {
    stopifnot(is.logical(x) && length(x) == 1L && !is.na(x))
    this$volatile_ <- x;
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field quiet `logical(1L)` \cr
  #'              Whether to silence messages when breakpoints auto update.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:get:quiet <- \( )
  {
    return(this$quiet_);
  }
  static:set:quiet <- \(x)
  {
    stopifnot(is.logical(x) && length(x) == 1L && !is.na(x))
    this$quiet_ <- x;
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field breaks `data.frame(6L)` \cr
  #'               Currently set breakpoints; giving the file, class, name,
  #'               line, steps, and condition. File paths are always relative
  #'               to the current working directory. Lines displayed are
  #'               referred to the last modified state of the file, not
  #'               necessarily to a functions `srcref`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:get:breaks <- \( )
  {
    out <- list();
    if(this$files_$size)
    {
      this$checkForChanges();
      out <- this$files_$apply(\(v)
      {
        breaks <- v$breaks;
        breaks$file <- if(nrow(breaks)) v$file else character(0L);
        return(breaks);
      });
      out <- .mapply(c, out, list(use.names = FALSE));
    }
    else
    {
      out <- rep.int(list(integer(0L), list(), character(0L)), 1:3);
    }
    names(out) <- c("line", "steps", "cond", "name", "class", "file");
    out <- out[c(6:4, 1:3)];
    out <- list2DF(out);
    out <- out[order(out$file, out$line), ]
    class(out) <- c("OoprBreakpointsBreak", "data.frame");
    return(out)
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprBreakpointsFile$add
  #' @description
  #' Add breakpoints.
  #'
  #' @param file  `character(1L)` \cr
  #'              Path to a `.R` file.
  #'
  #' @details
  #' If `$volatile == TRUE`, then `lines` must align with the most recent
  #' modified version of `file`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:add <- \(file, lines, conds = list())
  {
    stopifnot(
      is.character(file) && length(file)  == 1L && file.exists(file)
     ,is.numeric(lines)  && length(lines) >= 1L && all(lines %% 1L == 0L)
    );
    conds <- substitute(conds);
    if(!iscall(conds, "list"))
    {
      stop("`conds` must be a list of expressions");
    }
    conds <- conds[-1L];
    if(!length(conds))
    {
      conds <- as.list(!logical(length(lines)));
    }
    else if(length(conds) != length(lines))
    {
      stop("`conds` must be the same length as `lines`");
    }

    this$checkForChanges();
    i <- this$which(file);
    if(!i)
    {
      this$files_$emplace(file = file);
      i <- this$files_$size;
    }

    this$files_[i]$add(lines, conds);
    succ <- match(lines, this$files_[i]$breaks$line, 0L);
    if(!all(succ))
    {
      warning(sprintf("Lines %s unable to be set", deparse1(lines[!succ])));
    }
    return(invisible(this));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprBreakpointsFile$rm
  #' @inherit OoprBreakpointsImpl$add
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:rm <- \(file, lines = integer(0L))
  {
    stopifnot(
      is.character(file) && length(file)  == 1L && file.exists(file)
     ,is.numeric(lines)  && all(lines %% 1L == 0L)
    );
    this$checkForChanges();
    i <- this$which(file);
    if(i)
    {
      this$files_[i]$rm(lines);
      if(!length(lines)) { this$files_$erase(i); }
    }
    return(invisible(this));
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
protected:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  OoprBreakpointsImpl <- \( ) { }
  static:files_       <- OoprBreakpointsFile[];

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:which <- \(path)
  {
    i <- 1L;
    for(file in this$files_$data)
    {
      if(file == path) { return(i); }
      i <- i + 1L;
    }
    return(0L);
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:quiet_    <- FALSE;
  static:volatile_ <- TRUE;
  static:cond_     <- OoprBreakpointsCond;

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:checkForChanges <- \( )
  {
    if(!this$volatile_) { return(); }
    on.exit({this$cond_$clear()});
    size <- this$files_$size;
    if(size) for(i in seq.int(size, 1))
    {
      file <- this$files_[i];
      if(!file.exists(file$file))
      {
        this$files_$erase(i);
        this$cond_$child(file$file, "[removed]");
      }
      else if(file$modified || file$reloaded)
      {
        tryCatch(
        {
          file$load(cond = this$cond_$child(file$file, "[changed]"));
        }
        ,OoprBreakpointsParseError   = \(e) { this$toWarn(e, "parsing");   }
        ,OoprBreakpointsCompileError = \(e) { this$toWarn(e, "compiling"); }
        );
      }
    }
    if(!this$quiet_)
    {
      list <- this$cond_$list();
      if(length(list))
      {
        msg <- paste(collapse = "\n", capture.output(tree(list)));
        msg <- sprintf("OoprBreakpoints changes\n%s\n", msg);
        msg <- simpleMessage(msg, call = NULL);
        class(msg) <- c("OoprBreakpointsChange", class(msg));
        message(msg);
      }
    }
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:toWarn <- \(e, x)
  {
    e$call    <- quote(OoprBreakpoints);
    e$message <- sprintf("Error %s:\n%s", x, e$message);
    class(e)[2:3] <- c("simpleWarning", "warning");
    warning(e);
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
}) ## OoprBreakpointsImpl
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname OoprBreakpointsImpl
#' @description
#' The controller for RStudio breakpoints.
#'
#' @details
#' RStudio's process of setting a breakpoint is:
#'   1. Check the function is in sync.
#'   2. Get the steps of the functions body to arrive at a line.
#'   3. Get the environment that the function is assigned to.
#'   4. Set the breakpoints into the function.
#' Only steps 3 & 4 apply when unsetting a breakpoint, or reloading a file.
#'
#'
#' RStudio maintains its own record of breakpoints and is recorded by
#' file and function name. However, multiple classes in a file could share a
#' function name.
#'
#' TODO: newer versions of RStudio use `.rs.setBreakpoint`.
#'
#' @section RStudio:
#' Setting breakpoints inside `oopr` classes is the same process as setting
#' them for normal functions - use the gutter or `SHIFT+F9`.
#'
#' RStudio calls its R functions inside `tools:rstudio` from the global
#' environment. By assigning functions of the same name, these calls can be
#' intercepted.
#'
#' This feature is disabled by default as it is potentially destructive for
#' an R session. Environmental variable `R_OOPR_BREAKPOINTS=true` can be used
#' to enable this functionality when `oopr` namespace is loaded.
#'
#' Breakpoints cannot be set on single line functions with a specifier,
#' ```
#' get:x <- \( ) { return("x"); }
#' ````
#' as RStudio thinks the line is at the top-level.
#'
#' Tested with RStudio version 2026.1.1.403, there are no guarantees for
#' earlier or later versions.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprBreakpointsRStudio", OoprBreakpointsImpl,             # nocov start #
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:load <- \( )
  {
    if(exists(".rs.setBreakpoint", envir = this$rs, inherits = FALSE))
    {
      stop("Not supported in this version of RStudio");
    }
    for(fun in names(this$funs_))
    {
      rsFun <- this$funs_[fun];
      assign(rsFun, this[[fun]], envir = globalenv());
    }
    return(TRUE);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:unload <- \( )
  {
    loaded <- vapply(names(this$funs_), this$isLoadedInGlobal, logical(1L));
    loaded <- names(loaded)[loaded];
    rm(list = this$funs_[loaded], envir = globalenv());
    return(TRUE);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Checks whether the srcref matches the file.
  #'
  #' @param name `character(1L)` \cr
  #'             Name of the function.
  #'
  #' @param file `character(1L)` \cr
  #'             Path of the file.
  #'
  #' @param pkg  `character(1L)` \cr
  #'             The name of the package, or `"R_GlobalEnv"`.
  #'
  #' @details
  #' The first function to be called when setting a breakpoint.
  #'
  #' @seealso
  #' `.rs.isFunctionInSyncImpl`
  #'
  #' @returns
  #' `logical(1L)`
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:isFunctionInSync <- \(name, file, pkg)
  {
    # if a function exists in the namespace, check it first
    if(this$isNonClassFunction(name, pkg))
    {
      if(!this$rsFun("isFunctionInSync", name, file, pkg)) { return(FALSE); }
    }

    # now check for ooprs in the file
    name <- as.character(str2lang(name));
    i <- this$which(file);
    if(!i)
    {
      this$files_$emplace(file = file);
      i <- this$files_$size;
    }
    file <- this$files_[i];
    if(file$reloaded) { file$load(); }
    synced <- file$synced(name);
    if(!length(synced))
    {
      this$files_$erase(i);
    }
    this$inSync_ <- all(synced);
    return(this$inSync_);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprBreakpoints$isFunctionInSync
  #'
  #' @description
  #' Gets the "steps" required for a line.
  #'
  #' @param lines `list()` \cr
  #'              A list of `integer(1L)` of the line numbers to be added.
  #'
  #' @details
  #' The trick here is to prefix the steps with `"oopr:"` as RStudio does
  #' *not* validate the `steps` field. The function setting the breakpoints
  #' will know which breakpoints are for `oopr` classes.
  #'
  #' @seealso
  #' `.rs.getFunctionSteps`
  #'
  #' @returns
  #' A `list` of
  #' `list(name = character(1L), line = integer(1L), at = character(1L))`,
  #' where `at` is in the form `"oopr:line"` if relating to an `oopr` class.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:getFunctionSteps <- \(name, file, pkg, lines)
  {
    out <- list();
    if(!this$inSync_) { return(out); }
    on.exit(this$inSync_ <- FALSE);

    if(this$isNonClassFunction(name, pkg))
    {
      out <- this$rsFun("rpc.get_function_steps", name, file, pkg, lines);
    }

    i <- this$which(file);
    if(!i) { return(out); }

    classes <- this$files_[i]$where(lines);
    for(i in seq_along(classes))
    {
      cls  <- classes[i];
      line <- lines[[i]];
      if(!nzchar(cls)) { next; }
      at   <- sprintf("oopr:%i", line);
      part <- list(name = name, line = line, at = at);
      part <- lapply(part, `class<-`, "rs.scalar");
      out  <- c(out, list(part));
    }
    return(out);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprBreakpoints$isFunctionInSync
  #'
  #' @description
  #' Gets the environment of a function.
  #'
  #' @details
  #' This is called first when unsetting a breakpoint.
  #'
  #' Returning `emptenv()` ensures that the next step is called. This can
  #' be called multiple times, so I only want to do this once.
  #'
  #' @returns
  #' `environment`
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:getEnvironmentOfFunction <- \(name, file, pkg)
  {
    out  <- this$rsFun("getEnvironmentOfFunction", name, file, pkg);

    # only do this the first time
    if(!nzchar(pkg))
    {
      if(this$which(file))
      {
        this$file_ <- file;
        out <- out %||% emptyenv();
      }
    }
    return(out);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprBreakpoints$isFunctionInSync
  #'
  #' @description
  #' Sets the breakpoints for a function.
  #'
  #' @param env   `character(1L)` \cr
  #'              The environment that holds function `name`.
  #'
  #' @param steps `list()` \cr
  #'              A list of `character(1L)` of breakpoints to set.
  #'
  #' @details
  #' `steps` contains all active breakpoints (except those being removed) plus
  #' any that are being added. If `length(steps) == 0L`, then all breakpoints
  #' are being removed.
  #'
  #' @returns
  #' `name`
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:setFunctionBreakpoints <- \(name, env, steps)
  {
    name <- as.character(str2lang(name));
    file <- this$file_;
    on.exit(this$file_ <- character(0L));

    i <- this$which(file);
    if(i)
    {
      # clear breakpoints first
      file   <- this$files_[i];
      breaks <- this$breaks;
      lines  <- breaks[breaks$file == file$file & breaks$name == name, "line"];
      if(length(lines)) { file$rm(lines); }
      if(file$reloaded) { file$load();    }

      lines <- steps[grepl("^oopr:", steps)];
      lines <- suppressWarnings(as.integer(sub("^.*:", "", lines)));
      lines <- lines[!is.na(lines)];
      if(length(lines)) { file$add(lines, !logical(length(lines))); }
    }

    # make sure class breakpoints do not make it to normal functions
    steps <- steps[!grepl("^oopr:", steps)];

    name <- sprintf("`%s`", name);
    if(this$isNonClassFunction(name, env))
    {
      this$rsFun("setFunctionBreakpoints", name, env, steps);
    }
    return(name);
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  OoprBreakpointsRStudio <- \( ) { }
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:funs_  <- c(
    isFunctionInSync         = ".rs.isFunctionInSync"
   ,getFunctionSteps         = ".rs.rpc.get_function_steps"
   ,getEnvironmentOfFunction = ".rs.getEnvironmentOfFunction"
   ,setFunctionBreakpoints   = ".rs.setFunctionBreakpoints"
  );
  static:inSync_ <- FALSE;
  static:file_   <- character(0L);

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:isLoadedInGlobal <- \(thisFun)
  {
    rsFun <- this$funs_[thisFun];
    rsFun <- get0(rsFun, envir = globalenv(), inherits = FALSE);
    return(identical(rsFun, this[[thisFun]]));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:get:rs <- \( )
  {
    if(match("tools:rstudio", search(), 0L))
    {
      return(as.environment("tools:rstudio"));
    }
    stop("RStudio must be available");
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:rsFun <- \(fun, ...)
  {
    fun <- get(sprintf(".rs.%s", fun),, this$rs, "function", FALSE);
    fun(...);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:isNonClassFunction <- \(name, env)
  {
    if(is.character(env))
    {
      env <- if(nzchar(env)) getNamespace(env) else globalenv();
    }
    name  <- as.character(str2lang(name));
    if(exists(name, envir = env, mode = "function", inherits = FALSE))
    {
      return(!is.ooprC(get(name, envir = env, inherits = FALSE)))
    }
    return(FALSE);
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
}) ## OoprBreakpointsRStudio                                    # nocov end   #
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @name oopr_breakpoints
#' @title Breakpoints for oopr
#' @description
#' Set breakpoints for `oopr` classes.
#'
#' @details
#' Breakpoints for `oopr` classes are set within the class object and all
#' instances that are spawned from it.
#'
#' When interacting with `oopr` breakpoints, changes to source files and
#' their associated environment (either global or package) are checked for
#' changes. This allows eligible breakpoints to be applied to classes
#' that have been reloaded, and line numbers to be synced with the file.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
NULL


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname oopr_breakpoints
#' @export
#' @inheritSection OoprBreakpointsImpl RStudio
#' @examples
#' tmp <- tempfile();
#' on.exit(unlink(tmp));
#' cat(file = tmp, r"{
#' oopr("test",,
#' {
#' public:
#'   method <- \( )
#'   {
#'     "break me!";
#'   }
#' })
#' }");
#' source(tmp, keep.source = TRUE);
#' obj <- test();
#'
#' OoprBreakpoints$add(tmp, 7L);
#' print(OoprBreakpoints);
#' \dontrun{obj$method();}
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprBreakpoints", public:OoprBreakpointsImpl,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field rstudio `logical(1L)` \cr
  #'                 Tap into RStudios breakpoint mechanism. See section
  #'                 **RStudio** below. Cannot be used with `$volatile`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:get:rstudio <- \( )
  {
    return(this$rstudio_);
  }
  static:set:rstudio <- \(x)
  {
    stopifnot(is.logical(x) && length(x) == 1L && !is.na(x));
    if(!(
         identical(.Platform$GUI, "RStudio")
      && match("tools:rstudio", search(), 0L)
    ))
    {
      stop("RStudio must be available for this feature", call. = FALSE);
    }
    if(x)                                                       # nocov start #
    {
      OoprBreakpointsImpl$volatile <- FALSE;
      OoprBreakpointsRStudio[["load"]]();
    }
    else
    {
      OoprBreakpointsRStudio[["unload"]]();
    }
    this$rstudio_ <- x;                                         # nocov end   #
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprBreakpointsImpl$volatile
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:get:volatile <- \( )
  {
    return(OoprBreakpointsImpl$volatile);
  }
  static:set:volatile <- \(x)
  {
    if(this$rstudio_)
    {
      stop("$volatile cannot be set with RStudio", call. = FALSE);    # nocov #
    }
    OoprBreakpointsImpl$volatile <- x;
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
  OoprBreakpoints <- \( ) { }                                         # nocov #
  static:rstudio_ <- FALSE;

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
}) ## OoprBreakpoints
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @exportS3Method utils::str OoprBreakpointsBreak
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
str.OoprBreakpointsBreak <- \(object, indent.str, ...)
{
  if(!nrow(object))
  {
    class(object) <- "data.frame";
    NextMethod();
    return(invisible(object));
  }
  rsplit <- \(x, nm, fun = identity, ...)
  {
    if(!length(nm))
    {
      return(do.call(fun, list(x, ...)));
    }
    else
    {
      fct <- x[[nm[[1L]]]];
      fct <- factor(fct, unique(fct));
      return(lapply(split(x, fct), rsplit, nm[-1L], fun, ...));
    }
  }
  len  <- max(nchar(object$line))
  nest <- rsplit(object, c("file", "class", "name"), \(data)
  {
      cond <- vapply(data$cond, deparse1, character(1L));
      return(sprintf("%*s [%s]", len, data$line, cond));
  });
  cat(" \u252C\n");
  tree(nest, indent.str);
  cat(indent.str, "\n");
  return(invisible(object));
}
