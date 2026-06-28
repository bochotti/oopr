## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @name oopr_breakpoints
#' @title Breakpoints for oopr
#' @include init.R
#' @include container.R
#' @include source.R
#' @export
#' @description
#' Use RStudio breakpoints with `oopr` classes.
#'
#' @param on    `logical(1L)` \cr
#'              To enable/disable breakpoints. If the default, `NA`, checks
#'              whether they are currently enabled.
#'
#' @param force `logical(1L)` \cr
#'              Force assigning or removing the required functions to/from
#'              the global environment.
#'
#' @details
#' Setting breakpoints inside `oopr` classes is the same process as setting
#' them for normal functions - use the gutter or `SHIFT+F9`.
#'
#' This feature requires intercepting RStudio calls to functions that
#' normally reside within `tools:rstudio`, by assigning functions of the same
#' name inside the global environment. It is disabled by default as it is
#' potentially destructive for an R session. Environmental variable
#' `R_OOPR_BREAKPOINTS=true` can be used to enable this functionality when
#' `oopr` namespace is loaded.
#'
#' Breakpoints can be set for both methods and properties. One limitation is
#' that breakpoints cannot be set on single line functions with a specifier,
#' ```
#' get:x <- \( ) { return("x"); }
#' ````
#' as RStudio thinks the line is at the top-level.
#'
#' When a breakpoint is set class instances are searched in the global
#' environment and package namespace (if applicable). If the function inside
#' an instance does not match the source file, then all breakpoints are
#' removed for that function in that instance.
#'
#' Tested with RStudio version 2026.1.1.403, there are no guarantees for
#' earlier or later versions.
#'
#' @returns
#' `logical(1L)` indicating whether this functionality is enabled. Any
#' breakpoints set are printed to the console.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr_breakpoints <- \(on = NA, force = FALSE)
{
  stopifnot(
    is.logical(on)    && length(on)    == 1L
   ,is.logical(force) && length(force) == 1L && !is.na(force)
  );
  if(!match("tools:rstudio", search(), 0L)) stop("RStudio must be available");

  if(is.na(on))
  {
    if(OoprBreakpoints$allLoadedInGlobal())
    {
      if(length(OoprBreakpoints$printBreaks()))
      {
        return(invisible(TRUE));
      }
      else
      {
        return(TRUE);
      }
    }
    else
    {
      return(FALSE);
    }
  }
  else
  {
    return(OoprBreakpoints$loadInGlobal(on, force));
  }
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @name OoprBreakpoints
#' @keywords internal
#' @title Breakpoint Internals
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
NULL

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname OoprBreakpoints
#' @description
#' Represents a function object inside a class.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprBreakpointsFunction",,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @param name  `character(1L)` \cr
#'              The name of the function.
#'
#' @param ooprC `ooprC` \cr
#'              The `ooprC` class that the function resides.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
OoprBreakpointsFunction <- \(name, ooprC)
{
  this$name     <- name;
  this$ooprC    <- ooprC;
  this$encl     <- ooprC@encl;
  this$property <- nzchar(ooprC@meta$subs("property", names = name));
}
~OoprBreakpointsFunction <- \( )
{
  this$setBreakpoints();
}
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  name     <- character();
  ooprC    <- NULL;

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field encl `environment` \cr
  #'             The enclosure inside `ooprC`. Can change to set breakpoints
  #'             for class instances.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  encl     <- emptyenv();

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field property `logical(1L)` \cr
  #'                 Whether the function is a property. If so, then
  #'                 breakpoints need to be set on the active binding
  #'                 function.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  property <- logical();

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field fun `function` \cr
  #'            Returns the un-traced version of the function from
  #'            `$encl$this`. If set, it assigns into `$encl$this` and
  #'            `$encl$.this` if applicable.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:fun <- \( )
  {
    name <- this$name;
    if(!length(name)) return(NULL);
    if(this$property)
    {
      out <- activeBindingFunction(name, this$encl$this);
    }
    else
    {
      out <- get(name, envir = this$encl$this, inherits = FALSE);
    }
    if(isS4(out) && inherits(out, "functionWithTrace"))
    {
      out <- out@original;
    }
    return(out);
  }
  set:fun <- \(x)
  {
    if(!is.function(x)) return();
    name <- this$name;
    for(thiz in c(this$encl$this, this$encl$.this))
    {
      # wont be in .this if
      #  - non-static ooprC@encl,
      #  - private, or
      #  - non-inherited protected
      if(!exists(name, envir = thiz, inherits = FALSE)) next;
      if(bindingIsLocked(name, thiz))
      {
        unlockBinding(name, thiz);
        do.call(on.exit, list(substitute(lockBinding(name, thiz)), TRUE));
      }
      if(this$property)
      {
        makeActiveBinding(name, x, thiz);
      }
      else
      {
        assign(name, x, envir = thiz);
      }
    }
    return(x);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field srcref `srcref` \cr
  #'               The srcref for `this$fun`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:srcref <- \( ) { return(attr(this$fun, "srcref", TRUE)); }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field breaks `integer()` \cr
  #'               Line numbers of each call to `$getSteps`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:breaks <- \( ) { return(sort.default(this$lines_[this$steps_])); }

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
  isInSync <- \( )
  {
    srcref <- this$srcref;
    if(is.null(srcref)) return(FALSE);
    file <- attr(srcref, "srcfile", TRUE)$filename;
    if(is.null(file) || !file.exists(file)) return(FALSE);

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
  #' @param line `integer(1L)` \cr
  #'             The line number.
  #'
  #' @returns
  #' `logical(1L)`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  hasLine <- \(line)
  {
    srcref <- this$srcref;
    if(is.null(srcref)) return(FALSE);
    return(srcref[1L] <= line && line <= srcref[3L]);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprBreakpointsFunction$hasLine
  #'
  #' @description
  #' Get the steps of the function required to reach a line number.
  #'
  #' @details
  #' The srcrefs inside the body of `$fun` is searched to find where the line
  #' can be found. Steps is the integer position to enter the body of the
  #' function, e.g. `body(fun)[[steps]]`.
  #'
  #' @returns
  #' `character(1L)` of the steps separated by `,`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  getSteps <- \(line)
  {
    fun <- this$fun;
    if(!is.function(fun)) return("");
    steps <- this$findSteps(body(fun), line);
    steps <- paste(steps, collapse = ",");
    if(nzchar(steps))
    {
      this$lines_[steps] <- line;
    }
    return(steps);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Set breakpoints inside the class, and any class instances.
  #'
  #' @param steps `character()` \cr
  #'              Integer positions separated by `,`. If `length(steps) == 0L`,
  #'              then all breakpoints are removed.
  #'
  #' @details
  #' Uses [`base::trace`] to create a traced function, steps are ordered by
  #' their depth of nesting so `trace` doesn't overwrite its own breakpoints.
  #'
  #' @returns
  #' `this` invisibly. `steps` is also saved inside a private member for
  #' `$breaks` to return line numbers set.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  setBreakpoints <- \(steps = character())
  {
    fun <- this$fun;
    this$steps_ <- character(0L);
    if(length(steps))
    {
      this$steps_ <- steps;
      at <- lapply(strsplit(steps, ","), as.integer);
      at <- at[order(-vapply(at, length, integer(1L)))];
      suppressMessages(
        trace("fun", browser, at = at, print = FALSE, where = environment())
      );
      body(fun@.Data) <- this$addSrcrefsToBreaks(body(fun), body(fun@original));
    }
    this$fun  <- fun;
    this$setInstances(fun);
    return(invisible(this));
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  fun_   <- NULL;
  lines_ <- integer(0L);
  steps_ <- character(0L);

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  findSteps <- \(x, line)
  {
    if(!is.call(x)) return(integer(0L));
    srcs <- attr(x, "srcref", TRUE);
    for(i in seq_along(x))
    {
      src <- srcs[[i]];
      if(!is.null(src) && !(src[1L] <= line && line <= src[3L])) next;
      nest <- this$findSteps(x[[i]], line);
      if(length(nest))                          return(c(i, nest));
      if(!is.null(src) && !isname(x[[i]], "{")) return(i);
    }
    return(integer(0L));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' The srcref at the requested lines are added to the newly added breakpoints
  #' to ensure that the line number is available when being hit.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  addSrcrefsToBreaks <- \(expr, oexpr)
  {
    if(!(is.call(expr) && is.call(oexpr) && length(expr) == length(oexpr)))
    {
      return(expr);
    }
    for(i in seq_along(expr))
    {
      if(   iscall(expr[[i]], "{") && length(expr[[i]]) > 1L
         && iscall(expr[[i]][[2L]], ".doTrace")
      )
      {
        src <- attr(expr, "srcref")[[i]] %||% attr(oexpr, "srcref")[[i]];
        attr(expr[[i]], "srcref") <- rep(list(src), length(expr[[i]]));
      }
      else
      {
        expr[[i]] <- this$addSrcrefsToBreaks(expr[[i]], oexpr[[i]]);
      }
    }
    return(expr);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' Instances are found (refer `./src/breakpoint.cpp`) and they also have
  #' their functions amended. If the instance becomes out of sync of the
  #' source file, then its untraced version is set instead.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  setInstances <- \(fun)
  {
    name      <- this$name;
    instances <- .Call(Cpp_find_instances, this$ooprC, sys.frame(-2L), name);
    if(!length(instances)) return();
    encl     <- this$encl;
    property <- this$property;
    on.exit(this$encl <- encl);
    for(inst in instances)
    {
      thiz  <- inst$this;
      if(!exists(name, envir = thiz, inherits = FALSE)) next;
      active <- bindingIsActive(name, thiz);
      if((property && !active) || !property && active)  next;
      # setting $encl changes references of $fun property
      this$encl <- inst;
      old       <- this$fun;
      if(isS4(fun) && inherits(fun, "functionWithTrace"))
      {
        environment(fun@original) <- environment(old);
      }
      environment(fun) <- environment(old);
      # if in sync, then the definition matches `fun`
      # otherwise, force the non-traced version (of the instance)
      this$fun  <- if(this$isInSync()) fun else old;
    }
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
})


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname OoprBreakpoints
#' @description
#' Represents a class inside a file.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprBreakpointsClass",,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @param ooprC `ooprC` \cr
#'              The `ooprC` object.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
OoprBreakpointsClass <- \(ooprC)
{
  if(!is.ooprC(ooprC))       return();
  name  <- ooprC@name;
  # make sure to get the actual class, not a copy of
  ooprC <- get0(name, parent.env(ooprC@encl), inherits = FALSE);
  if(!is.ooprC(ooprC, name)) return();
  this$ooprC <- ooprC;
  this$loadFunctions();
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  ooprC <- NULL;

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field name `character(1L)` \cr
  #'        The name of the class.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:name <- \( )
  {
    if(is.null(this$ooprC)) return(character(0L)) else return(this$ooprC@name);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field functions `OoprBreakpointsFunction` \cr
  #'        An array of each function inside the class.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  functions <- OoprBreakpointsFunction[[]];

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Check if the class contains a function and/or a line.
  #'
  #' @param name `character(1L)` \cr
  #'             The name of the function.
  #'
  #' @param line `integer(1L)` \cr
  #'             Optionally, a line that the function should contain.
  #'
  #' @returns
  #' `logical(1L)`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  has <- \(name, line = integer(0L))
  {
    if(!this$functions$exists(name)) return(FALSE);
    return(!length(line) || this$functions[name]$hasLine(line));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprBreakpointsClass$has
  #'
  #' @description
  #' Check if a function is in sync.
  #'
  #' @details
  #' If the function is not in the class `TRUE` is returned.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  isInSync <- \(name)
  {
    return(!this$has(name) || this$functions[name]$isInSync());
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprBreakpointsFunction$getSteps
  #'
  #' @description
  #' Get the steps required to get to a line in a function.
  #'
  #' @param name `character(1L)` \cr
  #'             Then name of the function.
  #'
  #' @details
  #' The trick here is to append the steps with the name of the class. This
  #' way when receiving them for setting a breakpoints, the class holding the
  #' function is known.
  #'
  #' @returns
  #' `list(name = character(1L), line = integer(1L), at = character(1L))`.
  #' `at` is in the form `"class:steps"`, where `steps` are the integer
  #' position of the function body, separated by `,`. If no steps can be
  #' found, then `list()` is returned.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  getSteps <- \(name, line)
  {
    if(!this$has(name, line)) return(list());
    steps <- this$functions[name]$getSteps(line);
    if(!nzchar(steps))        return(list());
    if(make.names(name) != name)
    {
      name <- sprintf("`%s`", name);
    }
    steps <- sprintf("%s:%s", this$name, steps);
    out   <- list(name = name, line = line, at = steps);
    out   <- lapply(out, `class<-`, "rs.scalar");
    return(out);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprBreakpointsFunction$setBreakpoints
  #'
  #' @param name  `character(1L)` \cr
  #'              Then name of the function.
  #'
  #' @returns
  #' `this` invisibly.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  setBreakpoints <- \(name, steps = character(0L))
  {
    this$functions[name]$setBreakpoints(steps);
    return(invisible(this));
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  loadFunctions <- \( )
  {
    ooprC <- this$ooprC;
    meta  <- ooprC@meta;
    for(i in which(meta$subs(inherit = "")))
    {
      if(!(nzchar(meta$property$get(i)) || meta$method$get(i))) next;
      name <- meta$names$get(i);
      this$functions$emplace(name, name, ooprC);
    }
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
})


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname OoprBreakpoints
#' @description
#' Represents a source file that contains `oopr` classes.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprBreakpointsFile", OoprSource,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @param file `character(1L)` \cr
#'             The name of the source file.
#'
#' @param env  `environment` \cr
#'             The environment that holds the class definitions. Either a
#'             package namespace or the global environment.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
OoprBreakpointsFile <- \(file, env)
{
  OoprSource$file <- file;
  this$loadClassesFromEnvironment(env);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:file <- \( )
  {
    return(OoprSource$file);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field timestamp `POSIXct(1L)` \cr
  #'                  The last time the source file was read.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  timestamp <- .POSIXct(0);

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field classes `OoprBreakpointsClass` \cr
  #'                The classes defined inside the source file.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  classes <- OoprBreakpointsClass[[]];

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Check if the source file has been modified, if so reload the classes.
  #'
  #' @param env  `environment` \cr
  #'             The environment that holds the class definitions. Either a
  #'             package namespace or the global environment.
  #'
  #' @returns
  #' `this` invisibly.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  syncClassesWithFile <- \(env)
  {
    if(file.mtime(this$file) > this$timestamp)
    {
      this$classes$resize();
      this$loadClassesFromEnvironment(env)
    }
    return(invisible(this));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprBreakpointsClass$has
  #'
  #' @description
  #' Check if any class inside the file contains a function and/or a line.
  #'
  #' @returns
  #' Named `logical()` the same length as `$classes`, indicating which class
  #' holds the function and/or line. If only `name` is provided, can have
  #' more than one `TRUE`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  has <- \(name, line = integer(0L))
  {
    return(unlist(this$classes$apply(\(key, val) { val$has(name, line); })));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprBreakpointsClass$isInSync
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  isInSync <- \(name)
  {
    synced <- this$classes$apply(\(key, val) { val$isInSync(name); });
    return(all(unlist(synced)));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprBreakpointsClass$getSteps
  #'
  #' @description
  #' Get the steps of multiple lines across the file.
  #'
  #' @param lines `list(integer(1L))` \cr
  #'              A list of line numbers.
  #'
  #' @returns
  #' A list the same length of `lines`, containing further lists in the form:
  #' `list(name = character(1L), line = integer(1L), at = character(1L))`.
  #' Lines that cannot be found are returned as `list()`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  getSteps <- \(name, lines)
  {
    out <- rep_len(list(), length(lines));
    for(i in seq_along(lines))
    {
      line <- lines[[i]];
      has  <- this$has(name, line);
      if(!any(has)) next;
      class <- names(has)[has];
      out[[i]] <- this$classes[class]$getSteps(name, line);
    }
    return(out);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprBreakpointsClass$setBreakpoints
  #'
  #' @description
  #' Set breakpoints for a function across classes.
  #'
  #' @param classes `character()` \cr
  #'                The classes to set breakpoints for. If
  #'                `length(classes) == 0L`, then all classes.
  #'
  #' @returns
  #' `this` invisibly.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  setBreakpoints <- \(name, classes = character(0L), steps = character(0L))
  {
    if(!length(classes))
    {
      classes <- this$has(name);
      classes <- names(classes)[classes];
    }
    for(class in classes)
    {
      this$classes[class]$setBreakpoints(name, steps);
    }
    return(invisible(this));
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  loadClassesFromEnvironment <- \(env)
  {
    OoprSource$parse();
    if(length(OoprSource$defs))
    {
      for(class in names(OoprSource$defs))
      {
        ooprC <- get0(class, envir = env, inherits = FALSE);
        if(!is.ooprC(ooprC, class)) next;
        this$classes$emplace(class, ooprC);
      }
    }
    this$timestamp <- file.mtime(this$file);
    return(invisible(this));
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
})


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname OoprBreakpoints
#' @description
#' The controller of the breakpoints. Process of setting a breakpoint is:
#'   1. Check the function is in sync.
#'   2. Get the steps of the functions body to arrive at a line.
#'   3. Get the environment that the function is assigned to.
#'   4. Set the breakpoints into the function.
#' Only steps 3 & 4 are conducted when unsetting a breakpoint.
#'
#' RStudio maintains its own database for breakpoints and is  recorded by
#' file and function name. However, multiple classes in a file could share a
#' function name.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprBreakpoints",,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field files `OoprBreakpointsFile` \cr
  #'              The source files that have/had breakpoints.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:files <- OoprBreakpointsFile[[]];

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:allLoadedInGlobal <- \( )
  {
    for(nm in names(this$funs_)) if(!this$isLoadedInGlobal(nm)) return(FALSE);
    return(TRUE);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:loadInGlobal <- \(on, force)
  {
    if(!force)
    {
      ok <- vapply(names(this$funs_), logical(1L), FUN = \(nm) (
           !exists(this$funs_[nm], envir = globalenv(), inherits = FALSE)
        || this$isLoadedInGlobal(nm)
      ));
      if(!all(ok))
      {
        stop(sprintf(
          "Symbols %s are already assigned to the global environment"
         ,deparse1(unname(this$funs_[!ok]))
        ));
      }
    }

    if(on) for(thisFun in names(this$funs_))
    {
      rsFun <- this$funs_[thisFun];
      assign(rsFun, this[[thisFun]], envir = globalenv());
    }
    else
    {
      rm(list = this$funs_, envir = globalenv());
    }
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
  #' `.rs.getUntracedFunction`
  #' `.rs.getEnvironmentOfFunction`
  #'
  #' @returns
  #' `logical(1L)`
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:isFunctionInSync <- \(name, file, pkg)
  {
    # if a function exists in the namespace, check it first
    if(this$isNonClassFunction(name, pkg))
    {
      if(!this$rsFun("isFunctionInSync", name, file, pkg)) return(FALSE);
    }

    # now check for ooprs in the file
    env  <- if(nzchar(pkg)) getNamespace(pkg) else globalenv();
    name <- as.character(str2lang(name));

    if(!this$files$exists(file))
    {
      this$files$emplace(file, file, env);
    }
    else
    {
      this$files[file]$syncClassesWithFile(env);
    }

    if(this$files[file]$classes$empty)
    {
      this$inSync_ <-  TRUE;
    }
    else
    {
      this$inSync_ <- this$files[file]$isInSync(name);
    }
    return(this$inSync_);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprBreakpoints$isFunctionInSync
  #'
  #' @description
  #' Gets the steps required for a line.
  #'
  #' @param lines `list()` \cr
  #'              A list of `integer(1L)` of the line numbers.
  #'
  #' @details
  #' `lines` include only the lines to be added.
  #'
  #' @seealso
  #' `.rs.getFunctionSteps`
  #'
  #' @returns
  #' `list(name =, line =, at = )` - `at` should be separated by `,`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:getFunctionSteps <- \(name, file, pkg, lines)
  {
    out <- list();
    if(!this$inSync_) return(out);
    on.exit(this$inSync_ <- FALSE);

    if(this$isNonClassFunction(name, pkg))
    {
      out <- this$rsFun("rpc.get_function_steps", name, file, pkg, lines);
    }

    name  <- as.character(str2lang(name));
    if(this$files$exists(file) && any(this$files[file]$has(name)))
    {
      out <- c(out, this$files[file]$getSteps(name, lines));
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
      name <- as.character(str2lang(name));
      if(this$files$exists(file) && any(this$files[file]$has(name)))
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
  #' The function is fully untraced first, meaning that `steps` always
  #' contains all active breakpoints (except those being removed) plus any
  #' that are being added. If `length(steps) == 0L`, then all breakpoints are
  #' being removed.
  #'
  #' @returns
  #' `name`
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:setFunctionBreakpoints <- \(name, env, steps)
  {
    name <- as.character(str2lang(name));
    file <- this$file_;
    on.exit(this$file_ <- character(0L));

    if(length(file))
    {
      # clear breakpoints first
      this$files[file]$setBreakpoints(name);

      # collect the class breakpoints (they have `:`)
      steps <- this$splitOoprSteps(file, steps);
      cteps <- steps$class;
      steps <- steps$fun;
      for(class in names(cteps))
      {
        this$files[file]$setBreakpoints(name, class, cteps[[class]]);
      }
    }

    # make sure class breakpoints do not make it to functions
    steps <- steps[!grepl(":", steps)];

    name <- sprintf("`%s`", name);
    if(this$isNonClassFunction(name, env))
    {
      this$rsFun("setFunctionBreakpoints", name, env, steps);
    }
    return(name);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Print currently set breakpoints.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:printBreaks <- \( )
  {
    rm  <- \(x) { x[vapply(x, length, integer(1L)) > 0L]; }
    out <- rm(this$files$apply(\(key, file)
    {
      rm(file$classes$apply(\(key, class)
      {
        out <- rm(class$functions$apply(\(key, fun)
        {
          if(length(fun$breaks))
          {
            lines  <- attr(fun$srcref, "srcfile")$lines;
            width  <- trunc(log10(length(lines))) + 1L;
            breaks <- format.default(fun$breaks, width = width);
            breaks <- sprintf("#%s [%s]", breaks, names(breaks));
            return(breaks);
          }
        }));
        names(out) <- sprintf("$%s", names(out));
        return(out);
      }))
    }))
    for(file in names(out))
    {
      cat(file, "\n");
      tree(out[[file]]);
    }
    return(invisible(out));
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
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
  static:rsFun <- \(fun, ...)
  {
    if(!match("tools:rstudio", search(), 0L)) stop("RStudio must be available");
    fun <- get(sprintf(".rs.%s", fun), "tools:rstudio",, "function", FALSE);
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

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:splitOoprSteps <- \(file, steps)
  {
    class   <- list();
    for(cls in this$files[file]$classes$keys)
    {
      if(!length(steps)) break;
      pfx <- sprintf("%s:", cls);
      has <- startsWith(unlist(steps), pfx);
      if(!any(has)) next;
      class[[cls]] <- substr(steps[has], nchar(pfx) + 1L, nchar(steps[has]));
      steps <- steps[!has];
    }
    return(list(fun = steps, class = class));
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
})
