## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @name oopr_completion
#' @title Completion for oopr
#' @include init.R
#' @include source.R
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
    return(comp$names);
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
    comp$isStaticContainerMember(name);
    return(.subset2(comp$obj, name));
  }
  NextMethod();
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' This could be improved...
#' At the moment, the repeated `$` calls are prompting the file to be
#' re-parsed when it does not need to be.
#' What would work better is to have `oopr_this$` return itself and
#' keep going until it reaches the end of the call - then parse and do work.
#' Can also support standard .DollarNames to make it more portable.
#'
#' Something like this:
#' call <- NULL;
#' test <- structure(list(), class = "test");
#' `$.test` <- \(x, name)
#' {
#'   if(is.null(call))
#'   {
#'     if(iscall(sys.call(1L), ".DollarNames"))
#'     {
#'       call <<- sys.call(1L)[[2L]];
#'     }
#'     else if(iscall(sys.call(1L), ".rs.rpc.get_completions"))
#'     {
#'       call <<- str2lang(get("string", sys.frame(1L))[[1L]])
#'     }
#'   }
#'
#'   if(identical(substitute(name), as.character(call[[3L]])))
#'   {
#'     # do stuff ...
#'     browser();
#'     call <<- NULL;
#'   }
#'
#'   return(x);
#' }
#'
#' .DollarNames(test$b$c$d$)
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprCompletion", private:OoprSourceContext,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:obj   <- \( ) { if(!is.null(this$obj_)) return(this$obj_@encl$this); }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:names <- \( )
  {
    if(is.null(this$obj_)) return(character(0L));
    oopr   <- this$obj;
    class  <- this$obj_@name;
    access <- character(1L);
    if(this$isCMem_)
    {
      access <- "public";
    }
    else if(this$isInhr_)
    {
      access <- c("public", "protected");
    }
    else
    {
      access <- c("public", "protected", "private");
    }
    names <- this$obj_@meta$subs("names", access = access);
    names <- grep(sprintf("^~?%s$", class), names, value = TRUE, invert = TRUE);
    return(.DollarNames.oopr(oopr, names = names));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  isRStudioCompletion <- \( )
  {
    if(!OoprSourceContext$rStudioIsAvailable()) return(FALSE);
    for(i in rev(seq_len(sys.nframe())))
    {
      if(iscall(sys.call(i), ".rs.getCompletionType"))      return(FALSE);
      if(iscall(sys.call(i), ".rs.isDataTableExtractCall")) return(FALSE);
      if(iscall(sys.call(i), ".rs.getCompletionsDollar"))
      {
        return(this$cursorInClass(i));
      }
    }
    return(FALSE);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  isClassMember <- \(memb, name = NULL, call = sys.call(-1L))
  {
    if(!is.ooprC(memb))                      return(FALSE);
    if(!grepl("$", this$str_, fixed = TRUE)) return(FALSE);
    if(is.null(name))
    {
      call <- str2lang(this$str_);
    }
    if(this$isNestedMember(call, name))
    {
      this$isCMem_ <- TRUE;
      return(TRUE);
    }
    return(FALSE);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  isInheritedClass <- \(memb, name = NULL, call = sys.call(-1L))
  {
    if(!is.ooprC(memb)) return(FALSE);

    str <- this$str_;
    if(is.null(name))
    {
      call <- str2lang(str);
    }
    class <- sub("\\$.*$", "", str);
    obj   <- this$obj_;
    encl  <- obj@encl;

    if(match(class, obj@inhr, 0L) && is.ooprC(encl[[class]], class))
    {
      this$isInhr_ <- TRUE;
      this$obj_    <- encl[[class]];
      return(TRUE);
    }
    return(FALSE);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  isContainerMember <- \(memb, name = NULL, call = sys.call(-1L))
  {
    if(!grepl("[", this$str_, fixed = TRUE)) return(FALSE);
    if(this$isNestedMember(call))
    {
      this$isCMem_ <- TRUE;
      return(TRUE);
    }
    return(FALSE);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  isStaticContainerMember <- \(name)
  {
    if(any(this$obj_@meta$subs(names = name, static = TRUE, class = TRUE)))
    {
      encl  <- this$obj_@encl;
      thiz  <- encl$this;
      class <- class(thiz[[name]])[1L];
      ooprC <- get0(class, parent.env(encl));
      if(!is.ooprC(ooprC, class)) return(FALSE);
      thiz[[name]] <- ooprC;
      return(TRUE);
    }
    return(FALSE);
  }


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  obj_    <- NULL;
  str_    <- character(1L);
  isCMem_ <- FALSE;
  isInhr_ <- FALSE;

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  cursorInClass <- \(pos)
  {
    if(!iscall(sys.call(pos - 1L), ".rs.rpc.get_completions")) return(FALSE);
    env <- sys.frame(pos - 1L);
    OoprSourceContext$id <- env$documentId;
    try(OoprSourceContext$parseFile(try = TRUE)     , outFile = stdout());
    try(OoprSourceContext$evalExprs(env$envir, TRUE), outFile = stdout());
    obj <- OoprSourceContext$getByPos(stop = FALSE);
    obj <- this$replaceGlobalWithLoadedPackage(obj);
    this$obj_ <- obj;
    this$str_ <- env$string[[1L]];
    return(!is.null(this$obj_));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  replaceGlobalWithLoadedPackage <- \(obj)
  {
    if(is.null(obj)) return(obj);
    encl <- obj@encl;
    if(environmentName(topenv(encl)) != "R_GlobalEnv") return(obj);
    for(par in search()[-1L])
    {
      name <- sub("^package:", "", par);
      if(par == name || !requireNamespace(name, quietly = TRUE)) next;
      ns <- getNamespace(name);
      if(!exists(".__DEVTOOLS__", envir = ns, inherits = FALSE)) next;
      if(exists(obj@name, envir = ns, inherits = FALSE))
      {
        parent.env(encl) <- ns;
        return(obj);
      }
    }
    return(obj);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  isNestedMember <- \(call, name = NULL, encl = "this")
  {
    calls <- this$flattenCall(call);
    if(!isname(calls[[1]], c("this", this$obj_@inhr))) return(FALSE);
    calls[[1L]] <- NULL;
    if(!is.null(name))
    {
      calls[[length(calls)]] <- NULL;
    }
    obj <- this$obj_;
    len <- length(calls);
    for(i in seq_len(len))
    {
      call <- calls[[i]];
      if(isname(call$oper, c("$", "$.ooprC")))
      {
        if(!is.name(call$rhs)) return(FALSE);
        rhs <- as.character(call$rhs);
        if(!any(obj@meta$subs(names = rhs, class = TRUE))) return(FALSE);
        # if next call is `[`, then it is accessing a container
        if(i < len && isname(calls[[i + 1]]$oper, c("[", "[.ooprC")))
        {
          cont <- classmem_get_containers(obj@meta, obj@encl$this);
          if(!cont[rhs]) return(FALSE);
          obj  <- classmem_get_ooprC(rhs, obj@meta, obj@encl$this, cont, TRUE);
        }
        else if(any(obj@meta$subs(static = TRUE)))
        {
          obj <- get0(class(obj@encl$this[[rhs]])[1L], parent.env(obj@encl));
        }
        else
        {
          obj <- obj@encl$this[[rhs]];
        }
      }
      else if(i == len && isname(call$oper, c("[", "[.ooprC")))
      {
        # the last `[` call needs to display public members only
        obj <- this$makeContainerInterface(obj);
      }
    }
    this$obj_ <- obj;
    return(TRUE);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  flattenCall <- \(x)
  {
    if(!is.call(x)) return(list(x))
    c(this$flattenCall(x[[2L]]), list(list(oper = x[[1L]], rhs = x[[3L]])));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  makeContainerInterface <- \(obj)
  {
    meta  <- obj@meta;
    thiz  <- obj@encl$this;
    names <- meta$subs("names", access = "public");
    class <- class(obj@encl$.this)

    encl      <- new.env(parent = parent.env(obj@encl));
    encl$this <- interface(thiz, names, class, quote(this));

    cont <- classmem_get_containers(meta, thiz);
    cont <- cont & obj@meta$subs(static = TRUE);
    for(name in names(cont)[cont])
    {
      if(!match(name, names, 0L)) next;
      encl$this[[name]] <- classmem_get_ooprC(name, meta, thiz, cont, TRUE);
    }

    attr(obj, "encl") <- encl;
    return(obj);
  }

})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#'
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
OoprCompletionHelp <- NULL;
oopr("OoprCompletionHelp",,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
OoprCompletionHelp <- \(topic, source, class, package)
{
  ns <- if(package == "R_GlobalEnv") globalenv() else getNamespace(package);
  if(is.null(topic) && grepl("$", source, fixed = TRUE))
  {
    topic  <- sub("^.*\\$", "", source);
    source <- sub("\\$(?!.*\\$).*?$", "", source, perl = TRUE);
  }

  oopr <- tryCatch(eval(str2lang(source), ns), error = identity);
  if(!is.oopr(oopr, class))
  {
    if(startsWith(source, "this"))
    {
      oopr <- get0(class, ns);
    }
    if(!is.ooprC(oopr, class)) return(NULL);
    oopr <- oopr@encl$this;
  }

  this$tpc_  <- topic;
  this$src_  <- source;
  this$cls_  <- class;
  this$pkg_  <- package;
  this$oopr_ <- oopr;
  this$fail_ <- FALSE;
}
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:makeHelpHandler <- \(x)
  {
    if(!is.environment(x)) return(NULL);
    x       <- parent.env(x);
    class   <- base::class(.subset2(x, ".this"))[1L];
    package <- environmentName(topenv(x));
    expr    <- substitute({
      fun <- get("OoprCompletionHelp", envir = getNamespace("oopr"))$getHelp;
      formals(fun)[c("class", "package")] <- list(class, package);
      return(fun);
    })
    return(deparse1(expr, collapse = "\n"));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:getHelp <- \(type, topic, source, class, package)
  {
    comp <- OoprCompletionHelp(topic, source, class, package);
    if(comp$fail) return(NULL);
    comp$makeHelp(type);
    return(comp$out);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:fail <- \( ) { return(this$fail_); }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:out <- \( )
  {
    out <- this$out_;
    out <- out[vapply(out, length, integer(1L)) > 0L];
    if(length(out)) return(out) else return(NULL);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  makeHelp <- \(type)
  {
    switch(type,
      completion = this$makeCompletion()
     ,parameter  = this$makeParameter()
     ,url        = this$makeUrl()
    );
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  fail_ <- TRUE;
  tpc_  <- character(0L);
  src_  <- character(0L);
  cls_  <- character(0L);
  pkg_  <- character(0L);
  oopr_ <- emptyenv();

  #https://github.com/rstudio/rstudio/blob/main/src/gwt/src/org/rstudio/studio/
  #client/workbench/views/help/model/HelpInfo.java#L28
  out_     <- list(
    package_name     = character(0L)
   ,title            = character(0L)
   ,signature        = character(0L)
   ,description      = character(0L)
   ,args             = character(0L)
   ,arg_descriptions = character(0L)
   ,type             = integer(0L)
  );

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  makeCompletion <- \(topic = this$tpc_, class = this$cls_, oopr = this$oopr_)
  {
    description <- sprintf("A description of `%s$%s`", class, topic);
    obj         <- tryCatch(oopr[[topic]], error = \(e) NULL);
    signature   <- topic;
    if(is.ooprC(obj) || is.oopr(obj))
    {
      signature <- format(obj);
    }
    else if(is.function(obj))
    {
      signature <- deparse(args(obj), width.cutoff = 500L, nlines = 1);
      signature <- sub("function ", topic, signature);
    }
    else
    {
      signature <- typeof(obj);
      if(is.vector(obj))
      {
        signature <- sprintf("%s(%iL)", signature, length(obj));
      }
    }
    this$out_$description <- description;
    this$out_$signature   <- signature;
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  makeParameter  <- \(topic = this$tpc_, class = this$cls_, oopr = this$oopr_)
  {
    args  <- names(formals(.subset2(oopr, topic)));
    arg_descriptions <- sprintf(
      "a description of `%s$%s(%s)`", class, topic, args
    );
    this$out_$args <- args;
    this$out_$arg_descriptions <- arg_descriptions;
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  makeUrl <- \( )
  {

  }

})
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
help_formals_handler.oopr <- \(topic, source)
{
  if(is.oopr(source, "oopr_this")) stop() #TODO - redo comp
  formals <- sprintf("%s = ", names(formals(.subset2(source, topic))));
  help    <- OoprCompletionHelp@encl$.this$makeHelpHandler(source);
  return(list(formals = formals, helpHandler = help))
}

