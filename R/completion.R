## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @name oopr_completion
#' @title Completion for oopr
#' @include init.R
#' @include source.R
#' @export
#' @description
#' Code-completion / intellisense for `oopr` classes.
#'
#' @details
#' While typing inside an `oopr` definition, use dollarnames on `this` to
#' know what members are available. Inherited classes can also be accessed,
#' if they exist on the search path.
#'
#' Currently only implemented for RStudio.
#'
#' @examples
#' \dontrun{
#' oopr("memb",,
#' {
#' public:
#'   a <- 1L;
#'   b <- list(c = 1L, b = "b");
#' })
#'
#' oopr("test", memb,
#' {
#' public:
#'   memb   <- memb;
#'   method <- \( )
#'   {
#'     #    v press TAB here
#'     this$memb$b
#'     #         ^ or here
#'
#'     memb$b
#'     #    ^ and even here
#'   }
#' })}
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
this <- new.env(parent = baseenv());
class(this) <- c("oopr_this", "oopr");

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @exportS3Method utils::.DollarNames oopr_this
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
.DollarNames.oopr_this <- \(x, pattern)
{
  comp <- OoprCompletion();
  if(comp$isCompletion()) return(comp$names());
  NextMethod();
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @exportS3Method "$" oopr_this
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
`$.oopr_this` <- \(x, name)
{
  comp <- OoprCompletion();
  if(comp$isCompletion()) return(this);
  NextMethod();
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @exportS3Method "[" oopr_this
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
`[.oopr_this` <- \(x, name)
{
  comp <- OoprCompletion();
  if(comp$isCompletion()) return(this);
  NextMethod();
}


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @name OoprCompletion
#' @title Completion for oopr internals
#' @keywords internal
#' @aliases NULL
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
NULL
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname OoprCompletion
#' @description
#' A virtual class that can be inherited to use as a completion identifier.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprCompletionSource", public:OoprSourceTry,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field env `environment` \cr
  #'            The environment provided to `$load`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:env  <- \( ) { return(this$env_); }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Check if the call is currently a completion.
  #'
  #' @details
  #' This method should check the call stack to ensure the appropriate
  #' completion call is being made above. It should also use the `$load`
  #' method.
  #'
  #' @returns
  #' `logical(1L)`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  virtual:isAvailable <- \( ) { return(FALSE); }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Load the completion context into the class.
  #'
  #' @param env  `environment` \cr
  #'             The environment which acts as the parent of oopr classes.
  #'
  #' @param file `character(1L)` \cr
  #'             File path to the source file containing the oopr class. If
  #'             it does not exist, `text` will be sufficient.
  #'
  #' @param text `character()` \cr
  #'             The text containing lines of the source file. If `NULL` and
  #'             `file` exists, then its lines are read.
  #'
  #' @param row  `integer(1L)` \cr
  #'             Text cursor position of the line number, indexed by 1.
  #'
  #' @param col  `integer(1L)` \cr
  #'             Text cursor position of column on the line, indexed by 1.
  #'
  #' @details
  #' Cannot be over-ridden. This is used inside `OoprCompletion` class.
  #'
  #' @returns
  #' `this` invisibly.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  final:load  <- \(env = globalenv(), file, text = NULL, row, col)
  {
    stopifnot(
      is.environment(env)
     ,is.character(file)   && length(file) == 1L
     ,is.null(text)        || is.character(text)
    );
    this$env_   <- env;
    if(file.exists(file))
    {
      this$file <- file;
      text      <- text %||% readLines(file, warn = FALSE);
      if(grepl("(?i)\\.Rmd$", file))
      {
        text <- this$blocksFromRmd(text);
      }
    }
    this$text   <- text;
    this$row    <- row;
    this$col    <- col;
    return(invisible(this));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Source the file being completed.
  #'
  #' @details
  #' Cannot be over-ridden. This is used inside `OoprCompletion` class.
  #'
  #' @returns
  #' `this` invisibly.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  final:source  <- \( )
  {
    this$parse();
    this$eval(top = this$env_);
    return(invisible(this));
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  env_  <- emptyenv();
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  blocksFromRmd <- \(text)
  {
    edges <- which(startsWith(text, "```"));
    len   <- length(edges);
    if(len %% 2L != 0L)
    {
      stop("`file` is a .Rmd file with uneven ``` blocks");
    }
    edges <- split.default(edges, unlist(lapply(seq_len(len / 2), rep, 2L)));
    lines <- lapply(edges, \(edge)
    {
      i <- seq.default(edge[1L], edge[2L]);
      i[-c(1L, length(i))];
    });
    lines <- unlist(lines, use.names = FALSE);
    keep <- logical(length(text));
    keep[lines] <- TRUE;
    text[!keep] <- character(1L);
    return(text);
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
}) ## OoprCompletionSource
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname OoprCompletion
#' @description
#' Use completion in RStudio.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprCompletionRStudio", public:OoprCompletionSource,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Pull information from the `.rs.rpc_get_completions` call.
  #'
  #' @details
  #' Specifically pulls the rstudio `id` for the completion which contains
  #' the file path, row and column.
  #'
  #' @returns
  #' `logical(1L)`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  isAvailable <- \( )
  {
    if(!this$rStudioIsAvailable())                          return(FALSE);
    if(!iscall(sys.call(1L), ".rs.rpc.get_completions"))    return(FALSE);
    for(i in rev(seq_len(sys.nframe())))
    {
      if(iscall(sys.call(i), ".rs.getCompletionType"))      return(FALSE);
      if(iscall(sys.call(i), ".rs.isDataTableExtractCall")) return(FALSE);
      if(iscall(sys.call(i), c(
        ".rs.getCompletionsDollar", ".rs.getCompletionsCustomHelpHandler"
      )))
      {
        return(this$getItems(i));
      }
    }
    return(FALSE);
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  rStudioIsAvailable <- \( )
  {
    return(
         requireNamespace("rstudioapi", quietly = TRUE)
      && identical(.Platform$GUI, "RStudio")
    );
  }
  get:rstudioapi     <- \( ) { return(getNamespace("rstudioapi")); }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  getItems <- \(pos)
  {
    if(!iscall(sys.call(pos - 1L), ".rs.rpc.get_completions")) return(FALSE);
    env     <- sys.frame(pos - 1L);
    id      <- env$documentId;
    if(!nzchar(id))                                            return(FALSE);
    context <- this$rstudioapi$getSourceEditorContext(id);
    if(is.null(context))                                       return(FALSE);
    this$load(
      env    = env$envir
     ,file   = context$path
     ,text   = context$contents
     ,row    = context$selection[[1]]$range$start[[1]]
     ,col    = context$selection[[1]]$range$start[[2]]
    );
    return(TRUE);
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
}) ## OoprCompletionRStudio
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname OoprCompletion
#' @description
#' Use completion.
#'
#' @details
#' Completion call on an object is in the form `this$a$b$`. The trick here
#' is to check whether the text cursor is inside a class in the source file.
#' If so, collect that information and skip over everything before the last
#' dollar.
#'
#' When the last dollar is reached, `.DollarNames` is called on `oopr_this`
#' object, which then fires the `names` method below. The evaluation context
#' `this$a$b` is found, and `.DollarNames` provided.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprCompletion",,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field isCompleting `logical(1L)` \cr
  #'                     Whether completion is currently in place. This will
  #'                     skip over each `$` call in the evaluation context.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:get:isCompleting   <- \( ) { return(this$isCompleting_); }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field isGettingNames `logical(1L)` \cr
  #'                       Whether names of an `ooprC` are being saught.
  #'                       This is used from `.DollarNames.ooprC` to provide
  #'                       more than just public static members.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:get:isGettingNames <- \( ) { return(this$isGettingNames_); }
  static:set:isGettingNames <- \(x)
  {
    stopifnot(is.logical(x) && length(x) == 1L && !is.na(x));
    this$isGettingNames_ <- x;
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field source `OoprCompletionSource` \cr
  #'               A instanced class advising whether completion is being
  #'               sought.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:get:source <- \( )
  {
    return(this$source_);
  }
  static:set:source <- \(x)
  {
    stopifnot(is.oopr(x, "OoprCompletionSource"));
    this$source_ <- x;
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Is completion being sought?
  #'
  #' @details
  #' Checks the `source` member as to whether the `$` or `.DollarNames` calls
  #' are within a completion context. If so, then the file is parsed,
  #' `oopr` class constructor collected, and `$isCompleting` set to `TRUE`.
  #'
  #' @returns
  #' `logical(1L)`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  isCompletion <- \( )
  {
    if(this$isCompleting)           return(TRUE);
    if(!this$source_$isAvailable()) return(FALSE);
    try(this$source_$source(), outFile = stdout());
    if(!is.ooprC(this$source_$obj)) return(FALSE);
    this$isCompleting_ <- TRUE;
    return(TRUE);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Get the dollar names of the completion context.
  #'
  #' @returns
  #' `character()` of names.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  names <- \( )
  {
    if(!this$isCompleting_) return(character(0L));
    this$isGettingNames_ <- TRUE;
    on.exit({
      this$isGettingNames_ <- FALSE;
      this$isCompleting_   <- FALSE;
    });
    obj <- this$evaluateCall();
    if(is.null(obj)) return(character(0L));
    if(is.ooprC(obj))
    {
      names <- this$ooprCNames(obj);
    }
    else
    {
      names <- .DollarNames.oopr(obj);
    }
    return(names);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Get the object of the completion context.
  #'
  #' @details
  #' Used within `.rs.rpc.get_custom_parameter_help`.
  #'
  #' @returns
  #' `varies`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  obj <- \( )
  {
    if(!this$isCompleting_) return(NULL);
    on.exit(this$isCompleting_ <- FALSE);
    obj <- this$evaluateCall(rmLast = TRUE);
    if(is.ooprC(obj))
    {
      obj <- obj@encl$this;
    }
    return(obj)
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:source_         <- OoprCompletionSource;
  static:isCompleting_   <- FALSE;
  static:isGettingNames_ <- FALSE;
  isCMem_                <- FALSE;
  isInhr_                <- FALSE;

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  evaluateCall <- \(rmLast = FALSE)
  {
    obj   <- this$source_$obj;
    calls <- this$flattenCall(this$source_$call);
    if(rmLast)
    {
      calls[[length(calls)]] <- NULL;
    }
    if(length(calls) == 1L)
    {
      if(isname(calls[[1L]], "this"))
      {
        return(obj);
      }
      else if(isname(calls[[1L]], obj@inhr))
      {
        this$isInhr_ <- TRUE;
        return(eval(calls[[1L]], obj@encl));
      }
      else
      {
        return(NULL);
      }
    }

    calls[[1L]] <- NULL;
    len <- length(calls);
    for(i in seq_len(len))
    {
      if(is.null(obj)) return(NULL);
      call <- calls[[i]];
      oper <- as.character(call$oper);
      rhs  <- as.character(call$rhs);

      # access the class that a container holds
      if(i < len && isname(calls[[i + 1]]$oper, "["))
      {
        cont <- classmem_get_containers(obj@meta, obj@encl$this);
        if(!cont[rhs]) return(NULL);
        obj  <- classmem_get_ooprC(rhs, obj@meta, obj@encl$this, cont, TRUE);
        next;
      }

      # access the enclosure of each oopr class constructor
      if(is.ooprC(obj)) switch(oper
        ,"[" = # skip this (happened in prior loop)
        {
          next;
        }
        ,"$" = , "[[" =
        {
          obj <- if(any(obj@meta$subs(names = rhs))) obj@encl$this else NULL;
        }
        ,
        {
          obj <- NULL;
        }
      );

      obj <- do.call(oper, list(obj, call$rhs));
    }
    if(is.ooprC(obj))
    {
      this$isCMem_ <- TRUE;
    }
    return(obj);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  flattenCall <- \(x)
  {
    if(!is.call(x)) return(list(x))
    c(this$flattenCall(x[[2L]]), list(list(oper = x[[1L]], rhs = x[[3L]])));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  ooprCNames <- \(obj)
  {
    class  <- obj@name;
    thiz   <- obj@encl$this;
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
    names <- obj@meta$subs("names", access = access);
    names <- grep(sprintf("^~?%s$", class), names, value = TRUE, invert = TRUE);
    return(.DollarNames.oopr(thiz, names = names));
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
}) ## OoprCompletion
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname OoprCompletion
#' @description
#' Read the Rd (documentation) of an `oopr` class.
#'
#' @details
#' Will read the `.Rd` file that holds the `oopr` class, then provides
#' methods to access specific information.
#'
#' Used to help with documentation auto-completion in RStudio.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprRd",,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @param topic   `character(1L)` \cr
#'                The class name.
#'
#' @param package `character(1L)` \cr
#'                The package that the class lives in.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
OoprRd <- \(topic, package)
{
  call <- substitute(
    help(topic = .T, package = .P)
   ,list(.T = topic, .P = package)
  );
  tryCatch(
    help <- eval(call, globalenv())
   ,error = \(e) this$fail <- TRUE
  )
  if(this$fail) return();

  if(inherits(help, "dev_topic"))
  {
    rd <- tools::parse_Rd(help$path);
  }
  else
  {
    rd <- tools::Rd_db(package, lib.loc = dirname(dirname(dirname(help))));
    rd <- rd[[match(sprintf("%s.Rd", basename(help)), names(rd), 0L)]];
  }

  this$topic   <- topic;
  this$package <- package;
  this$rd      <- this$pullSection(topic, rd);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field topic `character(1L)` \cr
  #'              The class name.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  topic   <- character(1L);

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field package `character(1L)` \cr
  #'                The package that the class lives in.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  package <- character(1L);

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field rd `list()` \cr
  #'           The parsed `Rd`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  rd      <- list();

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field fail `logical(1L)` \cr
  #'             Whether the Rd was found and parsed at construction.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  fail    <- FALSE;

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Get the description of a field or method.
  #'
  #' @param name `character(1L)` \cr
  #'             The name of the field or method.
  #'
  #' @param html `logical(1L)` \cr
  #'             Whether to to output as HTML, otherwise `Rd`.
  #'
  #' @details
  #' If `name` is not valid, then a zero-character is returned.
  #'
  #' @returns
  #' If `html` is `TRUE`, then `character(1L)`, otherwise an `Rd`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  getDescription <- \(name, html = TRUE)
  {
    if(this$fail) return("");
    for(type in c("Fields", "Methods"))
    {
      rd    <- this$pullSection(type);
      if(is.null(rd)) next;
      rd    <- rd[[this$whichTag("\\describe", rd)]];
      rd    <- rd[this$whichTag("\\item", rd)];
      names <- vapply(rd, `[[`, character(1L), c(1L, 1L, 1L));
      m     <- match(name, names, 0L);
      if(m) break;
    }
    if(!m) return("");
    rd <- rd[[c(m, 2L)]];
    if(html)
    {
      rd <- this$toHTML(rd);
    }
    else
    {
      class(rd) <- "Rd";
    }
    return(rd);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprRd$getDescription
  #' @description
  #' Get the subsection of a method.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  getMethod <- \(name, html = FALSE)
  {
    if(this$fail) return("");
    rd <- this$pullSection(name);
    if(is.null(rd)) return("");
    if(html)
    {
      rd <- this$toHTML(rd);
    }
    return(rd);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprRd$getDescription
  #' @description
  #' Get the arguments of a method.
  #'
  #' @returns
  #' If `html` is `TRUE`, then a named character vector.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  getArguments <- \(name, html = TRUE)
  {
    if(this$fail) return("");
    rd <- this$getMethod(name, html = FALSE);
    if(is.null(rd)) return("");
    rd <- this$pullSection("Arguments", rd);
    if(!html) return(rd);
    rd <- rd[this$whichTag("\\tabular", rd)];
    rd <- this$toHTML(rd);
    rd <- paste(rd, collapse = "\n");

    p <- "(?xs)
    <tr>(.*?)
    <td(.*?)>\\s*(?'key'.*?)\\s*</td>
    <td(.*?)>\\s*(?'val'.*?)\\s*</td>
    (.*?)</tr>
    "
    m  <- gregexec(p, rd, perl = TRUE)[[1L]];
    l  <- attr(m, "match.length");
    rd <- rep.int(rd, ncol(m));

    desc <- substr(rd, m["val", ], m["val", ] + l["val", ] - 1L);
    args <- substr(rd, m["key", ], m["key", ] + l["key", ] - 1L);
    args <- sub("(<code>)(.*?)(</code>)", "\\2", args);
    names(desc) <- args;
    return(desc);
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @param `tag` `character(1L)` \cr
  #'              An `Rd` tag, e.g. `"\\section"`.
  #'
  #' @returns
  #' `integer` of the position containing `tag`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  whichTag <- \(tag, rd = this$rd)
  {
    tags <- vapply(rd, attr, character(1L), "Rd_tag");
    return(which(match(tags, tag, 0L) > 0L));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @returns
  #' `list()` of the `Rd` section or subsection `name`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  pullSection <- \(name, rd = this$rd)
  {
    class <- class(rd);
    rd    <- rd[this$whichTag(c("\\section", "\\subsection"), rd)];
    names <- vapply(rd, \(x) { trimws(tail(x[[1L]], 1L)) }, character(1L));
    m     <- match(name, names, 0L);
    rd    <- if(m) rd[[c(m, 2L)]] else return(NULL);
    class(rd) <- class;
    return(rd);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @param desc `logical(1L)` \cr
  #'             Force `rd` to be a `description`.
  #' @returns
  #' `character()` containing the converted HTML.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  toHTML <- \(rd = this$rd, desc = TRUE)
  {
    if(desc)
    {
      attr(rd, "Rd_tag") <- "\\description";
    }
    verb <- list(`attr<-`("A", "Rd_tag", "VERB"));
    verb <- lapply(c("\\name", "\\title"), \(x) `attr<-`(verb, "Rd_tag", x));
    rd   <- c(verb, list(rd));
    tmp  <- tempfile();
    on.exit(unlink(tmp));
    tools::Rd2HTML(rd, tmp, standalone = FALSE);
    out  <- readLines(tmp);
    out  <- out[nzchar(out)];
    if(desc)
    {
      out <- out[out != "<h3>Description</h3>"];
    }
    return(out[-1L]);
  }
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
}) ## OoprRd
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#'
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
  package <- environmentName(topenv(oopr));

  this$tpc_  <- topic;
  this$src_  <- source;
  this$cls_  <- class;
  this$pkg_  <- package;
  this$oopr_ <- oopr;
  this$rd(class, package);
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
      fun <- get("OoprCompletionHelp", envir = getNamespace("oopr"));
      fun <- fun@encl$.this$getHelp;
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

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  rd <- OoprRd;

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
  makeCompletion <- \(topic = this$tpc_, oopr = this$oopr_)
  {
    description <- paste(this$rd$getDescription(topic), collapse = "");
    OoprCompletion$isGettingNames <- TRUE;
    on.exit(OoprCompletion$isGettingNames <- FALSE);
    obj         <- tryCatch(.subset2(oopr, topic), error = identity);
    signature   <- topic;
    if(is.ooprC(obj) || is.oopr(obj))
    {
      signature <- format(obj);
    }
    else if(is.function(obj))
    {
      if(topic != make.names(topic))
      {
        topic <- sprintf("`%s`", topic);
      }
      signature <- deparse(args(obj), width.cutoff = 500L, nlines = 1);
      signature <- sub("function ", topic, signature);
    }
    else if(inherits(obj, "error"))
    {
      signature <- "?";
    }
    else
    {
      signature <- typeof(obj);
      if(is.vector(obj))
      {
        signature <- sprintf("%s(%iL)", signature, length(obj));
      }
    }
    if(!is.function(obj))
    {
      this$out_$title      <- topic;
    }
    this$out_$description  <- description;
    this$out_$signature    <- signature;
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  makeParameter  <- \(topic = this$tpc_)
  {
    topic <- deparse1(str2lang(topic));
    arg_descriptions <- this$rd$getArguments(topic);
    this$out_$args <- names(arg_descriptions);
    this$out_$arg_descriptions <- arg_descriptions;
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  makeUrl <- \(topic = this$cls_, package = this$pkg_)
  {
    call <- substitute(
      help(topic = topic, package = package)
     ,list(topic = topic, package = package)
    );
    print(eval(call, globalenv()));
    return(NULL);
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
}) ## OoprCompletionHelp
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
help_formals_handler.oopr <- \(topic, source)
{
  if(is.oopr(source, "oopr_this"))
  {
    comp   <- OoprCompletion();
    source <- if(comp$isCompletion()) comp$obj() %||% stop() else stop();
  }
  topic   <- deparse1(str2lang(topic));
  formals <- sprintf("%s = ", names(formals(.subset2(source, topic))));
  help    <- OoprCompletionHelp@encl$.this$makeHelpHandler(source);
  return(list(formals = formals, helpHandler = help))
}

