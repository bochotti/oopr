## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @name oopr_roclet
#' @title Roxygen for oopr
#' @export
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr_roclet <- \( ) roxygen2::roclet("oopr")

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @exportS3Method roxygen2::roclet_preprocess roclet_oopr
#' @intern
#' Finds the roxy blocks containing `ooprC` objects.
#' Inserts `<-` into the call so roxygen2 will pickup a symbol.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
roclet_preprocess.roclet_oopr <- \(x, blocks, base_path)
{
  OoprRoxy$classes$resize();
  env <- get("env", envir = sys.frame(-3L));
  for(i in seq_along(blocks))
  {
    block <- blocks[[i]];
    if(!is.null(block$object))   next;
    if(!is.ooprcall(block$call)) next;
    name         <- match.call(oopr, block$call)$name;
    block$call   <- call("<-", as.name(name), block$call);
    class(block) <- c("roxy_block_oopr", "roxy_block");

    if(roxygen2::block_has_tags(block, "exportS3Method"))
    {
      rdname <- roxygen2::block_get_tag(block, c("name", "rdname"));
      for(tag in roxygen2::block_get_tags(block, "exportS3Method"))
      {
        # tags <- list(rdname, tag);
        tags <- list(tag);
        call <- sub("^\"(.*?)\"", "\\1", tag$raw);
        call <- as.name(sub(" ", ".", call));
        call <- call("<-", call, get(call, envir = env));
        blocks[[length(blocks) + 1L]] <- roxygen2::roxy_block(
          tags, block$file, block$line, call, NULL
        );
        rm <- vapply(block$tags, identical, logical(1L), tag);
        block$tags <- block$tags[!rm];
      }
    }
    blocks[[i]]  <- block;
  }
  assign("blocks", blocks, envir = sys.frame(-3), inherits = FALSE);
  return(x);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @exportS3Method roxygen2::block_to_rd roxy_block_oopr
#' @intern
#' Allows specific handling of creating Rd files for `oopr` objects.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
block_to_rd.roxy_block_oopr <- \(block, base_path, env)
{
  block <- OoprRoxy$addBlock(block);
  NextMethod();
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @exportS3Method roxygen2::roclet_process roclet_oopr
#' @intern
#' Injects code when the lapply of `roclet_process` finishes.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
roclet_process.roclet_oopr <- \(x, blocks, env, base_path)
{
  expr <- quote(OoprRoxy$insertHeader());
  do.call(on.exit, list(expr, TRUE, FALSE), envir = sys.frame(-2L));
  return(x)
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @exportS3Method roxygen2::roclet_output roclet_oopr
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
roclet_output.roclet_oopr <- \(x, results, base_path, ...)
{
  OoprRoxy$classes$resize();
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @name OoprRoxy
#' @title OoprRoxy Internals
#' @description
#' `OoprRoxy` is called by `roxygen2` methods. It creates `OoprRoxyClass`
#' which creates a roxy block containing sections for each class.
#'
#' **TODO**:
#'
#'   1.  `@internal` .Rbuildignore can be used to hide internals.
#'
#'   2.  Hyperlinks for each class section, and inside those further links
#'       for methods.
#'         - Class hyperlinks can be inside `Format` section.
#'         - Would be useful to have a ↩ next to each heading.
#'
#'   3.  Try to constrain horizontal width for nested sections.
#'
#'   4.  Tidy up pdf
#'
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
OoprRoxy <- NULL;

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname OoprRoxy
#' @keywords internal
#' @description
#' Create a subsection.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprRoxySection",,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @param title   `character(1L)` \cr
#'                The title of the section.
#'
#' @param content `character()` \cr
#'                Lines of the content for the section.
#'
#' @param hr      `logical(1L)` \cr
#'                Whether to add horizontal line to section heading.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
OoprRoxySection <- \(title = "", content = character(0L), hr = FALSE)
{
  stopifnot(
    is.character(title)   && length(title) == 1L
   ,is.character(content)
   ,is.logical(hr) && length(hr) == 1L && !is.na(hr)
  );
  this$title_   <- title;
  this$content_ <- content;
  this$hr_      <- hr;
}
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:title <- \( )
  {
    return(this$title_);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:content  <- \( )
  {
    return(this$content_);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field size `integer(1L)` \cr
  #'             The number of lines.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:size <- \( )
  {
    return(length(this$content));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Insert a line into the section.
  #'
  #' @param x `character()` \cr
  #'          The line to insert.
  #'
  #' @param i `integer() | character()` \cr
  #'          The position to insert, must be the same length as `x`.
  #'
  #' @returns
  #' `this` invisibly.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  virtual:insert <- \(x, i = this$size + 1L)
  {
    stopifnot(
      is.character(x)
     ,is.character(i) || (is.numeric(i) && all(i %% 1L == 0L))
     ,length(x) == length(i)
    );
    this$content_[i] <- x;
    return(invisible(this));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Remove all lines from the section.
  #'
  #' @returns
  #' `this` invisibly.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  final:erase <- \( )
  {
    this$content_ <- character(0L);
    return(invisible(this));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Convert the section into Rd formatted text.
  #'
  #' @details
  #' Uses the output from virtual method `$format()`.
  #'
  #' @returns
  #' `character(1L)`, wrapped in `\subsection`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  final:toRd <- \( )
  {
    content <- this$format();
    if(!(is.character(content) && all(!is.na(content))))
    {
      stop("$format must return a non-NA character vector");
    }
    title <- this$title_;
    if(this$hr_)
    {
      title <- sprintf("\\hr{}%s", title);
    }
    content <- paste(content, collapse = "\n\n");
    content <- sprintf("\\subsection{%s}{\n%s\n}", title, content);
    return(content);
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
protected:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Convert `$content` into a format for use in `$toRd()`.
  #'
  #' @returns
  #' Must return `character()`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  virtual:format <- \( )
  {
    return(this$content_);
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  title_   <- character(1L);
  content_ <- character(0L);
  hr_      <- logical(1L);

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
}) ## OoprRoxySection
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname OoprRoxy
#' @keywords internal
#' @description
#' Represents a describe subsection.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprRoxyDescribe", public:OoprRoxySection,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
OoprRoxyDescribe <- \(title = "Fields", hr = FALSE)
{
  OoprRoxySection(title, hr = hr);
}
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprRoxySection$insert
  #'
  #' @param x `character() | roxy_tag_field` \cr
  #'          Item to insert. If a field roxy tag, then takes the `$val`
  #'          contents to set `x` and `i`.
  #'
  #' @param i `character()` \cr
  #'          The name of the item.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  insert <- \(x, i = character(1L))
  {
    if(inherits(x, "roxy_tag_field"))
    {
      i <- x$val$name;
      x <- x$val$description;
    }
    stopifnot(is.character(i));
    OoprRoxySection$insert(x, i);
    return(invisible(this));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field names `character(0L)` \cr
  #'              Name of each item.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:names <- \( )
  {
    return(names(this$content));
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
protected:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  format <- \( )
  {
    content <- trimws(this$content);
    content <- sprintf("\\item{\\code{%s}}{\n%s}", this$names, content);
    content <- paste0(content, collapse = "\n\n");
    content <- sprintf("\\describe{\n%s\n}", content);
    return(content);
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
}) ## OoprRoxyDescribe
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname OoprRoxy
#' @keywords internal
#' @description
#' Represents a usage subsection.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprRoxyUsage", public:OoprRoxySection,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
OoprRoxyUsage <- \(content = character(0L), name = "")
{
  stopifnot(is.character(name) && length(name) == 1L);
  if(is.function(content))
  {
    content <- this$makeUsageFromFun(content, name);
  }
  OoprRoxySection("Usage", content);
}
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
protected:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  format <- \( )
  {
    content <- paste(this$content, collapse = "\n");
    content <- sprintf("\\rc{\\preformatted{%s}}", content);
    return(content);
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  makeUsageFromFun <- \(fun, name)
  {
    args <- formals(fun);
    if(name != make.names(name) && !grepl("^`.*`$", name))
    {
      name <- sprintf("`%s`", name);
    }

    if(is.null(args)) return(sprintf("%s()", name));

    dflt <- vapply(args, deparse1, character(1L));
    dflt[nzchar(dflt)] <- sprintf(" = %s", dflt[nzchar(dflt)]);

    out <- sprintf("%s%s", names(args), dflt);
    out <- sprintf("%s(%s)", name, paste(out, collapse = ", "));
    if(nchar(out) <= 40L) return(out);

    out <- sprintf(
      "%s%s%s"
     ,rep(c("  ", " ,"), c(1L, length(args) - 1L))
     ,format(names(args))
     ,dflt
    );
    out <- sprintf("%s(\n%s\n)", name, paste(out, collapse = "\n"));
    return(out);
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
}) ## OoprRoxyUsage
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname OoprRoxy
#' @keywords internal
#' @description
#' Create an arguments subsection.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprRoxyArguments", public:OoprRoxyDescribe,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @param args `list()` \cr
#'             A list of `roxy_tag_param`s.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
OoprRoxyArguments <- \(args = list())
{
  OoprRoxyDescribe("Arguments");
  for(arg in args) this$insert(arg);
}
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprRoxySection$insert
  #'
  #' @param x `character() | roxy_tag_param` \cr
  #'          Item to insert. If a param roxy tag, then takes the `$val`
  #'          contents to set `x` and `i`.
  #'
  #' @param i `character()` \cr
  #'          The name of the argument.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  insert <- \(x, i = character(1L))
  {
    if(inherits(x, "roxy_tag_param"))
    {
      i <- x$val$name;
      x <- gsub("\\\\cr", "\\\\br", x$val$description);
    }
    stopifnot(is.character(i));
    OoprRoxyDescribe$insert(x, i);
    return(invisible(this));
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
protected:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  format <- \( )
  {
    content <- sprintf("\\code{%s} \\tab %s \\cr", this$names, this$content);
    content <- paste(content, collapse = "\n");
    content <- sub(" \\\\cr$", "", content);
    content <- sprintf("\\ar{#1}{#2}\\tabular{ll}{\n%s}", content);
    return(content);
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
}) ## OoprRoxyArguments
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname OoprRoxy
#' @keywords internal
#' @description
#' Represents a method subsection.
#'
#' @details
#' Combines many sections into a single subsection. If the tags
#' `@description`, `@returns` and `@param` are not provided, a warning will
#' display.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprRoxyMethod", public:OoprRoxySection,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @param tags `list()` \cr
#'             A list of (potentially optional) tags:
#'             `@description`, `@usage`, `@param`, `@details` & `@returns`.
#'
#' @param fun  `function` \cr
#'             The function object of the method.
#'
#' @param warn `logical(1L)` \cr
#'             Whether warnings should display when missing tags.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
OoprRoxyMethod <- \(title, tags, fun, warn = TRUE, hr = TRUE)
{
  stopifnot(
    is.list(tags) && all(vapply(tags, inherits, logical(1L), "roxy_tag"))
   ,is.function(fun)
   ,is.logical(warn) && length(warn) == 1L && !is.na(warn)
  );

  this$fun_   <- fun;
  this$title_ <- title;
  this$warn_  <- warn;

  if(grepl("^`.*`$", title))
  {
    title <- sprintf("`` %s ``", title);
  }
  else
  {
    title <- sprintf("`%s`", title);
  }
  OoprRoxySection(title, hr = hr);

  this$checkMissingTags(tags);
  this$insertArgsSection(tags);

  ord <- c("description", "usage", "arguments", "details", "returns");
  u   <- \(x) { `substr<-`(x, 1L, 1L, toupper(substr(x, 1L, 1L))); }
  for(tag in tags)
  {
    if(!match(tag$tag, ord[c(1L, 4:5)], 0L)) next;
    val <- tag$val; nm <- u(tag$tag);
    val <- sprintf("\\lbr{}%s", val);
    if(this$sections$exists(nm))
    {
      if(is.null(tag$INHR_)) this$sections[nm]$insert(val);
    }
    else
    {
      this$sections$emplace(nm, nm, val);
    }
  }
  keys <- this$sections$keys;
  this$sections$resize(keys[match(u(ord), keys, 0L)]);
  this$warning();
}
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprRoxySection$title
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:title <- \( )
  {
    return(this$title_);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:fun <- \( )
  {
    return(this$fun_);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:warn <- \( )
  {
    return(this$warn_);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field sections `OoprRoxySection` \cr
  #'                 A container of the subsections.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  sections <- OoprRoxySection[[]];

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
protected:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  format <- \( )
  {
    content <- unlist(this$sections$apply(\(k, v) v$toRd()));
    return(content);
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  title_    <- character(1L);
  fun_      <- NULL;
  warn_     <- logical(1L);
  warns     <- character(0L);

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:roxy <- \( )
  {
    getNamespace("roxygen2");
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' checks for missing sections, will save warning. `@usage` has a default.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  checkMissingTags <- \(tags, fun = this$fun, title = this$title)
  {
    names <- vapply(tags, `[[`, character(1L), "tag");

    req   <- c("description", "returns");
    miss  <- match(req, names, 0L) == 0L;
    switch(sum(miss)
     ,{
        this$warns[1L] <- sprintf("Requires @%s tag", req[miss]);
      }
     ,{
        this$warns[1L] <- "Requires @description and @returns tags";
      }
    )

    if(!match("usage", names, 0L))
    {
      this$sections$insert("Usage", OoprRoxyUsage(fun, title));
    }
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' saves warning if `@param` mis-matched with actual arguments
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  insertArgsSection <- \(tags, fun = this$fun)
  {
    tags <- tags[vapply(tags, `[[`, character(1L), "tag") == "param"];
    args <- names(formals(fun));
    docd <- vapply(tags, `[[`, character(1L), c("val", "name"));
    inhr <- vapply(tags, \(x) !is.null(x$INHR_), logical(1L));

    notDoc <- match(args, docd, 0L) == 0L;
    if(any(notDoc))
    {
      this$warns[length(this$warns) + 1L] <- sprintf(
        "Argument%s %s %s not documented"
       ,if(sum(notDoc) > 1L) "s"   else ""
       ,deparse1(args[notDoc])
       ,if(sum(notDoc) > 1L) "are" else "is"
      );
    }

    notArg <- match(docd, args, 0L) == 0L & !inhr;
    if(any(notArg))
    {
      this$warns[length(this$warns) + 1L] <- sprintf(
        "Documented argument%s %s %s not in the signature"
       ,if(sum(notArg) > 1L) "s"   else ""
       ,deparse1(docd[notArg])
       ,if(sum(notArg) > 1L) "are" else "is"
      );
    }

    tags <- tags[match(docd, args, 0L) > 0L & !duplicated.default(docd)];
    docd <- vapply(tags, `[[`, character(1L), c("val", "name"));
    tags <- tags[match(args, docd, 0L)];
    if(length(tags)) this$sections$insert("Arguments", OoprRoxyArguments(tags));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  warning <- \(fun = this$fun)
  {
    if(!(this$warn && length(this$warns))) return();
    clss <- class(environment(fun)[[".this"]])[1L];
    src  <- attr(fun, "srcref");
    line <- src[1L];
    file <- attr(src, "srcfile")$filename;
    tag  <- list(tag = "oopr", file = file, line = line);
    msg  <- c(
      sprintf("Issue/s with method \"%s$%s\":", clss, this$title)
     ,unlist(lapply(
        this$warns
       ,strwrap
       ,prefix  = strrep("\u00a0", 6)
       ,initial = sprintf("%s- ", strrep("\u00a0", 4))
      ))
    );
    this$roxy$warn_roxy_tag(tag, msg);
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
}) ## OoprRoxyMethod
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname OoprRoxy
#' @keywords internal
#' @description
#' Represents a class section.
#'
#' @details
#' Combines many sections into a single top-level section for a class.
#'
#' Methods need to be called in order.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprRoxyClass",,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @param block `roxy_block` \cr
#'              A roxy block containing an `oopr` class.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
OoprRoxyClass <- \(block)
{
  stopifnot(
    inherits(block, "roxy_block")
   ,is.ooprC(block$object$value)
  );
  this$block_   <- block;
  this$title_   <- block$object$value@name;
  this$warn_    <- this$roxy$block_has_tags(block, "export");
  this$members_ <- this$pullMemberTags();
  this$fillMembers();
}
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprRoxySection$title
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:title <- \( )
  {
    return(this$title_);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:block <- \( )
  {
    return(this$block_);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field members `list()` \cr
  #'                A (nested) list of tags for each member.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:members <- \( )
  {
    return(this$members_);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field sections `OoprRoxySection` \cr
  #'                 The subsections inside the class section.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  sections <- OoprRoxySection[[]];

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Creates subsections for `@description` and `@details` tags.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  makeSections <- \( )
  {
    tags <- vapply(this$tags, `[[`, character(1L), "tag");
    want <- c("description", "details");
    for(tag in want)
    {
      if(!match(tag, tags, 0L)) next;
      title <- tag;
      substr(title, 1L, 1L) <- toupper(substr(title, 1L, 1L));
      this$sections$emplace(title, title);

      for(tag in this$tags[match(tags, tag, 0L) > 0L])
      {
        tag$val <- sprintf("\\lbr{}%s", tag$val);
        this$sections[title]$insert(tag$val);
      }
    }
    this$tags <- this$tags[!match(tags, want, 0L)];
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Creates a subsection for a list of fields/properties.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  makeFields <- \( )
  {
    fields <- OoprRoxyDescribe("Fields");
    names  <- names(this$members_);
    names  <- this$ooprC@meta$subs("names", names = names, method = FALSE);

    for(name in names)
    {
      tags <- this$members_[[name]];
      tags <- this$findInheritsTag(tags, name);
      lapply(tags, \(x) if(inherits(x, "roxy_tag_field")) fields$insert(x));
    }

    if(fields$size)
    {
      this$addSpecifiersToDescribe(fields);
      this$sections$insert("Fields", fields);
    }
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Creates a subsection for a list of methods and inserts method sections.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  makeMethods <- \( )
  {
    methods <- OoprRoxyDescribe("Methods");
    this$sections$insert("Methods", methods);

    names <- this$unWrapNames(names(this$members_));
    names <- this$ooprC@meta$subs("names", names = names, method = TRUE);
    names <- this$wrapNames(names);

    for(name in names)
    {
      tags   <- this$members_[[name]];
      tags   <- this$findInheritsTag(tags, name);
      fun    <- this$ooprC@encl$this[[this$unWrapNames(name)]];
      method <- OoprRoxyMethod(name, tags, fun, this$warn_);
      this$sections$insert(name, method);

      # add to list
      desc <- "";
      if(method$sections$exists("Description"))
      {
        desc <- method$sections["Description"]$content;
        desc <- sub("^\\\\lbr\\{\\}", "", desc);
      }
      methods$insert(desc, name);
    }

    if(methods$size)
    {
      this$addSpecifiersToDescribe(methods);
    }
    else
    {
      this$sections$erase("Methods");
    }
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Places the class section under the blocks tags list
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  makeTag <- \( )
  {
    content <- this$sections$apply(\(k, v) v$toRd());
    content <- paste(content, collapse = "\n\n");
    content <- sprintf("\\hr\\hr{}%s:\n%s\n", this$title_, content);
    if(!this$sections$exists("Methods"))
    {
      content <- sprintf("%s\\hr", content);
    }
    tag <- this$roxy$roxy_tag_parse(this$roxy$roxy_tag("section", content));
    this$tags[[length(this$tags) + 1]] <- tag;
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  title_   <- character(1L);
  block_   <- list(tags=list(), file="", line=0L, call=NULL, object=NULL);
  members_ <- list();
  warn_    <- FALSE;
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:roxy  <- \( ) { return(getNamespace("roxygen2")); }
  get:tags  <- \( ) { return(this$block_$tags); }
  set:tags  <- \(x) { this$block_$tags <- x; }
  get:ooprC <- \( ) { return(this$block_$object$value); }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:wrapNames <- \(x)
  {
    i    <- x != make.names(x);
    x[i] <- sprintf("`%s`", x[i]);
    return(x);
  }
  static:unWrapNames <- \(x)
  {
    return(vapply(x, \(x) deparse1(str2lang(x)), character(1L)));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Tokenises the contents of definition `{ ... }` in the `oopr` call.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  pullMemberTags <- \(ooprC = this$ooprC)
  {
    src <- attr(ooprC, "srcref");
    txt <- as.character(src);

    # remove surrounding curlies { ... }
    txt[1L]          <- sub("^\\{", "", txt[1L]);
    txt[length(txt)] <- sub("\\}$", "", txt[length(txt)]);

    # add empty lines above to align the line number of tags
    txt <- c(character(src[1L] - 1L), txt, "");

    # tokenize as though there are multiple objects in one file
    tmp <- tempfile();
    on.exit(unlink(tmp));
    cat(txt, sep = "\n", file = tmp);
    blocks <- this$roxy$tokenize_file(tmp)
    for(i in seq_along(blocks))
    {
      # get the name of the member
      name <- blocks[[i]]$call[[2L]];
      if(iscall(name, ":"))
      {
        name <- name[[3L]]
      }
      name <- this$wrapNames(deparse1(name));
      names(blocks)[i] <- name;
      # only keep the tags
      blocks[[i]] <- blocks[[i]]$tags;
      # use the correct file name
      for(j in seq_along(blocks[[i]]))
      {
        blocks[[i]][[j]]$file <- this$block$file;
      }
    }

    # remove constructor+destructor
    rm <- sprintf("%s%s", c("", "~"), this$title);
    rm <- match(names(blocks), rm, 0L) > 0L;
    blocks <- blocks[!rm];

    # remove from the block via line number
    l  <- vapply(this$tags, `[[`, integer(1L), "line");
    rm <- match(l, rapply(blocks, identity, "integer"), 0L) > 0L;
    this$tags <- this$tags[!rm];

    return(blocks);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  warning <- \(
    fmt, ..., file = this$block_$file, line = this$block_$line, tag = "oopr"
  )
  {
    if(!this$warn_) return();
    tag <- list(tag = tag, file = file, line = line);
    this$roxy$warn_roxy_tag(tag, sprintf(fmt, ...));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Attempts to fill missing member tags.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  fillMembers <- \( )
  {
    names <- this$ooprC@meta$subs("names", access = c("public", "protected"));
    cargs <- names(formals(this$ooprC@encl$this[[this$title_]]));
    miss  <- logical(length(names));

    for(i in seq_along(names))
    {
      name <- names[i];

      # protected members are optional
      if(this$ooprC@meta$subs("access", names = name) == "protected") next;

      base  <- this$ooprC@meta$subs("inherit", names = name);
      field <- !this$ooprC@meta$subs("method", names = name);

      name <- this$wrapNames(name);
      if(match(name, names(this$members), 0L)) next;
      tags <- list();

      # if inherited member, automatically inherit
      if(nzchar(base))
      {
        tags[[1L]] <- this$roxy$roxy_tag_parse(this$roxy$roxy_tag(
          tag  = "inherit"
         ,raw  = sprintf("%s$%s", base, name)
         ,file = this$block_$file
         ,line = this$block_$line
        ));
      }
      # if a field, carry forward the param from constructor
      else if(field && match(name, this$wrapNames(cargs), 0L))
      {
        idx  <- vapply(this$tags, `[[`, character(1L), "tag") == "param";
        tags <- this$tags[idx];
        idx  <- vapply(tags, `[[`, character(1L), c("val", "name")) == name;
        tags <- tags[idx];
        if(length(tags))
        {
          tags[[1L]]$tag <- "field";
          class(tags[[1L]]) <- c("roxy_tag_field", "roxy_tag");
        }
      }

      if(length(tags))
      {
        this$members_[[name]] <- tags;
      }
      else
      {
        miss[i] <- TRUE;
      }
    }

    # throw warning for missing
    if(any(miss)) this$warning(
      "Member%s %s in class %s %s not documented"
     ,if(sum(miss) > 1L) "s"   else ""
     ,deparse1(names[miss])
     ,deparse1(this$title_)
     ,if(sum(miss) > 1L) "are" else "is"
    );

    # remove private members
    this$members_ <- this$members_[this$wrapNames(names)];
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Locates the tags that `@inherit` points to
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  findInheritsTag <- \(tags, name)
  {
    has <- vapply(tags, inherits, logical(1L), "roxy_tag_inherit");
    if(!any(has)) return(tags);

    for(i in which(has))
    {
      tag <- tags[[i]];
      src <- strsplit(tag$val$source, "\\$")[[1L]];
      err <- character(1L);
      if(length(src) != 2 || !all(nzchar(src)))
      {
        err <- "tag should be in the form x$y";
      }
      # check within the same class
      else if(src[1L] == this$title && match(src[2L], names(this$members_), 0L))
      {
        oth <- this$members_[[src[2L]]];
      }
      else if(!OoprRoxy$classes$exists(src[1L]))
      {
        # TODO: check other packges...?
        err <- sprintf("class %s is not documented", src[1L]);
      }
      else if(!match(src[2L], names(OoprRoxy$classes[src[1L]]$members), 0L))
      {
        err <- sprintf("member %s$%s is not documented", src[1L], src[2L]);
      }
      else
      {
        oth <- OoprRoxy$classes[src[1L]]$members[[src[2L]]];
      }
      if(nzchar(err))
      {
        this$warning(err, file = tag$file, line = tag$line, tag = "inherit");
        next;
      }

      oth <- lapply(oth, `[[<-`, "INHR_", TRUE);
      tags <- c(tags, oth);
      this$members_[[name]] <- c(this$members_[[name]], oth);
    }

    return(tags);
  }


  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Prefixes describe items with specifiers
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  addSpecifiersToDescribe <- \(section)
  {
    specs   <- character(section$size);
    names   <- section$names;
    for(which in c(
      "access", "property", "S3", "static", "container", "virtual", "final"
    ))
    {
      i <- this$ooprC@meta$subs(which, names = this$unWrapNames(names));
      if(which == "access")
      {
        i <- i == "protected";
        which <- "protected";
      }
      else if(which == "property")
      {
        which <- character(length(i));
        which[i == "get"] <- "read-only";
        which[i == "set"] <- "write-only";
        i <- nzchar(which);
        which <- which[i];
      }
      specs[i] <- sprintf("%s *`[%s]`*", specs[i], which);
    }
    nz <- nzchar(specs);
    specs[nz]  <- sprintf("%s \\cr\n", specs[nz]);
    specs[!nz] <- "\\lbr{}";
    content <- trimws(sprintf("%s%s", specs, section$content));
    section$erase();
    section$insert(content, names);
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
}) ## OoprRoxyClass
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprRoxy",,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field classes `OoprRoxyClass[[]]` \cr
  #'                `oopr` classes being documented.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:classes <- OoprRoxyClass[[]];

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Add a block and insert into `$classes` field.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:addBlock <- \(block)
  {
    obj <- OoprRoxyClass(block);
    obj$makeSections();
    obj$makeFields();
    obj$makeMethods();
    obj$makeTag();
    this$classes$insert(obj$title, obj)
    return(obj$block);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Get the value of `@name` tags for the `oopr` classes.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:getTopicNames <- \(sfx = ".Rd")
  {
    if(this$classes$empty) return(character(0L));
    topics <- this$classes$apply(\(k, v) {
      tags <- v$block$tags;
      for(t in tags) if(match(t$tag, c("name", "rdname"), 0L)) return(t$val);
      return(character(0L))
    });
    out <- unlist(topics);
    out <- `names<-`(sprintf("%s%s", out, sfx), names(out));
    return(out);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Insert required `.Rd` code into `RoxyTopics` holding `oopr` classes.
  #'
  #' @details
  #' Injected in `roxygen2::roxygenise` call to `roclet_process`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:insertHeader <- \(results = returnValue(), env = parent.frame())
  {
    force(env); force(results);
    rrd    <- which(vapply(env$X, inherits, logical(1L), "roclet_rd"));
    topics <- results[[rrd]];
    names  <- this$getTopicNames();
    for(name in unique(names))
    {
      topic <- topics[[name]];
      keys  <- names(names)[names == topic$filename];
      this$insertHeaderSection(topic);
      this$insertTableOfContents(topic, keys);
      this$insertHRule(topic, keys);
    }
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Create `\if{...}{...}` .Rd statements.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:switch <- \(html = "", latex = "", text = "", out = TRUE, sep = "")
  {
    wrap <- \(x, out = TRUE)
    {
      name <- as.character(substitute(x));
      x    <- paste(x, collapse = "\n")
      if(!nzchar(x)) return(character(0L));
      if(out)
      {
        x <- sprintf("\\out{%s}", x);
      }
      return(sprintf("\\if{%s}{%s}", name, x));
    }
    out <- c(wrap(html, out), wrap(latex, out), wrap(text, out));
    return(paste(out, collapse = sep));
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Insert .Rd commands into the value section
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:insertHeaderSection <- \(topic)
  {
    newcmd <- \(nm, html = "", latex = "", text = "")
    {
      sprintf("\\newcommand{\\%s}{%s}", nm, this$switch(html, latex, text));
    }

    header <- paste(collapse = "\n", c(
      newcmd("br"  ,"<br>"     ,"\\newline{}")
     ,newcmd("lbr" ,""         ,"\\hspace{0em}\\newline{}")
     ,newcmd("hr"  ,"<hr>"     ,"\\hrule\\vspace{0.5em}")
     ,sprintf(
        "\\newcommand{\\rc}{%s#1%s}"
       ,r"{\if{html}{\out{<pre><code class="language-R">}}}"
       ,r"{\if{html}{\out{</code></pre>}}}"
      )
     ,newcmd(
        "ar"
       ,r"{<h3 class="r-arguments-title" style="display:none;"></h3>}"
       ,r"{\def\Tabular#1#2{\Tabularr{#1}{#2}}}"
      )
     ,this$switch(latex = gsub("\n {6}", "\n", r"{
      \ExplSyntaxOn
      \cs_gset:Npn \Tabularr #1 #2
      {
        \begin{ldescription}
        \seq_set_split:Nnn \l_tmpa_seq { \\\\ } { #2 }
        \seq_map_inline:Nn \l_tmpa_seq
        {
          \tl_if_blank:nF { ##1 }
          {
            \seq_set_split:Nnn \l_tmpb_seq { & } { ##1 }
            \tl_set:Nx \l_tmpb_tl { \seq_item:Nn \l_tmpb_seq { 1 } }
            \tl_remove_once:Nn \l_tmpb_tl { ~ }
            \item[ \l_tmpb_tl ] \seq_item:Nn \l_tmpb_seq { 2 }
          }
        }
        \end{ldescription}
      }
      \ExplSyntaxOff
      }"))
    ));

    value <- topic$sections$value;
    if(is.null(value))
    {
      value <- list(type = "value", value = "");
      class(value) <- c("rd_section_value", "rd_section");
    }
    value$value <- sprintf("%s\n%s", value$value, header);
    topic$sections$value <- value;
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Creates a table of contents, listing all classes within the topic.
  #'
  #' Appends to the `\value` section (which I think is just before custom
  #' sections...)
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:insertTableOfContents <- \(topic, keys)
  {
    # describe contents, no. members and a description.
    items <- character(length(keys));
    for(i in seq_along(keys))
    {
      sections <- this$classes[keys[i]]$sections;
      nf <- if(sections$exists("Fields"))  sections["Fields"]$size  else 0L;
      nm <- if(sections$exists("Methods")) sections["Methods"]$size else 0L;
      dc <- "";
      if(sections$exists("Description"))
      {
        dc <- sub("^\\\\lbr\\{\\}", "", sections["Description"]$content);
        dc <- sprintf("\\cr\n%s", dc);
      }
      items[i] <- sprintf(
        "\n\\emph{\\verb{[%i field%s]}} \\emph{\\verb{[%i method%s]}}%s"
       ,nf, if(nf == 1) "" else "s"
       ,nm, if(nm == 1) "" else "s"
       ,dc
      );
    }

    # toc as a \describe list
    toc <- OoprRoxyDescribe("");
    toc$insert(items, keys);
    toc <- toc$toRd();
    toc <- substr(toc, 15L, nchar(toc) - 1L);

    # get the \value section and insert toc
    value <- topic$sections$value;
    value$value <- sprintf("%s\n%s", value$value, toc);
    topic$sections$value <- value;
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Add double horizontal lines at the end of oopr sections that are
  #' immediately followed by other sections.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:insertHRule <- \(topic, keys)
  {
    titles <- topic$sections$section$value$title;
    ptrn   <- r"{\\(.*?)((?=[\\ ])|(?'c'\{([^}{]|(?&c))*\})+)}";
    titles <- gsub(ptrn, perl = TRUE, "", titles);
    oopr   <- match(titles, keys, 0L) > 0L;
    if(!length(oopr)) return();
    i <- diff.default(!c(oopr, FALSE)) > 0L;
    content    <- topic$sections$section$value$content;
    content[i] <- sprintf("%s\n\\lbr\\hr\\hr", content[i]);
    topic$sections$section$value$content <- content;
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
}) ## OoprRoxy
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

