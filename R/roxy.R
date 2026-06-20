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
  # OoprRoxy$classes$resize();
}


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
OoprRoxy <- NULL;

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @keywords internal
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprRoxySection",,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
OoprRoxySection <- \(title = "", content = character(0L))
{
  this$title   <- title;
  this$content <- content;
}
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  title   <- character(1L);
  content <- character(0L);
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  virtual:insert <- \(x, i = length(this$content) + 1L)
  {
    this$content[i] <- x;
    return(invisible(this));
  }
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  final:erase <- \( )
  {
    this$content <- character(0L);
    return(invisible(this));
  }
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  virtual:toRd <- \( )
  {
    content <- paste(this$content, collapse = "\n\n");
    return(sprintf("\\subsection{%s}{\n%s\n}", this$title, trimws(content)));
  }
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
}) ## OoprRoxySection
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @keywords internal
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprRoxyFields", public:OoprRoxySection,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
OoprRoxyFields <- \( ) { OoprRoxySection("Fields"); }
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  insert <- \(x, i = x$val$name)
  {
    stopifnot(inherits(x, "roxy_tag_field"));
    OoprRoxySection$insert(x$val$description, i);
    return(this);
  }
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  toRd <- \( )
  {
    content <- this$content;
    on.exit(this$content <- content);

    new <- sprintf("\\item{\\code{%s}}{%s}", names(content), content);
    new <- paste0(new, collapse = "\n\n");
    this$content <- sprintf("\\describe{\n%s\n}", new);
    return(OoprRoxySection$toRd());
  }
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
}) ## OoprRoxyFields
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprRoxyUsage", public:OoprRoxySection,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
OoprRoxyUsage <- \(content = character(0L), name = "")
{
  if(is.function(content))
  {
    content <- this$makeUsageFromFun(content, name);
  }
  OoprRoxySection("Usage", content);
}
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  toRd <- \( )
  {
    content <- this$content;
    on.exit(this$content <- content);
    this$content <- paste0(
      r"{\if{html}{\out{<pre><code class="language-R">}}}"
     ,sprintf("\\preformatted{\n%s\n}", paste(content, collapse = '\n'))
     ,r"{\if{html}{\out{</code></pre>}}}"
    );
    return(OoprRoxySection$toRd());
  }
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  makeUsageFromFun <- \(fun, name)
  {
    args <- formals(fun);
    if(name != make.names(name))
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
#' @keywords internal
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprRoxyArguments", public:OoprRoxySection,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
OoprRoxyArguments <- \(x = list())
{
  OoprRoxySection("Arguments");
  for(x in x) this$insert(x);
}
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  insert <- \(x, i = x$val$name)
  {
    if(inherits(x, "roxy_tag_param"))
    {
      OoprRoxySection$insert(gsub("\\\\cr", "\\\\br", x$val$description), i);
    }
    else if(is.character(x))
    {
      OoprRoxySection$insert(x, i);
    }
    return(invisible(this));
  }
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  toRd <- \( )
  {
    content <- this$content;
    on.exit(this$content <- content);

    new <- sprintf("\\code{%s} \\tab %s \\cr", names(content), content);
    new <- sprintf("\\tabular{ll}{\n%s\n}", paste(new, collapse = "\n"));
    hdr <- OoprRoxy$switch(
      html  = r"{<h3 class="r-arguments-title" style="display:none;"></h3>}"
     ,latex = r"{\def\Tabular#1#2{\Tabularr{#1}{#2}}}"
     ,sep   = "\n"
    )
    this$content <- sprintf("%s\n%s", hdr, new);
    return(OoprRoxySection$toRd());
  }
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
}) ## OoprRoxyArguments
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @keywords internal
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprRoxyMethod", public:OoprRoxySection,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
OoprRoxyMethod <- \(title, tags, fun, warn = TRUE)
{
  this$sections(OoprRoxySection);
  OoprRoxySection(sprintf("\\hr %s", title));
  this$fun  <- fun;
  this$name <- title;
  this$warn <- warn;
  this$clss <- class(environment(fun)[[".this"]])[1L];

  this$checkMissing(tags);
  this$makeArgs(tags);

  ord <- c("description", "usage", "arguments", "details", "returns");
  u   <- \(x) { `substr<-`(x, 1L, 1L, toupper(substr(x, 1L, 1L))); }
  for(tag in tags)
  {
    if(!match(tag$tag, ord[c(1L, 4:5)], 0L)) next;
    val <- tag$val; nm <- u(tag$tag);
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
  fun  <- NULL;
  name <- character(1L);
  warn <- logical(1L);
  clss <- character(1L);
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  sections <- OoprRoxySection[[]];
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  toRd <- \( )
  {
    this$content <- unlist(this$sections$apply(\(k, v) v$toRd()));
    return(OoprRoxySection$toRd());
  }
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:roxy <- \( ) { getNamespace("roxygen2"); }
  warns    <- character(0L);
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  checkMissing <- \(tags, fun = this$fun, name = this$name)
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
      this$sections$insert("Usage", OoprRoxyUsage(fun, name));
    }
  }
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  makeArgs <- \(tags, fun = this$fun)
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
    if(length(tags)) this$sections$insert("Arguments", OoprRoxyArguments(tags));
  }
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  warning <- \(fun = this$fun)
  {
    if(!(this$warn && length(this$warns))) return();
    src  <- attr(fun, "srcref");
    line <- src[1L];
    file <- attr(src, "srcfile")$filename;
    tag  <- list(tag = "oopr", file = file, line = line);
    msg  <- c(
      sprintf("Issue/s with method \"%s$%s\":", this$clss, this$name)
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
#' @keywords internal
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprRoxyClass",,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
OoprRoxyClass <- \(block)
{
  this$block   <- block;
  this$title   <- block$object$value@name;
  this$members <- this$pullMemberTags();
  this$warn    <- this$roxy$block_has_tags(block, "export");
}
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  title    <- character(1L);
  block    <- list(tags=list(), file="", line=0L, call=NULL, object=NULL);
  members  <- list();
  warn     <- FALSE;
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:tags <- \( ) { return(this$block$tags); }
  set:tags <- \(x) { this$block$tags <- x; }
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:ooprC <- \( ) { return(this$block$object$value); }
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  sections <- OoprRoxySection[[]];
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
        this$sections[title]$insert(tag$val);
      }
    }
    this$tags <- this$tags[!match(tags, want, 0L)];
  }
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  makeFormat <- \( )
  {
    tags <- vapply(this$tags, `[[`, character(1L), "tag");
  }
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  makeFields <- \( )
  {
    fields <- OoprRoxyFields();
    names  <- this$ooprC@meta$subs("names", method = FALSE, access = "public");
    miss   <- vapply(names, logical(1L), FUN = \(name)
    {
      tags <- this$members[[name]];
      if(is.null(tags))
      {
        base <- this$ooprC@meta$subs("inherit", names = name);
        if(nzchar(base))
        {
          tags[[1L]] <- this$roxy$roxy_tag_parse(this$roxy$roxy_tag(
             tag  = "inherit"
            ,raw  = sprintf("%s$%s", base, name)
            ,file = this$block$file
            ,line = this$block$line
          ));
        }
        else
        {
          return(TRUE);
        }
      }
      tags <- this$findInheritsTag(tags, name);
      lapply(tags, \(x) if(inherits(x, "roxy_tag_field")) fields$insert(x));
      return(FALSE)
    })
    if(any(miss)) this$warning(
      "Field%s %s in class %s %s not documented"
     ,if(sum(miss) > 1L) "s"   else ""
     ,deparse1(names[miss])
     ,deparse1(this$title)
     ,if(sum(miss) > 1L) "are" else "is"
    );
    if(length(fields$content)) this$sections$insert("Fields", fields);
  }
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  makeMethods <- \( )
  {
    names <- this$ooprC@meta$subs("names", method = TRUE, access = "public");
    this$sections$emplace("Methods", "Methods");
    this$sections["Methods"]$insert("\\describe{");

    miss  <- vapply(names, logical(1L), FUN = \(name)
    {
      tags   <- this$members[[name]];
      if(is.null(tags))
      {
        base <- this$ooprC@meta$subs("inherit", names = name);
        if(nzchar(base))
        {
          tags[[1L]] <- this$roxy$roxy_tag_parse(this$roxy$roxy_tag(
             tag  = "inherit"
            ,raw  = sprintf("%s$%s", base, name)
            ,file = this$block$file
            ,line = this$block$line
          ));
        }
        else
        {
          return(TRUE);
        }
      }
      tags <- this$findInheritsTag(tags, name);

      desc <- tags[vapply(tags, `[[`, character(1L), "tag") == "description"];
      desc <- paste(vapply(desc, `[[`, character(1L), "val"), collapse = "\n");
      desc <- sprintf("\\item{\\code{%s}}{%s}", name, desc);
      this$sections["Methods"]$insert(desc);

      fun    <- this$ooprC@encl$this[[name]];
      method <- OoprRoxyMethod(name, tags, fun, this$warn);
      this$sections$insert(name, method);
      return(FALSE);
    })

    if(any(miss)) this$warning(
      "Method%s %s in class %s %s not documented"
     ,if(sum(miss) > 1L) "s"   else ""
     ,deparse1(names[miss])
     ,deparse1(this$title)
     ,if(sum(miss) > 1L) "are" else "is"
    );

    this$sections["Methods"]$insert("}");
  }
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  toRd <- \( )
  {
    content <- this$sections$apply(\(k, v) v$toRd());
    content <- paste(content, collapse = "\n\n");
    this$tags[[length(this$tags) + 1]] <- this$roxy$roxy_tag_parse(
      this$roxy$roxy_tag("section", paste0(
        "\\hr\\hr ", this$title, ":\n", content
      ))
    );
  }
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:roxy <- \( ) { return(getNamespace("roxygen2")); }
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
      name <- deparse1(name);
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
  warning <- \(fmt, ..., file = this$block$file, line = this$block$line)
  {
    if(!this$warn) return();
    tag <- list(tag = "oopr", file = file, line = line);
    this$roxy$warn_roxy_tag(tag, sprintf(fmt, ...));
  }
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
        err <- "@inherit tag should be in the form x$y";
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
      if(nzchar(err))
      {
        this$warning(err, file = tag$file, line = tag$line);
        next;
      }

      oth <- OoprRoxy$classes[src[1L]]$members[[src[2L]]];
      oth <- lapply(oth, `[[<-`, "INHR_", TRUE);
      tags <- c(tags, oth);
      this$members[[name]] <- c(this$members[[name]], oth);
    }

    return(tags);
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
    obj$toRd();
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
      for(t in v$tags) if(match(t$tag, c("name", "rdname"), 0L)) return(t$val);
      return(character(0L))
    });
    out <- unique(unlist(topics));
    return(sprintf("%s%s", out, sfx));
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
    for(name in this$getTopicNames()) this$insertHeaderSection(topics[[name]]);
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
  static:insertHeaderSection <- \(topic)
  {
    value <- topic$sections$section$value;
    if(is.null(value)) return();
    newcmd <- \(nm, html = "", latex = "", text = "")
    {
      sprintf("\\newcommand{\\%s}{%s}", nm, this$switch(html, latex, text));
    }
    header <- c(
      newcmd("br", html = "<br>", latex = "\\newline")
     ,newcmd("hr", html = "<hr>", latex = "\\hrule")
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
    );
    topic$sections$description$value <- sprintf(
      "%s\n%s", paste(header, collapse = "\n"), topic$sections$description$value
    );
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
}) ## OoprRoxy
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

