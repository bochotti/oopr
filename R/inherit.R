## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' @include utils.R
#' @include evaluate.R
#' Inherit other classes.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
inheritance <- \(env, expr, parent, err)
{
  skip <- is.null(expr);
  if(!skip && class(expr)[1L] != '{')
  {
    expr <- call("{", expr);
  }
  inhr <- evaluate_env("inhr", expr, err);
  env$inhr <- inhr;
  if(skip) return();
  if(is.null(inhr$src) && !is.null(env$src[[1L]]))
  {
    inhr$src <- list(NULL, env$src[[1L]]);
  }
  inheritance_yank(inhr, expr, err);
  inheritance_spec(inhr, err);
  inheritance_get(inhr, parent, err);
  inheritance_set(env, inhr, err);
  return();
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Create an environment to hold inherited classes.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
inheritance_yank <- \(inhr, expr, err)
{
  for(i in seq_along(expr)[-1L])
  {
    obj <- expr[[i]];
    if(iscall(obj, ":"))
    {
      inhr$spec$set(i, list(obj[[2L]]));
      obj <- obj[[3L]];
    }
    else
    {
      inhr$spec$set(i, list(quote(private)));
    }
    if(iscall(obj, c("::", ":::")))
    {
      inhr$meta$inherit$set(i, as.character(obj[[2L]]));
      obj <- obj[[3L]];
    }
    if(!is.name(obj))
    {
      err$push(
        cls = "ooprInheritBadQuote"
       ,src = inhr$src[[i]]
       ,msg = "Inherited class `%s` must be written in the form
               `class` or `pkg::class`."
       ,deparse1(obj)
      );
      next;
    }
    inhr$meta$names$set(i, as.character(obj));
    inhr$succ$set(i, TRUE);
  }
  return();
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Specifiers must be names: private, protected or public
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
inheritance_spec <- \(inhr, err)
{
  for(i in inhr$along)
  {
    name <- inhr$meta$names$get(i);
    spec <- inhr$spec$get(i)[[1L]];
    if(!isname(spec, c("private", "protected", "public")))
    {
      err$push(
        cls = "ooprInheritBadSpecifier"
       ,src = inhr$src[[i]]
       ,"Inherited class `%s` can only have one access specifier."
       ,name
      );
      inhr$succ$set(i, FALSE);
    }
    inhr$meta$access$set(i, as.character(spec));
  }
  return();
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Make sure that the objects provided are indeed `oopr` classes.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
inheritance_get <- \(inhr, parent, err)
{
  for(i in inhr$along)
  {
    name <- inhr$meta$names$get(i);
    pkg  <- inhr$meta$inherit$get(i);
    if(nzchar(pkg))
    {
      if(!requireNamespace(pkg, quietly = TRUE))
      {
        err$push(
          cls = "ooprInheritPackageNotFound"
         ,src = inhr$src[[i]]
         ,"Inherited class `%s` package \"%s\" cannot be found."
         ,name, pkg
        );
        inhr$succ$set(i, FALSE);
        next;
      }
      envir    <- getNamespace(pkg);
      inherits <- FALSE;
    }
    else
    {
      envir    <- parent
      inherits <- TRUE;
    }
    if(!exists(name, envir = envir, inherits = inherits))
    {
      err$push(
        cls = "ooprInheritNotFound"
       ,src = inhr$src[[i]]
       ,"Inherited class `%s` cannot be found in %s."
       ,name, format.default(envir)
      );
      inhr$succ$set(i, FALSE);
      next;
    }
    obj <- get(name, envir = envir, inherits = inherits)
    if(!is.ooprC(obj, name))
    {
      err$push(
        cls = "ooprInheritNotOopr"
       ,src = inhr$src[[i]]
       ,"Inherited class `%s` is not an oopr class."
       ,name
      );
      inhr$succ$set(i, FALSE);
      next;
    }
    inhr$this[[name]] <- obj;
  }
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' It places inherited members in the derived `meta` and `this`.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
inheritance_set <- \(env, inhr, err)
{
  this <- env$this;
  meta <- env$meta;
  for(i in inhr$along)
  {
    iname <- inhr$meta$names$get(i);
    ispec <- inhr$meta$access$get(i);
    ithis <- inhr$this[[iname]];
    imeta <- as.data.frame.oopr_meta(ithis@meta);
    ithis <- ithis@encl$this;
    for(j in seq_len(nrow(imeta)))
    {
      name <- imeta[["names"]][j];
      spec <- imeta[["access"]][j];

      # private members are not inherited
      if(spec == "private") next;

      # existing members are not overridden
      if(length(meta$subs("names", names = name))) next;

      # get the meta information of inherited member
      new <- imeta[j, ];
      new[["inherit"]] <- iname;
      new[["access"]]  <- ispec;
      # protecting a class does not expose its public members
      if(spec == "protected" && ispec == "public")
      {
        new[["access"]] <- "protected";
      }

      do.call(meta$push, new)
      if(nzchar(new$property))
      {
        this[[name]] <- activeBindingFunction(name, ithis);
      }
      else
      {
        this[[name]] <- ithis[[name]];
      }
    }
  }
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Enforces initialization of inherited class/s.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
definitions_inheritance <- \(i, name, fun, env, err)
{
  inhr  <- env$inhr;
  if(!inhr$size) return();
  expr  <- body(fun);
  bsrc  <- attr(expr, "srcref", exact = TRUE);
  pfx   <- \(., ...) as.call(c(call("::", quote(base), as.name(.)), ...));
  envir <- pfx("parent.env", quote(this));
  for(i in rev(inhr$along))
  {
    name <- inhr$meta$names$get(i);
    init <- inhr$this[[name]];
    ats  <- findInExpr(expr, \(e) iscall(e, name));
    call <- call(name);
    if(!length(ats))
    {
      if(any(vapply(formals(init), isname, logical(1L), "")))
      {
        err$push(
          cls = "ooprInheritNotInit"
         ,src = attr(body(fun), "srcref", exact = TRUE)[[1L]] %||% env$src[[i]]
         ,msg = "Inherited class `%s` must be initialized in the constructor
                 method `%s` via `%s(...)`."
         ,name, env$name, name
        );
        env$succ$set(i, FALSE);
        next;
      }
      # insert into the expression
      if(length(expr) > 1L)
      {
        expr[seq_along(expr) + 1L] <- expr[seq_along(expr)];
        bsrc[seq_along(bsrc) + 1L] <- bsrc[seq_along(bsrc)];
        # adjust any prior positions
        for(j in rev(inhr$along))
        {
          if(j <= i) break;
          at <- inhr$spec$get(j);
          at[[c(1, 1)]] <- at[[c(1, 1)]] + 1L;
          inhr$spec$set(j, at);
        }
      }
      expr[[2L]] <- call;
      at <- 2L;
    }
    else if(length(ats) > 1L)
    {
      err$push(
        cls = "ooprInheritMultipleInit"
       ,src = findSrcRef(ats[[length(ats)]], fun) %||% env$src[[i]]
       ,msg = "Inherited class `%s` has been initialized multiple times in
               the constructor method `%s`."
       ,name, env$name
      );
      env$succ$set(i, FALSE);
      next;
    }
    else
    {
      at   <- ats[[1L]];
      call <- expr[[at]];
    }

    # record the position of initialization
    inhr$spec$set(i, list(at));

    call <- matchsig(init, call);
    if(!is.call(call))
    {
      err$push(
        cls = "ooprInheritSignatureNotMatched"
       ,src = findSrcRef(at, fun) %||% env$src[[i]]
       ,msg = "Initialization of inherited class `%s` in the constructor method
               `%s` does not match its signature: \"%s\"."
       ,name, env$name, call$message
      );
      env$succ$set(i, FALSE);
      next;
    }

    expr[[at]] <- pfx("assign", x = name, value = call, envir = envir);
  }
  src                  <- attr(fun, "srcref", exact = TRUE);
  attr(expr, "srcref") <- bsrc;
  body(fun)            <- expr;
  attr(fun, "srcref")  <- src;
  env$this[[env$name]] <- fun;
  return();
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Check references to inherited classes.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
references_inheritance <- \(refs, meta, env, err)
{
  inhr   <- env$inhr;
  access <- c("public", "protected");
  for(i in env$along)
  {
    if(!(meta$method$get(i) || nzchar(meta$property$get(i)))) next;
    name <- meta$names$get(i);
    for(j in inhr$along)
    {
      encl  <- inhr$meta$names$get(j);
      imeta <- inhr$this[[encl]]@meta;
      references_method(i, name, refs, imeta, access, encl, env, err);

      # check if member used before initialization
      if(name == env$name) for(ref in .mapply(list, refs[[name]], NULL))
      {
        table <- c(ref$encl, env$meta$subs("inherit", names = ref$memb));
        if(match(encl, table, 0L) && at_lt(ref$at, inhr$spec$get(j)[[1L]]))
        {
          err$push(
            cls = "ooprInheritUsageBeforeInit"
           ,src = ref$src %||% env$src[[i]]
           ,msg = "Constructor method `%s` is using an inherited member `%s`
                   prior to initializing the inherited class `%s`."
           ,name, deparse1(ref$expr), encl
          );
          env$succ$set(i, FALSE);
        }
      }
    }
  }
}
