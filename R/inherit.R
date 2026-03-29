## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
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
#' Virtual inheritance skips over private, because the resulting class
#' will end up with `public` class names in its S3 list.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
inheritance_set <- \(env, inhr, err)
{
  this <- env$this;
  meta <- env$meta;
  vtl  <- logical(meta$size);
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

      # existing members are enforced if virtual, and not overridden
      k <- which(meta$subs(names = name));
      if(length(k))
      {
        if(!imeta$virtual[j] || meta$access$get(k) == "private" || vtl[k]) next;
        vtl[k] <- TRUE;
        inheritance_virtual(k, name, iname, meta, imeta, this, ithis, env, err);
        next;
      }

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
#' Called from `definitions_constructor`.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
definitions_inheritance <- \(i, name, env, err)
{
  fun   <- env$this[[name]];
  inhr  <- env$inhr;
  envir <- as.call(c(call("::", quote(base), quote(parent.env)), quote(this)));
  along <- rev(inhr$along);
  for(i in along)
  {
    name <- inhr$meta$names$get(i);
    ats  <- findInExpr(body(fun), \(e) iscall(e, name));
    call <- call(name);
    fun  <- definitions_init(i, name, ats, call, envir, along, fun, inhr, err);
  }
  env$this[[env$name]] <- fun;
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
      this  <- inhr$this[[encl]]@encl$this;
      references_method(
        i, name, refs[[name]], imeta, access, encl, this, env, err
      );

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

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Collect a virtual specifier.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
specifiers_virtual <- \(i, name, spec, meta, env, err)
{
  set <- spec$get(i)[[1L]];
  has <- match(set, "virtual", 0L) > 0L;
  if(sum(has) > 0L)
  {
    meta$virtual$set(i, TRUE);
    spec$set(i, list(set[!has]));
  }
  return(TRUE);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Virtual members must be methods
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
definitions_virtual  <- \(i, name, meta, env, err)
{
  if(!meta$virtual$get(i)) return();
  if(!meta$method$get(i))
  {
    err$push(
      cls = "ooprVirtualNotMethod"
     ,src = env$src[[i]]
     ,msg = "Virtual member `%s` must be a method."
     ,name
    );
    env$succ$set(i, FALSE);
  }
  return(env$succ$get(i));
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Inherited virtual members must match arguments.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
inheritance_virtual <- \(i, name, iname, meta, imeta, this, ithis, env, err)
{
  meta$virtual$set(i, TRUE);
  if(!meta$method$get(i)) return();

  iargs <- formals(ithis[[name]]);
  args  <- formals(this[[name]]);

  pass <- TRUE;
  msg  <- character(1L);
  if(length(iargs) != length(args))
  {
    pass <- FALSE;
    msg  <- "Not the same amount of arguments";
  }
  if(pass && !all(names(iargs) == names(args)))
  {
    pass <- FALSE;
    msg  <- "Argument names do not match";
  }
  idflt <- !vapply(iargs, isname, logical(1L), "");
  dflt  <- !vapply(args,  isname, logical(1L), "");
  if(pass && !all(idflt & dflt))
  {
    pass <- FALSE;
    msg  <- sprintf(
      "Argument%s %s must have %s"
     ,if(sum(!(idflt & !dflt)) > 1L) "s" else ""
     ,deparse1(names(dflt)[idflt & !dflt])
     ,if(sum(!(idflt & !dflt)) > 1L) "default values" else "a default value"
    );
  }

  if(!pass)
  {
    err$push(
      cls = "ooprVirtualSignatureNotMatched"
     ,src = env$src[[i]]
     ,msg = "Method `%s` signature %s does not match inherited class `%s`
             virtual method signature %s: \"%s\"."
     ,name,  sub("^list", "", deparse1(as.list(args)))
     ,iname, sub("^list", "", deparse1(as.list(iargs)))
     ,msg
    );
    env$succ$set(i, FALSE);
  }
  return();
}
