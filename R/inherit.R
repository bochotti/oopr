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
inheritance_get<- \(inhr, parent, err)
{
  for(i in inhr$along)
  {
    name  <- inhr$meta$names$get(i);
    pkg   <- inhr$meta$inherit$get(i);
    if(nzchar(pkg))
    {
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
    imeta <- ithis@meta;
    ithis <- ithis@encl$this;
    for(j in seq_len(imeta$size))
    {
      name <- imeta$names$get(j);
      spec <- imeta$access$get(j);

      # private members are not inherited
      if(spec == "private") next;

      # existing members are not overridden
      if(length(meta$subs("names", names = name))) next;

      # get the meta information of inherited member
      new <- as.data.frame(imeta)[j, ];
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
  inhr <- env$inhr;
  if(!inhr$size) return();
  expr <- body(fun);
  pfx  <- \(., ...) {
    out <- call("::", quote(base), as.name(.));
    if(...length())
    {
      out <- as.call(c(out, ...));
    }
    return(out)
  }
  for(i in inhr$along)
  {
    name <- inhr$meta$names$get(i);
    args <- formals(inhr$this[[name]]);
    ats  <- findInExpr(expr, \(e) iscall(e, name));
    if(!length(ats) && is.null(args))
    {
      if(length(expr) == 1L)
      {
        expr[[2L]] <- pfx("assign"
         ,x     = name
         ,value = pfx("force", call(name))
         ,envir = pfx("parent.env", pfx("environment", NULL))
        )
      }
    }
  }
  src                  <- attr(fun, "srcref", exact = TRUE);
  body(fun)            <- expr;
  attr(fun, "srcref")  <- src;
  env$this[[env$name]] <- fun;
  return();
}
