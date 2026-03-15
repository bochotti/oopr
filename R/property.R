## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Check if duplicate name is due to property get/set.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
evaluate_property <- \(i, name, j, env)
{
  type <- \(x)
  {
    x <- env$spec$get(x)[[1L]];
    m <- match(x, c("get", "set"), 0L) > 0L;
    if(!any(m)) return(NA_character_);
    return(x[m][1L]);
  }
  ti <- type(i);
  tj <- type(j);
  if(is.na(ti) || is.na(tj) || ti == tj) return(FALSE);
  env$meta$names$set(i, sprintf(".%s", name));
  return(TRUE);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' @include utils.R
#' Collect specifiers for properties.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
specifiers_property <- \(i, name, spec, meta, env, err)
{
  use <- c("get", "set");
  set <- spec$get(i)[[1L]];
  has <- match(set, use, 0L) > 0L;
  if(sum(has) > 1L)
  {
    err$push(
      cls = "ooprMultiplePropertySpecifiers"
     ,src = env$src[[i]]
     ,msg = "Member `%s` cannot be specified as both a \"get\" and \"set\"
             property."
     ,name
    );
    env$succ$set(i, FALSE);
  }
  else if(sum(has) == 1L)
  {
    meta$property$set(i, set[has]);
    meta$method$set(i, FALSE);
    spec$set(i, list(set[!has]));
  }
  return(env$succ$get(i));
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Search the meta object for properties and their types. If there is both
#' a get & set, then the meta object needs to change.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
property <- \(env, meta, err)
{
  properties <- nzchar(meta$property$data) & !nzchar(meta$inherit$data);
  jj <- integer(0L);
  for(i in which(properties))
  {
    if(match(i, jj, 0L)) next;
    name <- meta$names$get(i);
    type <- meta$property$get(i);

    # check if there are both get & set for the same name
    j <- which(meta$names$subs(sprintf(".%s", name)));
    if(length(j))
    {
      jj[length(jj) + 1L] <- j;
      property_both(i, name, type, j, meta, env, err);
      meta$property$set(i, "both");
    }
    else
    {
      fun <- env$this[[name]];
      do  <- if(type == "get") property_get else property_set;
      if(!do(i, name, fun, env, err)) next;
      args <- list(name = name, env = env);
      args[[type]] <- fun;
      do.call(property_create, args);
    }
  }
  if(length(jj))
  {
    meta$rmve(jj);
    env$succ$rmve(jj);
    env$src <- env$src[-jj];
  }
  return();
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Get properties cannot have an argument.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
property_get <- \(i, name, fun, env, err)
{
  if(!(is.function(fun) && is.null(formals(fun))))
  {
    err$push(
      cls = "ooprGetPropertyHasArgs"
     ,src = env$src[[i]]
     ,msg = "Get property `%s` must be a function with no arguments."
     ,name
    );
    env$succ$set(i, FALSE);
    return(FALSE);
  }
  return(TRUE);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Set properties require only one argument.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
property_set <- \(i, name, fun, env, err)
{
  if(!(is.function(fun) && length(formals(fun)) == 1L && isname(formals(fun)[[1L]], "")))
  {
    err$push(
      cls = "ooprSetPropertyNotOneArg"
     ,src = env$src[[i]]
     ,msg = "Set property `%s` must be a function with one argument with no
             default value."
     ,name
    );
    env$succ$set(i, FALSE);
    return(FALSE);
  }
  return(TRUE);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Get + set properties must be together.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
property_both <- \(i, name, type, j, meta, env, err)
{
  if(!(type == "get" && j - i == 1L))
  {
    err$push(
      cls = "ooprBothPropertyNotOrdered"
     ,src = if(type == "get") env$src[[j]] else env$src[[i]]
     ,msg = "Property `%s` must have its set defined immediately after its
             get definition."
     ,name
    );
    env$succ$set(c(i, j), FALSE);
    return(FALSE);
  }
  if(meta$access$get(i) != meta$access$get(j))
  {
    err$push(
      cls = "ooprBothPropertyNotSameAccess"
     ,src = env$src[[j]]
     ,msg = "Property `%s` get and set must have the same access specifier."
     ,name
    );
    env$succ$set(c(i, j), FALSE);
    return(FALSE);
  }
  if(meta$static$get(i) != meta$static$get(j))
  {
    err$push(
      cls = "ooprBothPropertyNotSameStatic"
     ,src = env$src[[j]]
     ,msg = "Property `%s` get and set must both be static/non-static."
     ,name
    );
    env$succ$set(c(i, j), FALSE);
    return(FALSE);
  }

  get <- env$this[[name]];
  if(!property_get(i, name, get, env, err)) return(FALSE);
  set <- env$this[[sprintf(".%s", name)]];
  if(!property_set(j, name, set, env, err)) return(FALSE);

  # merge the srcrefs
  src <- attr(get, "srcref", exact = TRUE);
  if(!is.null(src))
  {
    i <- 3:4;
    if(length(src) >= 6L) i <- c(i, 6L);
    if(length(src) == 8L) i <- c(i, 8L);
    src[i] <- attr(set, "srcref", exact = TRUE)[i];
    attr(get, "srcref") <- src;
  }

  property_create(name, get, set, env);
  rm(list = sprintf(".%s", name), envir = env$this);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Create the function for the property.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
property_create <- \(name, get, set, env)
{
  if(missing(set))
  {
    arg <- as.pairlist(alist(x=));
    set <- call("stop", sprintf("`%s` is read-only", name), call. = FALSE);
  }
  else
  {
    arg <- formals(set);
    src <- attr(set, "srcref", exact = TRUE);
    cls <- environment(set);
    set <- body(set);
  }

  if(missing(get))
  {
    # TODO: this will cause issues with lazy loading
    get <- call("stop", call("errorCondition"
      ,sprintf("`%s` is write-only", name)
      ,class = "ooprPropertyWriteOnly"
      ,call  = NULL
    ));
  }
  else
  {
    src <- attr(get, "srcref", exact = TRUE);
    cls <- environment(get);
    get <- body(get);
  }

  body <- call("if", call("missing", as.name(names(arg))), get, set);
  fun  <- eval(call("function", arg, call('{', body), src), cls);
  env$this[[name]] <- fun;
  return();
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Ensure properties are referred to properly.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
references_property <- \(i, name, j, meta, ref, env, err)
{
  switch(meta$property$get(j),
    get =
    {
      if(ref$type == "assign")
      {
        references_assign(i, name, j, meta, ref, "get property", env, err);
      }
    }
   ,set =
    {
      if(ref$type != "assign")
      {
        references_access(i, name, j, meta, ref, "set property", env, err);
      }
    }
  )
  if(ref$type == "call")
  {
    references_call(i, name, j, meta, ref, "property", env, err);
  }
}
