## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Gets all references to a member via `$`/`[[`. Includes whether its access,
#' assignment or a call. Also gives the `srcref`.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
findMemberRefs <- \(x, nms = names(x))
{
  out <- .Call("findMemberRefs", x, PACKAGE = "oopr");
  if(!is.null(nms))
  {
    out <- out[match(nms, names(out))];
  }
  return(out)
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Finds variables in a function which wont be defined if evaluated
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
getMissingVars <- \(x, env = globalenv(), nms = names(x))
{
  out <- .Call("getMissingVars", x, env, PACKAGE = "oopr");
  if(!is.null(nms))
  {
    out <- out[match(nms, names(out))];
  }
  return(out)
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' From a given `at`, get the srcref.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
findSrcRef <- \(at, expr)
{
  .Call("findSrcRef", at, expr, PACKAGE = "oopr");
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Find the steps of expressions within other expressions, using a function.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
findInExpr <- \(expr, cond)
{
  if(is.function(expr))
  {
    expr <- body(expr);
  }
  out  <- list();
  walk <- \(i, e)
  {
    if(!is.language(e)) return(integer(0L));
    if(cond(e))
    {
      out[length(out) + 1L] <<- list(i);
    }
    if(is.name(e)) return(integer(0L));
    for(j in seq_along(e))
    {
      walk(c(i, j), e[[j]]);
    }
  }
  walk(integer(0L), expr);
  return(out);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' @include utils.R
#' @include property.R
#' @include static.R
#' Check the references inside methods/properties with other members.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
references <- \(env, err)
{
  refs <- findMemberRefs(env$this);
  miss <- getMissingVars(env$this);
  meta <- env$meta;

  for(i in env$along)
  {
    if(!(meta$method$get(i) || nzchar(meta$property$get(i)))) next;
    name <- meta$names$get(i);
    references_method(i, name, refs, meta, c("this", ".this"), env, err);
    for(mis in .mapply(list, miss[[name]], NULL))
    {
      if(match(mis$var, c("this", ".this"), 0L)) next;
      err$push(
        cls = "ooprRefUndefinedVariable"
       ,src = mis$src %||% env$src[[i]]
       ,msg = "Member `%s` is using an undefined variable `%s`."
       ,name, mis$var
      );
      env$succ$set(i, FALSE);
    }
  }
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Check the body of each method/property
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
references_method <- \(i, name, refs, meta, encl, env, err)
{
  for(ref in .mapply(list, refs[[name]], NULL))
  {
    if(!match(ref$encl, encl, 0L)) next;
    j <- which(meta$subs(names = ref$memb));
    if(!references_exist(i, name, j, meta, ref, env, err)) next;
    if(nzchar(meta$property$get(j)))
    {
      references_property(i, name, j, meta, ref, env, err);
    }
    else switch(ref$type,
      assign =
      {
        if(meta$method$get(j))
        {
          references_assign(i, name, j, meta, ref, "method", env, err);
        }
      }
     ,call   =
      {
        references_call(i, name, j, meta, ref, "method", env, err);
      }
    )
    if(meta$static$get(i))
    {
      references_static(i, name, j, meta, ref, env, err);
    }
  }
  references_this(i, name, encl, env, err);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Members must be defined.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
references_exist <- \(i, name, j, meta, ref, env, err)
{
  if(length(j)) return(TRUE);
  err$push(
    cls = "ooprRefNotDefined"
   ,src = ref$src %||% env$src[[i]]
   ,msg = "Member `%s` is attempting to refer to non-defined member `%s`."
   ,name, ref$memb
  );
  env$succ$set(i, FALSE);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Must not access.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
references_access <- \(i, name, j, meta, ref, type = "method", env, err)
{
  err$push(
    cls = "ooprRefBadAccess"
   ,src = ref$src %||% env$src[[i]]
   ,msg = "Member `%s` is attempting to access %s `%s`."
   ,name, type, ref$memb
  );
  env$succ$set(i, FALSE);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Must not assign.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
references_assign <- \(i, name, j, meta, ref, type = "method", env, err)
{
  err$push(
    cls = "ooprRefBadAssignment"
   ,src = ref$src %||% env$src[[i]]
   ,msg = "Member `%s` is attempting to assign into %s `%s`."
   ,name, type, ref$memb
  );
  env$succ$set(i, FALSE);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Can only call methods, if method then call must match signature.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
references_call <- \(i, name, j, meta, ref, type = "field", env, err)
{
  if(meta$method$get(j))
  {
    match <- tryCatch(
      match.call(env$this[[ref$memb]], ref$expr)
     ,error = identity
    );
    if(!inherits(match, "error")) return(TRUE);
    err$push(
      cls = "ooprRefUnmatchedCall"
     ,src = ref$src %||% env$src[[i]]
     ,msg = "Member `%s` call to method `%s` does not match its signature:
             \"%s\"."
     ,name, ref$memb, match$message
    );
    env$succ$set(i, FALSE);
  }
  else
  {
    err$push(
      cls = "ooprRefBadCall"
     ,src = ref$src %||% env$src[[i]]
     ,msg = "Member `%s` is attempting to call %s `%s`."
     ,name, type, ref$memb
    );
    env$succ$set(i, FALSE);
  }
  return();
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' References to `this` must not be an assignment or call.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
references_this <- \(i, name, type = c("this", ".this"), env, err)
{
  expr <- body(env$this[[name]]);
  ats  <- findInExpr(expr, \(e) {
    iscall(e, c("<-", "=", "<<-")) && isname(e[[2L]], type)
  });
  for(at in ats)
  {
    err$push(
      cls = "ooprRefAssigningThis"
     ,src = findSrcRef(at, expr) %||% env$src[[i]]
     ,msg = "Member `%s` is attempting assign into `%s`."
     ,name, type[1L]
    );
    env$succ$set(i, FALSE);
  }
  ats  <- findInExpr(expr, \(e) iscall(e, type));
  for(at in ats)
  {
    err$push(
      cls = "ooprRefCallingThis"
     ,src = findSrcRef(at, expr) %||% env$src[[i]]
     ,msg = "Member `%s` is attempting call `%s`."
     ,name, type[1L]
    );
    env$succ$set(i, FALSE);
  }
  return();
}
