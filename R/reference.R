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
#' Check the references inside methods/properties with other members.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
references <- \(env, err)
{
  refs <- findMemberRefs(env$this);
  meta <- env$meta;

  for(i in env$along)
  {
    if(!(meta$method$get(i) || nzchar(meta$property$get(i)))) next;
    name <- meta$names$get(i);
    references_method(i, name, refs, meta, c("this", ".this"), env, err);
  }
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Check the body of each method/property
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
references_method <- \(i, name, refs, meta, type = c("this", ".this"), env, err)
{
  for(ref in .mapply(list, refs[[name]], NULL))
  {
    if(!match(ref$encl, type, 0L)) next;
    j <- which(meta$subs(names = ref$memb));
    if(!references_exist(i, name, j, meta, ref, env, err)) next;
    switch(ref$type,
      assign =
      {
        references_assign(i, name, j, meta, ref, "method", env, err);
      }
     ,call   =
      {
        references_call(i, name, j, meta, ref, env, err);
      }
    )
  }
  references_this(i, name, type, env, err);
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
#' Must not assign to methods.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
references_assign <- \(i, name, j, meta, ref, type = "method", env, err)
{
  if(!meta$method$get(j)) return(TRUE);
  err$push(
    cls = "ooprRefBadAssignment"
   ,src = ref$src %||% env$src[[i]]
   ,msg = "Member `%s` is attempting to assign to %s `%s`."
   ,name, type, ref$memb
  );
  env$succ$set(i, FALSE);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Can only call methods and not fields. Call must match signature.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
references_call <- \(i, name, j, meta, ref, env, err)
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
      cls = "ooprRefCallingNonMethod"
     ,src = ref$src %||% env$src[[i]]
     ,msg = "Member `%s` is attempting to call non-method `%s`."
     ,name, ref$memb
    );
    env$succ$set(i, FALSE);
  }
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' References to `this`.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
references_this <- \(i, name, type = c("this", ".this"), env, err)
{
  fun  <- env$this[[name]];
  expr <- body(fun);
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
