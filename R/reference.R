## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' From a given `at`, get the srcref.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
find_src_ref <- \(at, expr)
{
  .Call(Cpp_find_src_ref, at, expr);
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
#' Check if vector of integers `x` is above `y` in an expression.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
at_lt <- \(x, y)
{
  lx <- l <- length(x);
  ly <- length(y);
  if(lx > ly)
  {
    y[(ly + 1L):lx] <- 0L;
  }
  else if (lx < ly)
  {
    x[(lx + 1L):ly] <- 0L;
    l <- ly;
  }
  return(sum(x * 10^((l - 1L):0)) < sum(y * 10^((l - 1L):0)));
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Check the references inside methods/properties with other members.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
references <- \(env, err)
{
  refs <- .Call(Cpp_find_member_refs, env$this);
  refs <- refs[match(names(env$this), names(refs))];
  miss <- .Call(Cpp_get_missing_vars, env$this, env$prnt);
  miss <- miss[match(names(env$this), names(miss))];
  skip <- c("this", ".this", env$inhr$meta$subs("names", TRUE, names = ""));
  meta <- env$meta;

  references_inheritance(refs, meta, env, err);

  access <- c("public", "protected", "private");
  encl   <- c("this", ".this")
  for(i in env$along) # note, inherited members not part of $along
  {
    if(!(meta$method$get(i) || nzchar(meta$property$get(i)))) next;
    name <- meta$names$get(i);
    references_method(
      i, name, refs[[name]], meta, access, encl, env$this, env, err
    );
    references_this(i, name, encl, env, err);
    for(mis in .mapply(list, miss[[name]], NULL))
    {
      if(match(mis$var, skip, 0L)) next;
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
references_method <- \(i, name, refs, meta, access, encl, this, env, err)
{
  # skip the first call of a class member in the constructor method.
  refs$skip <- logical(length(refs$at));
  if(env$name == name && is.null(refs$nest))
  {
    refs$skip <- !duplicated.default(refs$memb) & refs$type == "call";
  }
  for(ref in .mapply(list, refs, NULL))
  {
    if(!match(ref$encl, encl, 0L)) next;
    j <- which(meta$subs(names = ref$memb, access = access));
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
        else if(meta$class$get(j) && length(ref$at) <= 2L)
        {
          references_assign(i, name, j, meta, ref, "class", env, err);
        }
      }
     ,call   =
      {
        if(!(ref$skip && meta$class$get(j)))
        {
          fun <- this[[ref$memb]];
          references_call(i, name, j, meta, ref, "non-method", fun, env, err);
        }
      }
    )
    # if static, check to make sure ref is also static. If nested via
    # class member, then reference is already asserted.
    if(is.null(ref$nest) && env$meta$static$get(i))
    {
      references_static(i, name, j, meta, ref, env, err);
    }
  }
  refs$skip <- NULL;
  references_classmem(i, name, refs, meta, access, encl, this, env, err);
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
   ,msg = "Member `%s` is attempting to refer to an undefined member `%s`."
   ,name, references_expr(ref)
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
   ,name, type, references_expr(ref)
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
   ,name, type, references_expr(ref)
  );
  env$succ$set(i, FALSE);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Can only call methods, if method then call must match signature.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
references_call <- \(i, name, j, meta, ref, type = "field", fun, env, err)
{
  if(meta$method$get(j))
  {
    call <- matchsig(fun, ref$expr);
    if(is.call(call)) return(TRUE);
    err$push(
      cls = "ooprRefUnmatchedCall"
     ,src = ref$src %||% env$src[[i]]
     ,msg = "Member `%s` call to method `%s` does not match its signature:
             \"%s\"."
     ,name, references_expr(ref), call$message
    );
    env$succ$set(i, FALSE);
  }
  else
  {
    err$push(
      cls = "ooprRefBadCall"
     ,src = ref$src %||% env$src[[i]]
     ,msg = "Member `%s` is attempting to call %s `%s`."
     ,name, type, references_expr(ref)
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

  # make sure enclosures are not being over-written
  ats  <- findInExpr(expr, \(e) {
    iscall(e, c("<-", "=", "<<-")) && isname(e[[2L]], type)
  });
  for(at in ats)
  {
    err$push(
      cls = "ooprRefAssigningThis"
     ,src = find_src_ref(at, expr) %||% env$src[[i]]
     ,msg = "Member `%s` is attempting to overwrite `%s`."
     ,name, type[1L]
    );
    env$succ$set(i, FALSE);
  }
  # skip initialization call for inherited classes
  if(type[1L] != "this" && env$name == name) return();

  # make sure enclosures are not being called
  ats  <- findInExpr(expr, \(e) iscall(e, type));
  for(at in ats)
  {
    err$push(
      cls = "ooprRefCallingThis"
     ,src = find_src_ref(at, expr) %||% env$src[[i]]
     ,msg = "Member `%s` is attempting call `%s`."
     ,name, type[1L]
    );
    env$succ$set(i, FALSE);
  }
  return();
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Pull the correct expression that is causing references error.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
references_expr <- \(ref)
{
  out <- ref$nest %||% call(ref$oper, as.name(ref$encl), as.name(ref$memb));
  if(ref$type == "call")
  {
    out <- as.call(c(out, as.list(ref$expr[-1])));
  }
  return(deparse1(out));
}
