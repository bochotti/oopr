## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' @include utils.R
#' @include property.R
#' Enforces definitions of members.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
definitions <- \(env, err)
{
  meta <- env$meta;
  property(env, meta, err);
  for(i in env$along)
  {
    name <- meta$names$get(i);
    definitions_special(i, name, meta, env, err);
    if(meta$method$get(i) || nzchar(meta$property$get(i)))
    {
      definitions_args(i, name, env, err);
      definitions_return(i, name, env, err);
    }
  }
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Handles special methods: constructor, destructor and print.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
definitions_special <- \(i, name, meta, env, err)
{
  use <- c("print", sprintf("%s%s", c("", "~"), env$name));
  has <- match(name, use, 0L);
  if(has == 0L) return(TRUE);

  if(!env$meta$method$get(i))
  {
    err$push(
      cls = "ooprSpecialNotAMethod"
     ,src = env$src[[i]]
     ,msg = "Member `%s` must be a method."
     ,name
    );
    env$succ$set(i, FALSE);
    return(FALSE);
  }

  if(name == "print")
  {
    definitions_print(i, name, meta, env, err);
  }
  else
  {
    if(meta$access$get(i) != "private")
    {
      err$push(
        cls = "ooprSpecialNotPrivate"
       ,src = env$src[[i]]
       ,msg = "Method `%s` must be private."
       ,name
      );
      env$succ$set(i, FALSE);
    }
    if(name == env$name)
    {
      definitions_constructor(i, name, env, err);
    }
    else
    {
      definitions_destructor(i, name, env, err);
    }
  }

  return(env$succ$get(i));
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Print method must be public.
#' All argument must have a default, to prevent errors with `print(x, ...)`
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
definitions_print <- \(i, name, meta, env, err)
{
  if(meta$access$get(i) != "public")
  {
    err$push(
      cls = "ooprPrintNotPublic"
     ,src = env$src[[i]]
     ,msg = "Method `%s` must be public."
     ,name
    );
    env$succ$set(i, FALSE);
  }
  ndflt <- vapply(formals(env$this[[name]]), isname, logical(1L), "");
  ndflt <- ndflt[match(names(ndflt), "...", 0L) == 0L];
  if(any(ndflt))
  {
    err$push(
      cls = "ooprPrintNonDefaultArgs"
     ,src = env$src[[i]]
     ,msg = "Method `%s` has arguments without a default value: %s."
     ,name, names(ndflt)[ndflt]
    );
    env$succ$set(i, FALSE);
  }
  return();
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Constructor method cannot have some argument names.
#' `.this` is not available during construction.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
definitions_constructor <- \(i, name, env, err)
{
  fun  <- env$this[[name]];
  args <- names(formals(fun));
  if(!is.null(args) && any(match(args, c(".", ".."), 0L)))
  {
    err$push(
      cls = "ooprConstructorBadArgNames"
     ,src = env$src[[i]]
     ,msg = "Constructor method `%s` cannot have arguments \".\" or \"..\"."
     ,name
    );
    env$succ$set(i, FALSE);
  }
  ats <- findInExpr(fun, \(e) isname(e, ".this"));
  if(length(ats))
  {
    err$push(
      cls = "ooprConstructorRefersToDotThis"
     ,src = findSrcRef(ats[[length(ats)]], fun) %||% env$src[[i]]
     ,msg = "Constructor method `%s` cannot refer to `.this`."
     ,name
    );
    env$succ$set(i, FALSE);
  }
  definitions_classmem(i, name, env, err);
  definitions_inheritance(i, name, env, err);
  return();
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Make amendments for the initialization of inherited classes and
#' class members.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
definitions_init <- \(i, name, ats, call, envir, along, fun, env, err)
{
  expr  <- body(fun);
  bsrc  <- attr(expr, "srcref", exact = TRUE);
  init  <- env$this[[name]];
  if(!length(ats))
  {
    if(any(vapply(formals(init), isname, logical(1L), ""))&&length(call) == 1L)
    {
      err$push(
        cls = "ooprDefNoInit"
       ,src = attr(body(fun), "srcref", exact = TRUE)[[1L]] %||% env$src[[i]]
       ,msg = "Class `%s` must be initialized in the constructor
               method via `%s(...)`."
       ,name, deparse1(call[[1L]])
      );
      env$succ$set(i, FALSE);
      return(fun);
    }
    # insert into the expression
    if(length(expr) > 1L)
    {
      expr[seq_along(expr) + 1L] <- expr[seq_along(expr)];
      bsrc[seq_along(bsrc) + 1L] <- bsrc[seq_along(bsrc)];
      # adjust any prior positions
      for(j in rev(along))
      {
        if(j <= i) break;
        at <- env$spec$get(j);
        at[[c(1, 1)]] <- at[[c(1, 1)]] + 1L;
        env$spec$set(j, at);
      }
    }
    expr[[2L]] <- call;
    at <- 2L;
  }
  else if(length(ats) > 1L)
  {
    err$push(
      cls = "ooprDefMultipleInit"
     ,src = findSrcRef(ats[[length(ats)]], fun) %||% env$src[[i]]
     ,msg = "Class `%s` has been initialized multiple times in
             the constructor method."
     ,deparse1(call[[1L]])
    );
    env$succ$set(i, FALSE);
    return(fun);
  }
  else
  {
    at   <- ats[[1L]];
    call <- expr[[at]];
  }

  # record the position of initialization
  env$spec$set(i, list(at));

  call <- matchsig(init, call);
  if(!is.call(call))
  {
    err$push(
      cls = "ooprDefInitSignatureNotMatched"
     ,src = findSrcRef(at, fun) %||% env$src[[i]]
     ,msg = "Initialization of class `%s` in the constructor method
             does not match its signature: \"%s\"."
     ,name, call$message
    );
    env$succ$set(i, FALSE);
    return(fun);
  }

  assign     <- call("::", quote(base), quote(assign));
  expr[[at]] <- as.call(c(assign, x = name, value = call, envir = envir));

  src                  <- attr(fun, "srcref", exact = TRUE);
  attr(expr, "srcref") <- bsrc;
  body(fun)            <- expr;
  attr(fun, "srcref")  <- src;
  return(fun);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Destructor method cannot have any arguments.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
definitions_destructor <- \(i, name, env, err)
{
  fun <- env$this[[name]];
  if(!is.null(formals(fun)))
  {
    err$push(
      cls = "ooprDestructorHasArgs"
     ,src = env$src[[i]]
     ,msg = "Destructor method `%s` cannot have any arguments."
     ,name
    );
    env$succ$set(i, FALSE);
  }
  src <- attr(fun, "srcref", exact = TRUE);
  formals(fun) <- alist(this=);
  attr(fun, "srcref") <- src;
  env$this[[name]] <- fun;
  return();
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Replace `return` of `this` to `.this`.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
definitions_return <- \(i, name, env, err)
{
  fun  <- env$this[[name]];
  src  <- attr(fun, "srcref", exact = TRUE);
  expr <- body(fun);

  nbraced <- !iscall(expr, '{');
  if(nbraced)
  {
    expr <- call('{', expr);
  }

  ats <- findInExpr(expr, \(e) {
    iscall(e, "return") && (
         isname(e[[2L]], "this")
      || iscall(e[[2L]], "invisible") && isname(e[[c(2L, 2L)]], "this")
    )
  });
  if(!length(ats)) return();

  sub <- list(this = quote(.this));
  for(at in ats)
  {
    expr[[at]] <- do.call(substitute, list(expr[[at]], sub));
  }

  if(nbraced)
  {
    expr <- expr[[2L]];
  }

  body(fun)           <- expr;
  attr(fun, "srcref") <- src;
  env$this[[name]]    <- fun;
  return();
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Ensure arguments do not overwrite any enclosure.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
definitions_args <- \(i, name, env, err)
{
  if(startsWith(name, '~')) return();
  args <- names(formals(env$this[[name]]));
  bad <- c("this", ".this", env$inhr$meta$names$data);
  m   <- match(bad, args, 0L) > 0L;
  if(any(m))
  {
    err$push(
      cls = "ooprDefinitionBadArgs"
     ,src = env$src[[i]]
     ,"Member `%s` cannot have %s as %s."
     ,name, deparse1(bad[m]), if(sum(m) == 1L) "an argument" else "arguments"
    );
    env$succ$set(i, FALSE);
  }
}
