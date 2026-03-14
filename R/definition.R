## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
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
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
definitions_constructor <- \(i, name, env, err)
{
  args <- names(formals(env$this[[name]]));
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
  return();
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
