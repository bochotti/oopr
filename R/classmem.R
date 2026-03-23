## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Capture a member that is being defined as another `oopr` class.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
definitions_classmem <- \(i, name, env, err)
{
  fun   <- env$this[[name]];
  envir <- quote(this);
  # inherited members change length of the meta
  along <- env$meta$subs(class = TRUE, inherit = "");
  along <- along[seq_along(env$succ$data)] & env$succ$data;
  along <- rev(which(along));
  for(i in along)
  {
    name  <- env$meta$names$get(i);
    call  <- env$this[[name]];
    if(!is.call(call) || iscall(call, c("::", ":::")))
    {
      call <- as.call(c(call));
    }
    ats   <- findInExpr(body(fun), \(e) {
      is.call(e) && (
           identical(e[[1L]], call("$",  quote(this), as.name(name)))
        || identical(e[[1L]], call("[[", quote(this), as.name(name)))
      )
    });
    if(env$meta$static$get(i))
    {
      if(length(ats))
      {
        err$push(
          cls = "ooprStaticClassMemInitializedInConstructor"
         ,src = findSrcRef(ats[[1L]], fun) %||% env$src[[i]]
         ,"Static class member `%s` cannot be initialized in the
           constructor method, it must be initialized where defined."
         ,name
        );
        env$succ$set(i, FALSE);
      }
      fun  <- eval(call[[1L]], env$prnt, NULL);
      call <- matchsig(fun, call);
      if(!is.call(call))
      {
        err$push(
          cls = "ooprStaticClassMemSignatureUnmatched"
         ,src = env$src[[i]]
         ,"Initialization of static class member `%s` does not match its
           signature \"%s\"."
         ,name, call$message
        );
        env$succ$set(i, FALSE);
      }
      env$this[[name]] <- call;
      env$spec$set(i, list(0L));
      next;
    }
    env$this[[name]] <- eval(call[[1L]], env$prnt, NULL);
    call[[1L]] <- call("$", quote(this), as.name(name));
    fun   <- definitions_init(i, name, ats, call, envir, along, fun, env, err);
  }
  env$this[[env$name]] <- fun;
  return();
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Checks to make sure a class member is not used prior to initialization.
#' Checks usage for the class members' own members, recursively.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
references_classmem <- \(i, name, refs, meta, access, encl, this, env, err)
{
  classes <- meta$subs("names", class = TRUE, inherit = "");
  refs    <- lapply(refs, `[`, match(refs$memb, classes, 0L) > 0L);
  if(name == env$name && encl[1L] == "this")
  {
    for(j in seq_along(classes))
    {
      m   <- match(refs$memb, classes[j], 0L) > 0L;
      ref <- lapply(refs, `[`, m);
      if(!length(ref$type)) next;
      if(ref$type[[1L]] != "call")
      {
        err$push(
          cls = "ooprClassMemUsageBeforeInit"
         ,src = ref$src[[1L]]
         ,msg = "Class member `%s` is being used prior to being initialized."
         ,classes[m]
        );
        env$succ$set(i, FALSE);
        return();
      }
      refs <- lapply(refs, `[`, -which.max(m));
    }
  }
  if(!length(refs$at)) return();

  # recreate the class members own member usage
  expr <- do.call(call, c('{', refs$expr), TRUE)
  nest <- rep(list(NULL), length(refs$at));
  for(j in seq_along(refs$at))
  {
    # be careful of calls within access
    at  <- refs$at[[j]]
    len <- length(at);
    adj <- at == 1L;
    if(any(adj))
    {
      adj <- at[which.max(adj):len];
    }
    else
    {
      # 2L identifies LHS of assignment, or $ access - get the last one
      adj <- at != 2L;
      if(any(adj))
      {
        adj <- at[(which.max(adj) + 1L):len];
      }
      else
      {
        # if all assignment/access, then pull from back of expr
        adj <- if(refs$type[[j]] == "assign") 3L else 2L;
        adj <- at[seq.int(to = len, length.out = min(adj, len - 1L))];
      }
    }
    nest[[j]] <- expr[[c(j + 1L, adj)]];
    # replace encl$memb with memb
    expr[[c(j + 1L, adj)]] <- as.name(refs$memb[[j]]);
  }
  refs2 <- findMemberRefs(expr);
  refs2$src  <- refs$src;
  refs2$nest <- refs$nest %||% refs$expr;
  for(class in classes)
  {
    refs <- lapply(refs2, `[`, match(refs2$encl, class, 0L) > 0L);
    oopr <- this[[class]];
    # static members are not ooprC, but oopr
    if(meta$subs("static", names = class))
    {
      if(is.oopr(oopr))
      {
        oopr <- get(class(oopr)[1L], envir = parent.env(parent.env(oopr)));
      }
      else if(is.call(oopr))
      {
        # if this class is being constructed, it will be a call
        oopr <- eval(oopr[[1L]], env$prnt, NULL);
      }
      if(!is.ooprC(oopr)) next;
    }
    cmeta <- oopr@meta;
    cthis <- oopr@encl$this;
    references_method(i, name, refs, cmeta, "public", class, cthis, env, err);
  }
  return();
}
