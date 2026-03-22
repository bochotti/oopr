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
    ats   <- findInExpr(body(fun), \(e) {
      is.call(e) && (
           identical(e[[1L]], call("$",  quote(this), as.name(name)))
        || identical(e[[1L]], call("[[", quote(this), as.name(name)))
      )
    });
    call  <- env$this[[name]];
    if(!is.call(call) || iscall(call, c("::", ":::")))
    {
      call <- as.call(c(call));
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
  expr <- do.call(call, c('{', refs$expr), TRUE)
  nest <- rep(list(NULL), length(refs$at));
  for(j in seq_along(refs$at))
  {
    # be careful of calls
    adj <- refs$at[[j]] == 1L;
    if(any(adj))
    {
      adj <- refs$at[[j]][which.max(adj):length(refs$at[[j]])];
    }
    else
    {
      adj <- refs$at[[j]] != 2L;
      if(any(adj))
      {
        adj <- refs$at[[j]][(which.max(adj) + 1L):length(refs$at[[j]])];
      }
      else
      {
        adj <- if(refs$type[[j]] == "assign") 3L else 2L;
        adj <- tail(refs$at[[j]], min(adj, length(refs$at[[j]]) - 1L));
      }
    }
    nest[[j]] <- expr[[c(j + 1L, adj)]];
    expr[[c(j + 1L, adj)]] <- as.name(refs$memb[[j]]);
  }
  refs2 <- findMemberRefs(expr);
  refs2$src  <- refs$src;
  refs2$nest <- refs$nest %||% refs$expr;
  for(class in classes)
  {
    refs <- lapply(refs2, `[`, match(refs2$encl, class, 0L) > 0L);
    oopr <- this[[class]];
    meta <- oopr@meta;
    this <- oopr@encl$this;
    #TODO: error messages to include prefixed this, this$mem, ..., etc.
    references_method(i, name, refs, meta, "public", class, this, env, err);
  }
  return();
}
