## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Capture `oopr` containers with `[` or `[[`
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
evaluate_classmem <- \(i, name, rhs, env, err)
{
  isvec <- iscall(rhs, "[");
  ismap <- iscall(rhs, "[[");
  if(!(isvec || ismap) || !isname(rhs[[3L]], "")) return(rhs);
  rhs[[1L]] <- if(isvec) quote(oopr::OoprVec) else quote(oopr::OoprMap);
  rhs[[3L]] <- NULL;
  return(rhs);
}
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
    ats   <- findInExpr(body(fun), \(e)
    {
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
      oopr <- eval(call[[1L]], env$prnt, NULL);
      call <- matchsig(oopr, call);
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
    fun <- definitions_init(i, name, ats, call, envir, along, fun, env, err);
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
  contain <- classmem_get_containers(meta, this);

  refs    <- lapply(refs, `[`, match(refs$memb, classes, 0L) > 0L);
  if(name == env$name && encl[1L] == "this")
  {
    refs <- classmem_ignore_init(i, name, refs, classes, env, err);
  }
  if(!length(refs$at)) return();

  refs2 <- classmem_make_expr(refs, contain);

  for(class in classes)
  {
    refs <- lapply(refs2, `[`, match(refs2$encl, class, 0L) > 0L);
    for(ref in lapply(split.default(seq_along(refs$at), refs$slct), \(x) {
      lapply(refs, `[`, x)
    }))
    {
      oopr <- classmem_get_ooprC(class, meta, this, contain, ref$slct, env);
      if(!is.ooprC(oopr)) next;
      cmeta <- oopr@meta;
      cthis <- oopr@encl$this;
      references_method(i, name, refs, cmeta, "public", class, cthis, env, err);
    }

  }
  return();
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Collect class containers.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
classmem_get_containers <- \(meta, this)
{
  contain <- logical(meta$size)
  names(contain) <- meta$names$data
  is.containercall <- \(obj) is.call(obj) &&
    (
        identical(obj[[1L]], quote(oopr::OoprVec))
     || identical(obj[[1L]], quote(oopr::OoprMap))
    )
  for(nm in names(contain))
  {
    if(!meta$subs("class", names = nm)) next;
    obj <- this[[nm]];
    contain[[nm]] <- if(meta$subs("static", names = nm))
    {
      is.oopr(obj, c("OoprVec", "OoprMap"));
    }
    else
    {
      is.ooprC(obj, c("OoprVec", "OoprMap"));
    }
    contain[[nm]] <- contain[[nm]] || is.containercall(obj);
  }
  return(contain);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Remove a class members initialization call from the refs.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
classmem_ignore_init <- \(i, name, refs, classes, env, err)
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
  return(refs);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Create a new `expr`, comprising of class members own members.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
classmem_make_expr <- \(refs, contain)
{
  expr <- do.call(call, c('{', refs$expr), TRUE);
  nest <- refs$nest %||% rep(list(NULL), length(refs$at));
  slct <- logical(length(refs$at));
  for(j in seq_along(refs$at))
  {
    # be careful of calls within access
    at   <- refs$at[[j]];
    len  <- length(at);
    memb <- refs$memb[[j]];
    adj  <- at == 1L;
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
        adj <- if(refs$type[[j]] == "assign") 2L else 1L;
        adj <- at[seq.int(to = len, length.out = max(adj, len - 1L))];
      }
    }
    if(is.null(nest[[j]]))
    {
      nest[[j]] <- call(refs$oper[[j]], as.name(refs$encl[[j]]), as.name(memb));
    }
    # nest[[j]] <- call(refs$oper[[j]], nest[[j]], as.name(memb))
    #expr[[c(j + 1L, adj)]];
    # replace encl$memb with memb
    expr[[c(j + 1L, adj)]] <- as.name(memb);
    # remove [
    if(contain[memb])
    {
      at <- findInExpr(expr[[j + 1L]], \(e)
      {
        iscall(e, "[") && isname(e[[2L]], memb)
      });
      if(length(at))
      {
        a <- expr[[c(j + 1L, at[[1L]])]];
        a[[2L]] <- nest[[c(j)]];
        nest[[c(j)]] <- a;
        expr[[c(j + 1L, at[[1L]])]] <- as.name(memb);
        slct[[j]] <- TRUE;
      }
      else
      {
        slct[[j]] <- FALSE;
      }
    }
  }
  refs2 <- .Call(Cpp_findMemberRefs, expr);
  refs2$src  <- refs$src;
  nest <- .mapply(list(nest, .mapply(list, refs2, NULL)), NULL, FUN = \(nest, ref)
  {
    memb <- if(ref$oper == "$") as.name(ref$memb) else ref$memb;
    call(ref$oper[[j]], nest, memb);
  })
  refs2$nest <- nest;
  refs2$slct <- slct;
  return(refs2);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Get `ooprC` of a class member.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
classmem_get_ooprC <- \(class, meta, this, contain, slct, env)
{
  oopr <- this[[class]];
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
  }
  # If `[` is used, then need to obtain the container's underlying class
  if(contain[class] && slct)
  {
    if(meta$subs("static", names = class))
    {
      if(is.ooprC(oopr))
      {
        if(is.call(this[[class]]))
        {
          oopr <- get(this[[class]]$ooprC, envir = env$prnt);
        }
        else
        {
          oopr <- parent.env(this[[class]])[["this"]]$ooprC_;
        }
      }
      else if(is.oopr(oopr))
      {
        browser()
      }
      else
      {
        browser()
      }
    }
    else
    {
      parent <- parent.env(this);
      if(identical(parent, emptyenv()))
      {
        name <- env$name;
      }
      else
      {
        name <- class(parent[[".this"]])[1L];
      }
      at <- findInExpr(this[[name]], \(e)
      {
          iscall(e, c("$", "[[")) && isname(e[[2L]], "this") && isname(e[[3L]], class)
      })[[1L]];
      oopr <- body(this[[name]])[[at[-length(at)]]]$ooprC;
      oopr <- eval(oopr, environment(this[[name]]), NULL);
    }
  }
  return(oopr);
}
