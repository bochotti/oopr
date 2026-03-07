## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Evaluates the `definition` argument of `oopr` and saves the results within
#' an environment.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
evaluate <- \(name, expr, parent, err)
{
  env <- evaluate_env(name, expr, err);
  evaluate_expr(env, expr, err);
  evaluate_lhs(env, expr, err);
  evaluate_nme(env, err);
  evaluate_rhs(env, expr, parent, err);
  evaluate_src(env, expr, err);
  return(env);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Create and enclosing environment, it contains:
#' @field name  `character(1L)` \cr
#'              The class name.
#' @field names `vector(character)` \cr
#'              Vector containing the names of each member.
#' @field spec  `vector(list)` \cr
#'              Vector containing specifiers for each member.
#' @field this  `environment` \cr
#'              To hold members.
#' @field succ  `vector(logical)` \cr
#'              Vector indicating if member is successful.
#' @field src   `list()` \cr
#'              Srcref of `expr`.
#' @field along `integer()` \cr
#'              The positions of each successful member - allows for skipping
#'              over them for future operations.
#' @field size  `integer(1L)` \cr
#'              The amount of members.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
evaluate_env <- \(name, expr, err)
{
  env  <- new.env(parent = baseenv());
  size <- length(expr);
  env$name  <- name;
  env$meta  <- meta(size);
  env$spec  <- vector("list", size);
  env$this  <- new.env(parent = emptyenv(), size = size);
  env$succ  <- vector("logical", size);
  env$src   <- attr(expr, "srcref", exact = TRUE);

  along <- \( ) { return(which(succ$data)); }
  environment(along) <- env;
  makeActiveBinding("along", along, env);

  size <- \( ) { return(succ$size); }
  environment(size) <- env;
  makeActiveBinding("size", size, env);

  return(env);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' All expressions must be `<-` or `=`.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
evaluate_expr <- \(env, expr, err)
{
  for(i in seq_len(env$size)[-1L]) # first will always be {
  {
    if(iscall(expr[[i]], c("<-", "=")))
    {
      env$succ$set(i, TRUE);
    }
    else
    {
      err$push(
        cls = "ooprNotATopLevelAssignment"
       ,src = env$src[[i]]
       ,msg = "Top-level expressions in `definition` must be assignments,
               either using `<-` or `=`."
      );
    }
  }
  return();
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Grab the names and specifiers for each member, defined on the left-hand
#' side of an assignment. The name is always the on the right most side.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
evaluate_lhs <- \(env, expr, err)
{
  # pull names out of `:` calls
  t <- \(i, lhs, env, err)
  {
    if(is.name(lhs))
    {
      # pure names will be assigned
      if(skip)
      {
        # as a specifier
        env$spec$set(i, list(c(env$spec$get(i)[[1L]], as.character(lhs))));
      }
      else
      {
        # or a name if not yet defined
        env$meta$names$set(i, as.character(lhs));
        skip <<- TRUE;
      }
    }
    else if(!skip && iscall(lhs, '~') && length(lhs) == 2L && is.name(lhs[[2L]]))
    {
      # a name can be prefixed with `~`, for destructor method
      env$meta$names$set(i, sprintf("~%s", as.character(lhs[[2L]])));
      skip <<- TRUE;
    }
    else if(iscall(lhs, ':'))
    {
      # rhs of `:` are saved a specifier, lhs recurses
      env$spec$set(i, list(c(env$spec$get(i)[[1L]], as.character(lhs[[3L]]))));
      t(i, lhs[[2L]], env, err);
    }
    else
    {
      err$push(
        cls = "ooprLHSInvalidCall"
       ,src = env$src[[i]]
       ,msg = "Left-hand side of assignments can only contain `:` calls."
      );
      env$succ$set(i, FALSE);
    }
  }

  for(i in env$along)
  {
    # collect names and specifiers
    lhs  <- expr[[i]][[2L]];
    skip <- FALSE;
    if(iscall(lhs, ':'))
    {
      # do rhs of `:` first, to collect the name of member
      t(i, lhs[[3L]], env, err);
      t(i, lhs[[2L]], env, err);
    }
    else
    {
      # when no specifiers are used
      t(i, lhs, env, err);
    }
  }
  return();
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Check names of the members
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
evaluate_nme <- \(env, err)
{
  for(i in env$along)
  {
    name <- env$meta$names$get(i);

    # check for hidden names
    if(startsWith(name, '.'))
    {
      err$push(
        cls = "ooprHiddenMember"
       ,src = env$src[[i]]
       ,msg = "Member `%s` cannot start with \".\"."
       ,name
      );
      env$succ$set(i, FALSE);
    }

    # check for dupes
    dupe <- match(name, env$meta$names$data[seq_len(i - 1L)], 0L);
    if(dupe != 0)
    {
      # exception for properties
      if(evaluate_property(i, name, dupe, env)) next;

      err$push(
        cls = "ooprDuplicateMember"
       ,src = env$src[[i]]
       ,msg = "Member `%s` has multiple definitions."
       ,name
      );
      env$succ$set(i, FALSE);
    }
  }
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Evaluates the rhs of `<-`, which gets assigned into `this`.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
evaluate_rhs <- \(env, expr, parent, err)
{
  eenv <- new.env(parent = parent);
  for(i in env$along)
  {
    name <- env$meta$names$get(i);
    rhs  <- expr[[c(i, 3L)]];
    #TODO: behaviour for oopr members
    obj  <- tryCatch(eval(rhs, eenv), error = \(e)
    {
      err$push(
        cls = "ooprRHSError"
       ,src = env$src[[i]]
       ,msg = "An error occured while evaluating member `%s`: \"%s\"."
       ,name, e$message
      );
      env$succ$set(i, FALSE);
      return(NULL);
    })
    if(is.function(obj))
    {
      env$meta$method$set(i, TRUE);
    }
    env$this[[name]] <- obj;
  }

  # ensure there is a constructor method - should move this after evaluate_src
  if(match(env$name, env$meta$names$data, 0L) == 0L)
  {
    obj <- \( ) { };
    environment(obj)     <- eenv;
    attr(obj, "srcref")  <- env$src[[1L]];
    env$this[[env$name]] <- obj;
    env$meta$push(names = env$name, access = "private", method = TRUE)
  }
  return();
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Collects srcref
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
evaluate_src <- \(env, expr, err)
{
  for(i in env$along)
  {
    name <- env$meta$names$get(i);
    if(env$meta$method$get(i))
    {
      attr(env$this[[name]], "srcref") <- env$src[[i]];
    }
  }
  #env$src <- attr(expr, "wholeSrcref", exact = TRUE);
  return();
}
