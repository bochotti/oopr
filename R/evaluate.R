## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' @include utils.R
#' @include meta.R
#' Evaluates the `definition` argument of `oopr` and saves the results within
#' an environment.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
evaluate <- \(name, expr, parent, err)
{
  env <- evaluate_env(name, expr, parent, err);
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
#' @field wsrc  `srcref` \cr
#'              wholeSrcref of `expr`.
#' @field along `integer()` \cr
#'              The positions of each successful member - allows for skipping
#'              over them for future operations.
#' @field size  `integer(1L)` \cr
#'              The amount of members.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
evaluate_env <- \(name, expr, parent, err)
{
  env  <- new.env(parent = baseenv(), size = 10L);
  size <- length(expr);
  env$name  <- name;
  env$meta  <- meta(size);
  env$spec  <- vector("list", size);
  env$this  <- new.env(parent = emptyenv(), size = size);
  env$succ  <- vector("logical", size);
  env$src   <- attr(expr, "srcref", exact = TRUE);
  env$wsrc  <- NULL;
  env$prnt  <- parent;

  along <- \( ) { }
  body(along) <- quote({ return(which(succ$data)) })
  environment(along) <- env;
  makeActiveBinding("along", along, env);

  size <- \( ) { }
  body(size) <- quote({ return(succ$size); })
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
  walk <- \(i, lhs, env, err)
  {
    if(is.name(lhs))
    {
      # symbols will be assigned
      if(named)
      {
        # as a specifier
        specs[length(specs) + 1L] <<- as.character(lhs);
      }
      else
      {
        # or the name if not yet defined
        env$meta$names$set(i, as.character(lhs));
        named <<- TRUE;
      }
    }
    else if(!named && iscall(lhs, '~') && length(lhs)==2L && is.name(lhs[[2L]]))
    {
      # a name can be prefixed with `~`, for destructor method
      env$meta$names$set(i, sprintf("~%s", as.character(lhs[[2L]])));
      named <<- TRUE;
    }
    else if(iscall(lhs, ':'))
    {
      # rhs of `:` is saved as a specifier, lhs recurses
      specs[length(specs) + 1L] <<- as.character(lhs[[3L]]);
      walk(i, lhs[[2L]], env, err);
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
    named <- FALSE;
    specs <- character(0L);

    # collect names and specifiers
    lhs  <- expr[[i]][[2L]];
    if(iscall(lhs, ':'))
    {
      # do rhs of `:` first, to collect the name of member
      walk(i, lhs[[3L]], env, err);
      walk(i, lhs[[2L]], env, err);
    }
    else
    {
      # when no specifiers are used
      walk(i, lhs, env, err);
    }
    env$spec$set(i, list(specs));
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
    dupe <- match(name, env$meta$names$get(seq_len(i - 1L)), 0L);
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
    obj  <- tryCatch(eval(rhs, eenv, NULL), error = \(e)
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
    if(is.ooprC(obj) || is.oopr(obj))
    {
      obj <- rhs;
      env$meta$class$set(i, TRUE);
    }
    else if(is.function(obj))
    {
      env$meta$method$set(i, TRUE);
    }
    env$this[[name]] <- obj;
  }

  # if a constructor method is not defined, create an empty one
  name <- env$name;
  if(match(name, env$meta$names$data, 0L) == 0L)
  {
    obj <- parse(text = "\\() {}", keep.source = FALSE);
    obj <- eval(obj, globalenv(), NULL);
    environment(obj) <- eenv;
    env$this[[name]] <- obj;
    env$meta$push(names = name, method = TRUE);
    env$spec$push(list("private"));
    env$succ$push(TRUE);
    env$src[[length(env$src) + 1L]] <- env$src[[1L]]
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

  wsrc <- attr(expr, "wholeSrcref", exact = TRUE);
  if(!is.null(wsrc))
  {
    # wholeSrcref starts at the beginning of the file - so restrict it to `{`
    i <- 1:2;
    if(length(wsrc) >= 6L) i <- c(i, 5L);
    if(length(wsrc) == 8L) i <- c(i, 7L);
    wsrc[i] <- env$src[[1L]][i];
  }
  env$wsrc <- wsrc;
  return();
}
