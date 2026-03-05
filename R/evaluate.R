## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @hide
#' Evaluates the `definition` argument of `oopr` and saves the results within
#' an environment.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
evaluate <- \(name, expr, parent, err)
{
  env <- evaluate_create_env(name, expr, parent, err);
  evaluate_expr(env, expr, err);
  evaluate_lhs(env, expr, err);
  return(env);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @hide
#' Create and enclosing environment, it contains:
#' @field name  `character(1L)` \cr
#'              The class name.
#' @field err   `environment` \cr
#'              Error collection object.
#' @field this  `environment` \cr
#'              To hold members.
#' @field names `character()` \cr
#'              Vector containing the names of each member.
#' @field succ  `logical()` \cr
#'              Vector indicating if member is successful.
#' @field along `integer()` \cr
#'              The positions of each successful member.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
evaluate_create_env <- \(name, expr, parent, err)
{
  env  <- new.env(parent = topenv());
  size <- length(expr);
  env$name  <- name;
  env$names <- vector("character", size);
  env$spec  <- vector("list", size);
  env$this  <- new.env(parent = parent, size = size);
  env$succ  <- vector("logical", size);
  env$src   <- attr(expr, "srcref");

  along <- \( ) { return(which(succ$data)); }
  environment(along) <- env;
  makeActiveBinding("along", along, env);

  size <- \( ) { return(names$size); }
  environment(size) <- env;
  makeActiveBinding("size", size, env);

  return(env);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @hide
#' All expressions must be `<-` or `=`.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
evaluate_expr <- \(env, expr, err)
{
  i <- 1L;
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
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @hide
#' Grab the names and specifiers for each member, defined on the left-hand
#' side of an assignment. The name is always the on the right most side.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
evaluate_lhs <- \(env, expr, err)
{
  t <- \(i, lhs, env, err)
  {
    if(is.name(lhs))
    {
      if(skip)
      {
        env$spec$set(i, list(c(env$spec$get(i)[[1L]], as.character(lhs))));
      }
      else
      {
        env$names$set(i, as.character(lhs));
        skip <<- TRUE;
      }
    }
    else if(!skip && iscall(lhs, '~') && length(lhs) == 2L && is.name(lhs[[2L]]))
    {
      env$names$set(i, paste0('~', as.character(lhs[[2]])));
      skip <<- TRUE;
    }
    else if(iscall(lhs, ':'))
    {
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
  i <- 1L;
  for(i in env$along)
  {
    lhs <- expr[[i]][[2L]];
    skip <- FALSE;
    if(iscall(lhs, ':'))
    {
      t(i, lhs[[3L]], env, err);
      t(i, lhs[[2L]], env, err);
    }
    else
    {
      t(i, lhs, env, err);
    }
  }
}
