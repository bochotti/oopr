## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
isname <- \(x, names = character(0L))
{
  .Call(Cpp_isname, x, names);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
iscall <- \(x, names = character(0L), package = character(0L))
{
  .Call(Cpp_iscall, x, names, package);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
is.ooprcall <- \(x)
{
  iscall(x, "oopr") || iscall(x, "oopr", "oopr");
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @exportS3Method roxygen2::roxy_tag_parse
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
roxy_tag_parse.roxy_tag_intern <- \(x) { return(x); }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
`%||%` <- \(x, y) { if(is.null(x)) return(y) else return(x); }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
symlink <- \(tenv, tname, env, name)
{
  .Call(Cpp_symlink, tenv, tname, env, name);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Match a functions signature. If no match returns error object with
#' reason why it did not match.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
matchsig <- \(fun, call)
{
  ndflt <- vapply(formals(fun), isname, logical(1L), "");
  ndflt <- names(ndflt)[ndflt];
  ndflt <- ndflt[match(ndflt, "...", 0L) == 0L];
  call  <- tryCatch(match.call(fun, call), error = identity);
  if(is.call(call))
  {
    miss <- setdiff(ndflt, names(call)[-1L]);
    if(length(miss))
    {
      plural  <- if(length(miss) > 1) "s" else "";
      miss    <- sub("^list", "", deparse1(lapply(miss, as.name)))
      message <- sprintf("missing non-default argument%s %s", plural, miss);
      call    <- simpleError(message, call = match.call());
    }
  }
  else
  {
    call$call <- match.call();
  }
  return(call)
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' To obtain the environment in a stack
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
get_in_stack <- \(call, off = 0L)
{
  call <- substitute(call);
  if(is.name(call))
  {
    call <- as.character(call);
  }
  len <- sys.nframe() - 1L;
  for(i in -seq_len(len))
  {
    if(iscall(sys.call(i), call))
    {
      i <- i + off;
      if(-i > len) return(NULL)
      return(sys.frame(i))
    }
  }
  return(NULL);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Print a tree-list
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
tree <- \(x, pfx = "")
{
  if(is.list(x))
  {
    for(i in seq_along(x))
    {
      if(i == length(x))
      {
        pre <- "\u2514\u2500";
        ind <- "  ";
      }
      else
      {
        pre <- "\u251c\u2500";
        ind <- "\u2502 ";
      }
      cat(pfx, pre, names(x)[i], "\n", sep = "");
      if(!is.list(x[[i]]) && !identical(x[[i]], quote(expr=)))
      {
        y <- rep(list(quote(expr=)), length(x[[i]]));
        names(y) <- as.character(x[[i]]);
        x[[i]]   <- y;
      }
      tree(x[[i]], paste0(pfx, ind));
    }
  }
  else if(!identical(x, quote(expr=)))
  {
    cat(pfx, x, "\n", sep = "")
  }
}
