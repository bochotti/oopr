## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @useDynLib oopr, .registration = TRUE
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
isname <- \(x, names = character(0L))
{
  .Call("isname", x, names, PACKAGE = "oopr");
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
iscall <- \(x, names = character(0L))
{
  .Call("iscall", x, names, PACKAGE = "oopr");
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
  .Call("symlink", tenv, tname, env, name, PACKAGE = "oopr");
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
