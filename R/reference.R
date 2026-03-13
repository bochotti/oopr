## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Gets all references to a member via `$`/`[[`. Includes whether its access,
#' assignment or a call. Also gives the `srcref`.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
findMemberRefs <- \(x, nms = names(x))
{
  out <- .Call("findMemberRefs", x, PACKAGE = "oopr");
  if(!is.null(nms))
  {
    out <- out[match(names(out), nms)];
  }
  return(out)
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' From a given `at`, get the srcref.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
findSrcRef <- \(at, expr)
{
  .Call("findSrcRef", at, expr, PACKAGE = "oopr");
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' General version of `findMemberRefs`, using a function to find steps.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
findInExpr <- \(expr, cond)
{
  if(is.function(expr))
  {
    expr <- body(expr);
  }
  out  <- list();
  walk <- \(i, e)
  {
    if(!is.language(e)) return(integer(0L));
    if(cond(e))
    {
      out[length(out) + 1L] <<- list(i);
    }
    if(is.name(e)) return(integer(0L));
    for(j in seq_along(e))
    {
      walk(c(i, j), e[[j]]);
    }
  }
  walk(integer(0L), expr);
  return(out);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Check the references inside methods/properties with other members.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
references <- \(env, err)
{

}


# findInExpr(body(specifiers), \(e) isname(e, "spec"))
