## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Static members always refer to the constructor object's enclosure.
#' Each instance shares the same state.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
specifiers_static <- \(i, name, spec, meta, env, err)
{
  set <- spec$get(i)[[1L]];
  has <- match(set, "static", 0L) > 0L;
  if(sum(has) > 0L)
  {
    meta$static$set(i, TRUE);
    spec$set(i, list(set[!has]));
  }
  return(TRUE);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Static methods/properties can only refer to other static members.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
references_static <- \(i, name, j, meta, ref, env, err)
{
  if(!meta$static$get(j))
  {
    err$push(
      cls = "ooprRefNotStatic"
     ,src = ref$src %||% env$src[[i]]
     ,msg = "Static member `%s` is attempting to use non-static member `%s`."
     ,name, references_expr(ref)
    );
    env$succ$set(i, FALSE);
  }
}
