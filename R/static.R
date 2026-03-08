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
