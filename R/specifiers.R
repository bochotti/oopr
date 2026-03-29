## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Collect specifiers, which are moved into the `meta` object.
#' Each function called inside here should amend `meta` accordingly, and
#' remove that specifier from the `spec` object. It should return `TRUE` if
#' there are no errors.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
specifiers <- \(env, err)
{
  spec <- env$spec;
  meta <- env$meta;
  for(i in env$along)
  {
    name <- meta$names$get(i);
    if(!specifiers_dupes(i, name, spec, env, err))          next;
    if(!specifiers_access(i, name, spec, meta, env, err))   next;
    if(!specifiers_S3(i, name, spec, meta, env, err))       next;
    if(!specifiers_property(i, name, spec, meta, env, err)) next;
    if(!specifiers_static(i, name, spec, meta, env, err))   next;
    if(!specifiers_virtual(i, name, spec, meta, env, err))  next;
    if(!specifiers_final(i, name, spec, meta, env, err))    next;
    specifiers_unknown(i, name, spec, env, err);
  }
  return();
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Check for duplicates.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
specifiers_dupes <- \(i, name, spec, env, err)
{
  set <- spec$get(i)[[1L]];
  has <- duplicated(set);
  if(any(has))
  {
    err$push(
      cls = "ooprDuplicateSpecifiers"
     ,src = env$src[[i]]
     ,msg = "Member `%s` has duplicate specifiers: %s."
     ,name, deparse1(set[has])
    );
    env$succ$set(i, FALSE);
  }
  return(env$succ$get(i));
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' This will grab the access specifiers. If not defined, it will use the
#' previous specifier. If no previous member has one, then defaults
#' to private. A member cannot have multiple specifiers.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
specifiers_access <- \(i, name, spec, meta, env, err)
{
  use <- c("public", "protected", "private");
  set <- spec$get(i)[[1L]];
  has <- match(set, use, 0L) > 0L;
  if(sum(has) == 0L)
  {
    # use the last specifier
    for(j in seq_len(meta$access$size))
    {
      if(j == i) break;
      set <- meta$access$get(i - j);
      if(nzchar(set)) break;
    }

    # default to private
    if(!nzchar(set))
    {
      set <- "private";
    }
    meta$access$set(i, set);
  }
  else if(sum(has) == 1L)
  {
    meta$access$set(i, set[has]);
    spec$set(i, list(set[!has]));
  }
  else
  {
    err$push(
      cls = "ooprMultipleAccessSpecifiers"
     ,src = env$src[[i]]
     ,msg = "Member `%s` has multiple access specifiers: %s."
     ,name, deparse1(set[has])
    );
    env$succ$set(i, FALSE);
  }
  return(env$succ$get(i));
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Catch any specifiers that remain
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
specifiers_unknown <- \(i, name, spec, env, err)
{
  set <- spec$get(i)[[1L]];
  if(length(set))
  {
    err$push(
      cls = "ooprUnknownSpecifier"
     ,src = env$src[[i]]
     ,msg = "Member `%s` has %s: %s."
     ,name
     ,if(length(set) == 1L) "an unknown specifier" else "unknown specifiers"
     ,deparse1(set)
    );
    env$succ$set(i, FALSE);
  }
  return();
}
