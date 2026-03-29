## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Collect specifier.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
specifiers_S3 <- \(i, name, spec, meta, env, err)
{
  set <- spec$get(i)[[1L]];
  has <- match(set, "S3", 0L) > 0L;
  if(sum(has) > 0L)
  {
    meta$S3$set(i, TRUE);
    spec$set(i, list(set[!has]));
  }
  return(TRUE);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Ensure the name of an S3 is a generic, and the definition matches the
#' signature.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
definitions_S3  <- \(i, name, meta, env, err)
{
  if(!meta$S3$get(i)) return();
  if(meta$access$get(i) != "public" || !meta$method$get(i) ||meta$static$get(i))
  {
    err$push(
      cls = "ooprS3NotNonStaticMethod"
     ,src = env$src[[i]]
     ,msg = "S3 member `%s` must be a public non-static method."
     ,name
    );
    env$succ$set(i, FALSE);
    return();
  }
  bad <- c("$", "$<-", "[[", "[[<-", ".DollarNames");
  if(match(name, bad, 0L))
  {
    err$push(
      cls = "ooprS3BadName"
     ,src = env$src[[i]]
     ,msg = "S3 member `%s` cannot have the name %s."
     ,name, deparse1(bad)
    );
    env$succ$set(i, FALSE);
    return();
  }
  generic <- S3_get_generic(i, name, env, err);
  if(!is.null(generic)) S3_match_arguments(i, name, generic, env, err);
  return();
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @importFrom methods getGeneric
#' @intern
#' Find S3 generic
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
S3_get_generic <- \(i, name, env, err)
{
  # check S4 first, includes primitives
  gen <- methods::getGeneric(name, where = env$prnt);
  if(!is.null(gen))
  {
    x <- gen@.Data;
    environment(x) <- asNamespace(gen@package);
    return(x);
  }

  g <- "UseMethod"
  # check namespaces for generics, with search path done first
  loaded <- sub("^package:", "", grep("^package:", search(), value = TRUE));
  for(ns in unique(c(loaded, loadedNamespaces())))
  {
    ns <- asNamespace(ns);
    x  <- get0(name, envir = ns, inherits = FALSE);
    if(!is.function(x)) next;
    if(!length(findInExpr(body(x), \(e) iscall(e, g) && e[[2]] == name))) next;
    return(x);
  }

  err$push(
    cls = "ooprS3NotAGeneric"
   ,src = env$src[[i]]
   ,msg = "S3 member `%s` must have its name match a generic."
   ,name
  );
  env$succ$set(i, FALSE);
  return(NULL);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Arguments of S3 method must match the generic.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
S3_match_arguments <- \(i, name, generic, env, err)
{
  method <- env$this[[name]];
  gargs  <- names(formals(generic))[-1L];
  margs  <- names(formals(method));
  pass   <- TRUE;
  dots   <- match("...", gargs, 0L);
  if(dots)
  {
    # pre `...`
    pass <- identical(gargs[seq_len(dots - 1L)], margs[seq_len(dots - 1L)]);
    if(pass && length(gargs) > dots)
    {
      # post `...`
      pass <- identical(
        gargs[seq.int(dots + 1L, length(gargs))]
       ,margs[seq.int(to = length(margs), length.out = length(gargs) - dots)]
      );
    }
    # method also has `...`
    pass <- pass && match("...", margs, 0L);
  }
  else
  {
    pass <- identical(gargs, margs);
  }

  if(!pass)
  {
    err$push(
      cls = "ooprS3ArgumentsNotMatched"
     ,src = env$src[[i]]
     ,msg = "S3 member `%s` argument names %s does not match generic `%s::%s`
             argument names %s."
     ,name
     ,deparse1(margs)
     ,environmentName(environment(generic)), name
     ,deparse1(gargs)
    );
    env$succ$set(i, FALSE);
  }
  else
  {
    attr(env$this[[name]], ".__ARG__.") <- formals(generic)[1L];
  }
  return();
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Register S3 method
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
enclosure_S3 <- \(name, class, this, parent)
{
  fun <- this[[name]];
  arg <- attr(fun, ".__ARG__.", TRUE);
  attr(this[[name]], ".__ARG__.") <- NULL;

  # insert non-first arguments into the call
  # non-dots should be by name = value, to stop dots swallowing
  args <- names(formals(fun));
  names(args) <- args;
  args <- lapply(args, as.name);
  names(args)[match("...", names(args), 0L)] <- "";

  formals(fun)     <- c(arg, formals(fun));
  arg              <- as.name(names(arg));
  body(fun)        <- as.call(c(call(".subset2", arg, name), args));
  environment(fun) <- parent;
  attributes(fun)  <- attributes(this[[name]]);

  #registerS3method(name, class, method, parent);
  assign(sprintf("%s.%s", name, class), fun, envir = parent);
}
