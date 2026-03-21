## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Capture a member that is being defined as another `oopr` class.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
definitions_classmem <- \(i, name, env, err)
{
  fun   <- env$this[[name]];
  envir <- quote(this);
  # inherited members change length of the meta
  along <- env$meta$subs(class = TRUE, inherit = "");
  along <- along[seq_along(env$succ$data)] & env$succ$data;
  along <- rev(which(along));
  for(i in along)
  {
    name  <- env$meta$names$get(i);
    ats   <- findInExpr(body(fun), \(e) {
      is.call(e) && (
           identical(e[[1L]], call("$",  quote(this), as.name(name)))
        || identical(e[[1L]], call("[[", quote(this), as.name(name)))
      )
    });
    call  <- env$this[[name]];
    if(!is.call(call) || iscall(call, c("::", ":::")))
    {
      call <- as.call(c(call));
    }
    env$this[[name]] <- eval(call[[1L]], env$prnt, NULL);
    call[[1L]] <- call("$", quote(this), as.name(name));
    fun   <- definitions_init(i, name, ats, call, envir, along, fun, env, err);
  }
  env$this[[env$name]] <- fun;
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
references_classmem <- \(i, name, refs, meta, access, encl, env, err)
{
  ""
  classes <- env$meta$subs("names", class = TRUE, inherit = "");
  refs    <- lapply(refs, `[`, match(refs$memb, classes, 0L) > 0L);
  if(name == env$name)
  {
    for(class in classes)
    {
      ref <- lapply(refs, `[`, match(refs$memb, class, 0L) > 0L);
      if(!length(ref$type)) next;
      if(ref$type[[1L]] != "call")
      {
        err$push(
          cls = "ooprClassMemUsageBeforeInit"
         ,src = ref$src[[1L]]
         ,msg = "Class member `%s` is being used prior to being initialized."
         ,class
        );
        env$succ$set(i, FALSE);
      }
    }
  }
  ""
  #refs <- lapply(refs, `[`, match(ref$type, "call", 0L) == 0L);
}
