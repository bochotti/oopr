## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Creates a new environment, which refers back to the original environment.
#' Fields specifically have an active binding created.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
interface <- \(env, names = NULL, class = NULL, sym)
{
  if(missing(sym))
  {
    sym <- substitute(env);
  }
  .Call(Cpp_interface, env, sym, names, class);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Create an enclosure which holds `this`, its interface, and inherited
#' classes.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
enclosure <- \(env, parent)
{
  inhr <- env$inhr;
  encl <- new.env(parent = parent, size = 2L + env$inhr$size - 1L);
  inms <- character(0L);
  for(i in inhr$along)
  {
    # include inherited classes into enclosure
    name <- inhr$meta$names$get(i);
    encl[[name]] <- inhr$this[[name]];
    # collect the names of these classes
    if(inhr$meta$access$get(i) == "public")
    {
      inms <- c(inms, name, encl[[name]]@inhr);
    }
  }

  this <- env$this;
  parent.env(this) <- encl;
  meta <- env$meta;
  for(i in seq_len(meta$size))
  {
    inhr <- meta$inherit$get(i);
    name <- meta$names$get(i);
    if(nzchar(meta$property$get(i)))
    {
      # property funds need to be converted to active bindings
      fun <- this[[name]];
      # TODO: will inherited properties break on install?
      if(!nzchar(inhr))
      {
        environment(fun) <- encl;
      }
      rm(list = name, envir = this);
      makeActiveBinding(name, fun, this);
    }
    else if(meta$method$get(i))
    {
      # methods need a change in environment
      if(!nzchar(inhr))
      {
        environment(this[[name]]) <- encl;
        if(meta$S3$get(i)) enclosure_S3(name, env$name, this, parent);
      }
      lockBinding(name, this);
    }
    else if(nzchar(inhr))
    {
      # inherited fields refer to their own enclosure
      rm(list = name, envir = this);
      symlink(encl[[inhr]]@encl$this, "this", this, name);
    }
    else if(meta$class$get(i) && meta$static$get(i))
    {
      this[[name]] <- eval(this[[name]], parent, NULL);
    }
    if(!meta$static$get(i))
    {
      lockBinding(name, this);
    }
  }
  # the constructors interface only reveals static members
  names <- meta$subs("names", access = "public", static = TRUE);
  class <- c(env$name, inms, "oopr");
  encl$this  <- this;
  encl$.this <- interface(this, names, class);
  lockEnvironment(this);
  lockEnvironment(encl$.this)
  lockEnvironment(encl, bindings = TRUE);
  return(encl);
}
