## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Create an enclosure which holds `this`, its interface, and inherited
#' classes.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
enclosure <- \(env, parent)
{
  encl <- new.env(parent = parent, size = 2L);
  this <- env$this;
  parent.env(this) <- encl;
  meta <- env$meta;
  for(i in seq_len(meta$size))
  {
    name <- meta$names$get(i);
    if(nzchar(meta$property$get(i)))
    {
      fun <- this[[name]];
      environment(fun) <- encl;
      rm(list = name, envir = this);
      makeActiveBinding(name, fun, this);
    }
    else if(meta$method$get(i))
    {
      environment(this[[name]]) <- encl;
      lockBinding(name, this);
    }
    if(!meta$static$get(i))
    {
      lockBinding(name, this);
    }
  }
  lockEnvironment(this);
  encl$this  <- this;
  # the constructors enclosure only reveals static members
  encl$.this <- interface(this, meta$subs("names", static = TRUE), env$name);
  lockEnvironment(encl);
  return(encl);
}
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
interface <- \(env, names = NULL, class = NULL, sym)
{
  if(missing(sym))
  {
    sym <- substitute(env);
  }
  .Call("interface", env, sym, names, class, PACKAGE = "oopr");
}
