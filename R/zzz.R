## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
.onLoad <- \(libname, pkgname)
{
  oopr_onLoad();
  if(!exists("this", envir = .AutoloadEnv, inherits = FALSE))
  {
    assign("this", this, envir = .AutoloadEnv);
  }
  if(identical(.Platform$GUI, "RStudio"))
  {
    if(requireNamespace("rstudioapi", quietly = TRUE))
    {
      thiz = OoprCompletion@encl$this;
      thiz$source <- OoprCompletionRStudio();
    }
    if(Sys.getenv("R_OOPR_BREAKPOINTS") == "true")
    {
      try(OoprBreakpoints[["rstudio"]] <- TRUE, TRUE);
    }
  }
}
.onUnload <- \(libpath)
{
  if(OoprBreakpoints[["rstudio"]])
  {
    OoprBreakpoints[["rstudio"]] <- FALSE;
  }
  if(identical(get0("this", envir = .AutoloadEnv, inherits = FALSE), this))
  {
    rm(list = "this", envir = .AutoloadEnv);
  }
  gc();
  library.dynam.unload("oopr", libpath);
}
oopr_onInstall();
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
