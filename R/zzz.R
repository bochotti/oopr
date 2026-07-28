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
      OoprBreakpoints@encl$this$loadInGlobal(TRUE, TRUE);
    }
  }
}
.onUnload <- \(libpath)
{
  if(OoprBreakpoints@encl$this$allLoadedInGlobal())
  {
    OoprBreakpoints@encl$this$loadInGlobal(FALSE, TRUE);
  }
  library.dynam.unload("oopr", libpath);
}
oopr_onInstall();
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
