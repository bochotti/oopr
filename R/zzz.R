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
      OoprCompletion$source <- OoprCompletionRStudio();
    }
    if(Sys.getenv("R_OOPR_BREAKPOINTS") == "true")
    {
      OoprBreakpoints$loadInGlobal(TRUE, TRUE);
    }
  }
}
.onUnload <- \(libpath)
{
  if(OoprBreakpoints$allLoadedInGlobal())
  {
    OoprBreakpoints$loadInGlobal(FALSE, TRUE);
  }
}
oopr_onInstall();
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
