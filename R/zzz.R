## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
.onLoad <- \(libname, pkgname)
{
  oopr_onLoad();
  if(!exists("this", envir = .AutoloadEnv, inherits = FALSE))
  {
    assign("this", this, envir = .AutoloadEnv);
  }
  if(   identical(.Platform$GUI, "RStudio")
     && requireNamespace("rstudioapi", quietly = TRUE)
  )
  {
    OoprCompletion$source <- OoprCompletionRStudio();
  }
}
.onUnload <- \(libpath)
{
  if(OoprBreakpoints$isLoadedInGlobal())
  {
    OoprBreakpoints$loadInGlobal(FALSE, TRUE);
  }
}
oopr_onInstall();
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
