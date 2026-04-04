## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
.onLoad <- \(libname, pkgname)
{
  oopr_onLoad();
  if(!exists("this", envir = .AutoloadEnv, inherits = FALSE))
  {
    assign("this", this, envir = .AutoloadEnv);
  }
}
oopr_onInstall();
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
