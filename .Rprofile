## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Safe re-load of package... cos reloading .so cause issues with Cfinalizer
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
if(requireNamespace("pkgload", quietly = TRUE)) (\()
{
  fun <- pkgload::load_all;
  body(fun)[[2L]] <- call("{", body(fun)[[2L]], quote(
    if(is_dev_package(pkg_name(pkg_path(path))))
    {
      base::gc();
      unload(package = pkg_name(pkg_path(path)), quiet = base::isTRUE(quiet));
      base::gc();
    }
  ));
  ns <- getNamespace("pkgload");
  unlockBinding("load_all", ns);
  on.exit(lockBinding("load_all", ns));
  assign("load_all", fun, envir = ns);
})()

if(file.exists("~/.Rprofile")) source("~/.Rprofile");
