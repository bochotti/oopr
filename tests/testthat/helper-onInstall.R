## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' Simulate installing another package
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
local_packageInstall <- \(
  name = "ooprTest"
 ,description = sprintf(
    "Package: %s\nTitle:T\nVersion: 0.1\nDescription:T\nImports:\n    %s"
   ,name, pkg
  )
 ,namespace   = ""
 ,files
 ,envir = parent.frame()
 ,pkg   = "oopr"
)
{
  ns <- getNamespaceInfo(pkg, "path");
  # eval on.exit in envir
  on.exit <- \(expr, envir = envir)
  {
    expr <- substitute(expr);
    expr <- do.call(substitute, list(expr, parent.frame()));
    do.call(base::on.exit, list(expr, TRUE, FALSE), envir = envir);
  }

  # check if oopr already installed
  ips <- installed.packages();
  if(match("oopr", rownames(ips), 0L))
  {
    #browser();
  }

  # install recent development version of oopr
  libs <- withr::local_tempdir(.local_envir = envir);
  withr::local_libpaths(libs, "prefix", .local_envir = envir);
  callr::rcmd("INSTALL", c(ns, sprintf("--library=%s", libs)));
  on.exit(remove.packages("oopr", libs), envir);

  # create new directory to put simulated package into
  dir  <- withr::local_tempdir(.local_envir = envir);
  dir.create(file.path(dir, 'R'));
  on.exit(unlink(file.path(dir, 'R'), TRUE), envir);

  # create description
  desc <- withr::local_file(file.path(dir, "DESCRIPTION"), .local_envir = envir);
  writeLines(description, desc);

  # create namespace
  nspc <- withr::local_file(file.path(dir, "NAMESPACE"), .local_envir = envir);
  writeLines(namespace, nspc);

  # create .R files
  for(nm in names(files))
  {
    if(!nzchar(nm)) stop("all `files` must be named");
    path <- sprintf("%s.R", nm);
    path <- withr::local_file(file.path(dir, 'R', path), .local_envir = envir);
    writeLines(files[nm], path);
  }

  # install simulated package
  callr::rcmd("INSTALL", c(dir, sprintf("--library=%s", libs)));
  on.exit(remove.packages(name, libs), envir);
  on.exit(unloadNamespace(name), envir);
  withr::local_package(name, lib.loc = libs, .local_envir = envir);
}

