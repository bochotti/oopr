## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' See if function has breakpoint
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
has_breakpoint <- \(fun)
{
  return(!is.null(attr(body(fun), "orig", TRUE)));
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' Create package directory
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
create_pkg <- \(name, envir = parent.frame())
{
  top  <- fake_namespace(name);
  root <- file.path(tempdir(), name);
  if(!dir.exists(root)) { dir.create(file.path(root, "R"), recursive = TRUE); }
  write.dcf(list(Package = name), file.path(root, "DESCRIPTION"));
  tmp <- tempfile(tmpdir = file.path(root, "R"), fileext = ".R");
  for(x in c("top", "root", "tmp"))
  {
    assign(x, get(x), envir = envir);
  }
  on.exit2(unlink(root, recursive = TRUE), envir);
}
