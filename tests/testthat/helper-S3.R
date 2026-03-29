## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' Remove S3 method
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
rmS3 <- \(gen, class, ns, envir = parent.frame())
{
  table <- get(".__S3MethodsTable__.", envir = asNamespace(ns));
  name  <- sprintf("%s.%s", gen, class);

  expr <- substitute({
    if(exists(name, envir = table, inherits = FALSE))
    {
      rm(list = name, envir = table);
    }
  })
  do.call(on.exit, list(expr, TRUE, FALSE), envir = envir);
}
