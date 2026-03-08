expect_env <- \(object, expected, inverse = FALSE)
{
  act <- deparse1(substitute(object));
  exp <- deparse1(substitute(expected));
  if(is.function(object)) object <- environment(object);
  if(!is.environment(object))
  {
    fail(c(
      sprintf("Expected %s to be an environment", act)
     ,sprintf("  Actual mode: %s", mode(object))
    ));
    return(invisible(object));
  }
  object   <- format.default(object);
  expected <- format.default(expected);
  test     <- identical(object, expected);
  if(inverse) test <- !test;
  if(test)
  {
    pass();
  }
  else
  {
    fail(c(
      sprintf("Expected %s address to %smatch %s"
              ,act, if(inverse) "not " else "", exp
      )
     ,sprintf("  %s: %s", format(c(act, exp))[1L], object)
     ,sprintf("  %s: %s", format(c(act, exp))[2L], expected)
    ));
  }
  return(invisible(object))
}
