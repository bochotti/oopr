expect_env <- \(object, expected)
{
  act <- quasi_label(rlang::enquo(object));
  exp <- quasi_label(rlang::enquo(expected));
  if(is.function(object)) object <- environment(object);
  if(!is.environment(object))
  {
    fail(c(
      sprintf("Expected %s to be an environment", act$lab)
     ,sprintf("Actual mode: %s", mode(object))
    ));
    invisible(object);
  }
  expect_equal(
    format.default(object)   ,label = act$lab
   ,format.default(expected) ,expected.label = exp$lab
  );
}
