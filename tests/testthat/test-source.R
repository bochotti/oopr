## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("OoprSourceContext",
{
  tmp <- tempfile();
  on.exit(rm(tmp));

  text <- r"{
  oopr("test",,
  {
  public:
    method <- \( )
    {
      return(1L);
    }
  })
  }"
  cat(text, file = tmp);
  obj <- OoprSourceContext(file = tmp);

  it("collects the objects defined in the file",
  {
    expect_equal(obj$file, tmp);
    expect_true(is.call(obj$defs[[1L]]));
    expect_equal(
      paste(collapse = "\n", capture.output(obj$defs[[1L]]))
     ,regmatches(text, regexpr("(?<c>\\{([^{}]|(?&c))*\\})", text, perl = TRUE))
    );
  })

  it("can pull the ooprC from position",
  {
    expect_error(obj$getByPos(1));
    expect_error(obj$getByPos(3, 2));
    expect_true(is.ooprC(obj$getByPos(3, 3), "test"));
    expect_true(is.ooprC(obj$getByPos(9, 3), "test"));
    expect_error(obj$getByPos(9, 4));
    expect_error(obj$getByPos(10));
  })

  text <- r"{
  oopr("test",,
  {
  public:
    method <- \( )
    {
      return(1L);
    }
  })
  {
    oopr("test2", test,
    {
    public:
      method2 <- \( )
      {
        return(1L);
      }
    })
  }
  }"
  cat(text, file = tmp);
  obj <- OoprSourceContext(file = tmp);

  it("can collect more than one class",
  {
    expect_length(obj$defs, 2L);
    expect_true(is.ooprC(obj$getByPos(9, 1), "test"));
    expect_error(obj$getByPos(10));
    expect_error(obj$getByPos(12, 4));
    expect_true(is.ooprC(obj$getByPos(12, 5), "test2"));
    expect_true(is.ooprC(obj$getByPos(18, 5), "test2"));
    expect_error(obj$getByPos(18, 6));
  })

})

