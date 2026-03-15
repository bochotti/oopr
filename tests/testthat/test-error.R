## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("error",
{
  op <- options(ooprCompilationErrorMessages = TRUE);
  on.exit(options(op));
  it("throws error with class",
  {
    err <- error(quote(a));
    err$push("eee", NULL, "a");
    capture.output(expect_error(err$throw(), class = "eee"));
  })

  it("prints the message",
  {
    err <- error(quote(a));
    err$push("eee", NULL, "aaa");
    expect_output(expect_error(err$throw(), class = "eee"), "aaa");
  })

  it("prints the position in source",
  {
    src <- integer(5L)
    src[c(1, 5)] <- c(10, 20)
    attr(src, "srcfile") <- list(filename = "file");
    err <- error(quote(a));
    err$push("eee", src, "aaa");
    expect_output(expect_error(err$throw(), class = "eee"), "file:10:20");
  })
})
