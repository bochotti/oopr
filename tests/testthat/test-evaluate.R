## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("evaluate_expr",
{
  it("only allows assignments in top-level",
  {
    expect_error(
      oopr("test",, { a; })
     ,class = "ooprNotATopLevelAssignment"
    );
    expect_error(
      oopr("test",, { a(b); })
     ,class = "ooprNotATopLevelAssignment"
    );
    expect_error(
      oopr("test",, { a <<- 1L; })
     ,class = "ooprNotATopLevelAssignment"
    );
    expect_no_error(
      oopr("test",, { a <- 1L; })
    );
    expect_no_error(
      oopr("test",, { a = 1L; })
    );
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("evaluate_lhs",
{
  it("only accepts `:`",
  {
    expect_error(
      oopr("test",, { a(b) <- 1L})
     ,class = "ooprLHSInvalidCall"
    );
    expect_error(
      oopr("test",, { b:a(b) <- 1L})
     ,class = "ooprLHSInvalidCall"
    );

    expect_no_error(
      oopr("test",, { a:b <- 1L})
    );
    expect_no_error(
      oopr("test",, { a:b:c <- 1L})
    );
  })

  it("accepts `~` only for the name",
  {
    expect_no_error(
      oopr("test",, { ~c <- 1L})
    );
    expect_no_error(
      oopr("test",, { a:~c <- 1L})
    );
    expect_error(
      oopr("test",, { ~a:~c <- 1L})
     ,class = "ooprLHSInvalidCall"
    );
    expect_error(
      oopr("test",, { a:b~c <- 1L})
     ,class = "ooprLHSInvalidCall"
    );
  })
})
