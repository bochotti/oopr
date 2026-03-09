## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("oopr asserts",
{
  it("must have a single character for name",
  {
    expect_error(oopr(1L));
    expect_error(oopr(letters));
    expect_error(oopr(NA_character_));
    expect_error(oopr(""));
    expect_no_error(oopr("class",,{}));
  })

  it("must have a { enclosure for definition",
  {
    expect_error(oopr("test"));
    expect_error(oopr("test",,a));
    expect_no_error(oopr("test",,{ a <- 1L; }));
  })

  it("must have an environment for a parent",
  {
    expect_error(oopr("test",,{}, parent = NULL));
    expect_no_error(oopr("test",,{}, parent = environment()));
  })

})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("is.oopr",
{
  test <- oopr("test",, { })()
  expect_true(is.oopr(test));
  expect_true(is.oopr(test, "test"));
  expect_false(is.oopr(1L));
  expect_false(is.oopr(test, "test2"));
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("str.oopr",
{
  test <- oopr("test",, {
    public:
      static:very_long <- 1L;
      member_names <- \(with, some, arguments) { }
      in_this_class <- list(a=1, b=2, c=3);
      get:err <- \( ) stop("blah blah")
      null    <- NULL
      dbl <-1.0
      lgl <- TRUE
      raw <- raw(1)
      cmp <- complex(1)
  })
  obj  <- test();
  obj$very_long <- test();
  obj$very_long <- obj;
  str(obj)
})
