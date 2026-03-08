## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("specifiers_dupes",
{
  it("does not allow duplicated specifiers",
  {
    expect_error(
      oopr("test",, { a:a:b <- 1L; })
     ,class = "ooprDuplicateSpecifiers"
    );
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("specifiers_access",
{
  it("collects the access specifiers into meta",
  {
    oopr("test",, { public:a <- 1L; })
    expect_equal(test@meta$access$get(1L), "public");
  })

  it("does not allow multiple specifiers",
  {
    expect_error(
      oopr("test",, { public:private:a <- 1L; })
     ,class = "ooprMultipleAccessSpecifiers"
    );
  })

  it("will use the last specifier if not provided",
  {
    oopr("test",, { public:a <- 1L; b <- 2L; })
    expect_equal(test@meta$access$get(2L), "public");
    oopr("test",, { public:a <- 1L; b <- 2L; c <- 3L; })
    expect_equal(test@meta$access$get(3L), "public");
  })

  it("will default to private",
  {
    oopr("test",, { a <- 1L; })
    expect_equal(test@meta$access$get(2L), "private");
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("specifiers_unknown",
{
  it("catches any unknown specifiers",
  {
    expect_error(
      oopr("test",, { unknown:a <- 1L} )
     ,class = "ooprUnknownSpecifier"
    );
  })
})
