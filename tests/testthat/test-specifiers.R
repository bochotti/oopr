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

  it("forces to be listed first",
  {
    expect_error(
      oopr("test",, { static:private:a <- 1L; })
     ,class = "ooprAccessSpecifierNotFirst"
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
    expect_equal(test@meta$access$get(1L), "private");
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("specifiers_special",
{
  it("Does not allow non-access specifiers",
  {
    expect_error(
      oopr("test",, { static:test <- \( ) { }})
     ,class = "ooprNonAccessSpecifierSpecial"
    );
    expect_error(
      oopr("test",, { static:~test <- \( ) { }})
     ,class = "ooprNonAccessSpecifierSpecial"
    );
  })

  it("does allow final specifier for public constructors",
  {
    expect_no_error(
      oopr("test",, { public:final:test <- \( ) { } })
    );
    expect_error(
      oopr("test",, { public:final:~test <- \( ) { }})
     ,class = "ooprNonAccessSpecifierSpecial"
    );
    expect_error(
      oopr("test",, { protected:final:test <- \( ) { } })
     ,class = "ooprFinalConstructorNotPublic"
    );
    expect_error(
      oopr("test",, { private:final:test <- \( ) { } })
     ,class = "ooprFinalConstructorNotPublic"
    );
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
