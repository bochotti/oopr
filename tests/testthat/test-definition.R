## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("definitions_special",
{
  it("enforces print as public method with defaulted arguments",
  {
    expect_error(
      oopr("test",, { print <- 1L; })
     ,class = "ooprSpecialNotAMethod"
    );
    expect_error(
      oopr("test",, { print <- \( ) { } })
     ,class = "ooprPrintNotPublic"
    );
    expect_error(
      oopr("test",, { public:print <- \(a) { } })
     ,class = "ooprPrintNonDefaultArgs"
    );
    expect_error(
      oopr("test",, { public:print <- \(a, b = 1) { } })
     ,class = "ooprPrintNonDefaultArgs"
    );
    expect_no_error(
      oopr("test",, { public:print <- \(a = 1, b = 1) { } })
    );
  })

  it("enforces constructor method as private method without '.', '..' args",
  {
    expect_error(
      oopr("test",, { test <- 1L; })
     ,class = "ooprSpecialNotAMethod"
    );
    expect_error(
      oopr("test",, { public:test <- \( ) { } })
     ,class = "ooprSpecialNotPrivate"
    );
    expect_error(
      oopr("test",, { private:test <- \(.) { } })
     ,class = "ooprConstructorBadArgNames"
    );
    expect_error(
      oopr("test",, { private:test <- \(., a) { } })
     ,class = "ooprConstructorBadArgNames"
    );
    expect_error(
      oopr("test",, { private:test <- \(..) { } })
     ,class = "ooprConstructorBadArgNames"
    );
    expect_no_error(
      oopr("test",, { test <- \(a, b, c) { } })
    );
  })

  it("enforces destructor method as private method with no arguments",
  {
    expect_error(
      oopr("test",, { ~test <- 1L; })
     ,class = "ooprSpecialNotAMethod"
    );
    expect_error(
      oopr("test",, { public:~test <- \( ) { } })
     ,class = "ooprSpecialNotPrivate"
    );
    expect_error(
      oopr("test",, { private:~test <- \(x) { } })
     ,class = "ooprDestructorHasArgs"
    );
    expect_no_error(
      oopr("test",, { ~test <- \( ) { } })
    );
  })

})
