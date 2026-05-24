## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("definitions_special",
{
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
      oopr("test",, { private:test <- \(.Call) { } })
     ,class = "ooprConstructorBadArgNames"
    );
    expect_no_error(
      oopr("test",, { test <- \(a, b, c) { } })
    );
  })

  it("does not allow `.this` in the constructor",
  {
    expect_error(
      oopr("test",, { test <- \() { .this; }})
     ,class = "ooprConstructorRefersToDotThis"
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

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("definitions_return",
{
  it("replaces returning this with .this",
  {
    oopr("test",, { a <- \( ) return(this) })
    expect_equal(body(test@encl$this$a), quote(return(.this)))

    oopr("test",, { a <- \( ) { return(this); } })
    expect_equal(body(test@encl$this$a), quote({ return(.this); }))

    oopr("test",, { a <- \( ) return(invisible(this)) })
    expect_equal(body(test@encl$this$a), quote(return(invisible(.this))))

    oopr("test",, { a <- \( ) { return(invisible(this)); } })
    expect_equal(body(test@encl$this$a), quote({ return(invisible(.this)); }))
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("definitions_args",
{
  it("does not allow argument names of enclosures",
  {
    expect_error(
      oopr("test",, { a <- \(this) { } })
     ,class = "ooprDefinitionBadArgs"
    );

    expect_error(
      oopr("test",, { a <- \(this, .this) { } })
     ,class = "ooprDefinitionBadArgs"
    );

    oopr("base",, { a <- \( ) { }})
    expect_error(
      oopr("test", base, { a <- \(base) { } })
     ,class = "ooprDefinitionBadArgs"
    );
  })
})
