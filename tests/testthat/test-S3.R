## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("specifiers_S3",
{
  oopr("test",, { public:S3:head <- \(x, ...) { } })
  expect_true(test@meta$S3$get(1L))
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("definitions_S3",
{
  it("requires S3 to be a public non-static method",
  {
    expect_error(
      oopr("test",, { private:S3:a <- \( ) { } })
     ,class = "ooprS3NotNonStaticMethod"
    );
    expect_error(
      oopr("test",, { public:S3:a <- 1L })
     ,class = "ooprS3NotNonStaticMethod"
    );
    expect_error(
      oopr("test",, { public:static:S3:a <- \( ) { } })
     ,class = "ooprS3NotNonStaticMethod"
    );
  })

  it("does not allow specific methods",
  {
    expect_error(
      oopr("test",, { public:S3:`$` <- \( ) { } })
     ,class = "ooprS3BadName"
    );
    expect_error(
      oopr("test",, { public:S3:`$<-` <- \( ) { } })
     ,class = "ooprS3BadName"
    );
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("S3_get_generic",
{
  it("requires the name of the member to be a generic",
  {
    expect_error(
      oopr("test",, { public:S3:abbreviate <- \( ) { } })
     ,class = "ooprS3NotAGeneric"
    );
    rmS3("str", "test", "utils");
    expect_no_error(
      oopr("test",, { public:S3:str <- \(...) { } })
    );
    rmS3("+", "test", "base");
    expect_no_error(
      oopr("test",, { public:S3:`+` <- \(e2) { } })
    );
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("S3_match_arguments",
{
  it("requires the arguments to match the generic",
  {
    expect_error(
      oopr("test",, { public:S3:plot <- \(x, ...) { } })
     ,class = "ooprS3ArgumentsNotMatched"
    );
    expect_error(
      oopr("test",, { public:S3:plot <- \(y) { } })
     ,class = "ooprS3ArgumentsNotMatched"
    );

    expect_error(
      oopr("test",, { public:S3:`[<-` <- \(i, j, ..., val) { } })
     ,class = "ooprS3ArgumentsNotMatched"
    );
    expect_error(
      oopr("test",, { public:S3:`[<-` <- \(i, j, ..., value, z) { } })
     ,class = "ooprS3ArgumentsNotMatched"
    );
  })

  it("allows additional arguments for `...`",
  {
    expect_no_error(
      oopr("test",, { public:S3:str <- \(a, ...) { } })
    );
    expect_no_error(
      oopr("test",, { public:S3:str <- \(..., a) { } })
    );

    expect_no_error(
      oopr("test",, { public:S3:`[<-` <- \(i, j, a, ..., value) { } })
    );
    expect_no_error(
      oopr("test",, { public:S3:`[<-` <- \(i, j, ..., a, value) { } })
    );
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("S3",
{
  it("Registers S3 methods against the class",
  {
    oopr("test",, { public:S3:str <- \(...) { return("a"); }})
    expect_identical(
      getS3method("str", "test")
     ,\(object, ...) { .subset2(object, "str")(...); }
    );
    obj <- test();
    expect_equal(str(obj), "a")
  })
})
