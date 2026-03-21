## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("evaluate_classmem",
{
  oopr("memb",, {})
  it("marks classes as members",
  {
    oopr("test",, { a <- memb; })
    expect_true(test@meta$class$get(1L));

    oopr("test",, { a <- memb(); })
    expect_true(test@meta$class$get(1L));

    old1 <- str.oopr;
    unlockBinding("str.oopr", asNamespace("oopr"));
    on.exit({
      assign("str.oopr", old1, asNamespace("oopr"));
      lockBinding("str.oopr",  asNamespace("oopr"));
    })
    oopr("str.oopr",, {}, parent = asNamespace("oopr"))

    oopr("test",, { a <- oopr:::str.oopr; })
    expect_true(test@meta$class$get(1L));

    oopr("test",, { a <- oopr:::str.oopr(); })
    expect_true(test@meta$class$get(1L));
  })

})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("definitions_classmem",
{
  it("inserts call into constructor method",
  {
    oopr("memb",, {})
    oopr("test",, { a <- memb; })
    expect_identical(body(test@encl$this$test)[[2:1]], quote(base::assign));

    oopr("test",, { a <- memb(); })
    expect_identical(body(test@encl$this$test)[[2:1]], quote(base::assign));

    oopr("memb",, { memb <- \(x) { }})
    oopr("test",, { a <- memb(1L); })
    expect_identical(body(test@encl$this$test)[[2:3]], quote(this$a(x = 1L)));

    oopr("memb",, { memb <- \(x = 1) { }})
    oopr("test",, { a <- memb; })
    expect_identical(body(test@encl$this$test)[[2:3]], quote(this$a()));
  })

  it("requires initialization of classes with non-default arguments",
  {
    oopr("memb",, { memb <- \(x) { }})
    expect_error(
      oopr("test",, { a <- memb; })
     ,class = "ooprDefNoInit"
    );
    expect_error(
      oopr("test",, { test <- \( ) { this$a(); }; a <- memb; })
     ,class = "ooprDefInitSignatureNotMatched"
    );
    expect_no_error(
      oopr("test",, { test <- \( ) { this$a(1); }; a <- memb; })
    );
  })

  it("does not allow multiple initializations",
  {
    oopr("memb",, { memb <- \( ) { }})
    expect_error(
      oopr("test",, { test <- \( ) { this$a(); this$a(); }; a <- memb; })
     ,class = "ooprDefMultipleInit"
    );
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("references_classmem",
{
  it("does not allow usage prior to init",
  {
    oopr("memb",, { public:a <- 1L; })
    expect_error(
       oopr("test",, { test <- \( ) { this$a$a; this$a(); }; a <- memb; })
      ,class = "ooprClassMemUsageBeforeInit"
    );
    expect_no_error(
       oopr("test",, { test <- \( ) { this$a(); this$a$a; }; a <- memb; })
    );
  })

  # it("does not allow usage of undefined members",
  # {
  #   oopr("memb",, { public:a <- 1L; })
  #   expect_error(
  #      oopr("test",, { test <- \( ) { this$a$b; }; a <- memb; })
  #     ,class = "ooprClassMemUsageBeforeInit"
  #   );
  #   expect_no_error(
  #      oopr("test",, { test <- \( ) { this$a(); this$a$a; }; a <- memb; })
  #   );
  # })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("classmem",
{
  oopr("memb",, { public:a <- 1L; });
  oopr("test",, { public:a <- memb; })
  obj <- test();
  expect_true(is.oopr(obj$a, "memb"));
  obj$a$a <- 2L;
  expect_equal(obj$a$a, 2L);
})
