## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("specifiers_static",
{
  it("saves static in meta",
  {
    oopr("test",, { static:a <- 1L; })
    expect_true(test@meta$static$get(1L));
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("static",
{
  it("creates references to the constructors enclosure",
  {
    oopr("test",,
    {
    public:
      static:a     <- 1L;
      static:b     <- \( ) { }
      static:get:c <- \( ) { }
    })
    expect_false(bindingIsLocked('a', test@encl$this));
    expect_false(bindingIsLocked('c', test@encl$this));
    obj <- test();
    expect_env(activeBindingFunction('a', obj), test@encl);
    expect_env(obj$b, test@encl);
    expect_env(activeBindingFunction('c', obj), test@encl);
  })

  it("shares state between class instances",
  {
    oopr("test",,
    {
    public:
      static:a     <- 1L;
      static:b     <- \( ) { return(this$a); }
      static:get:c <- \( ) { return(this$c_); }
      static:set:c <- \(x) { this$c_ <- x; }
    private:
      static:c_    <- 'a';
    })
    obj1 <- test();
    obj2 <- test();
    obj1$a <- 2L;
    expect_equal(obj2$a, 2L);
    expect_equal(test$a, 2L);
    expect_equal(obj2$b(), 2L);
    expect_equal(test$b(), 2L);
    obj1$c <- 'b';
    expect_equal(obj2$c, 'b');
    expect_equal(test$c, 'b');
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("references_static",
{
  it("does not allow static members to refer to non-static members",
  {
    expect_error(
      oopr("test",, { a <- 1L; static:b <- \( ) { this$a; }})
     ,class = "ooprRefNotStatic"
    );
    expect_no_error(
      oopr("test",, { static:a <- 1L; static:b <- \( ) { this$a; }})
    );
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("static inheritance",
{
  oopr("base",, { public:static:a <- 1L; static:b <- \( ) { } })
  oopr("test", { public:base; }, { })
  expect_true(bindingIsActive("a", test@encl$this));
  expect_env(activeBindingFunction("a", test@encl$this), base@encl);
  expect_env(test@encl$this$b, base@encl)

  obj <- test();
  expect_env(activeBindingFunction("a", parent.env(obj)$base), base@encl);
  expect_env(obj$b, base@encl)
})
