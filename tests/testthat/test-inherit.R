## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("inheritance_yank",
{
  oopr("base",, { a <- 1L; })

  it("only allows names",
  {
    expect_error(
      oopr("test", { base(); }, { b <- 1L; })
     ,class = "ooprInheritBadQuote"
    );
    expect_error(
      oopr("test", { 1L; }, { b <- 1L; })
     ,class = "ooprInheritBadQuote"
    );
  })

  it("allows for packages",
  {
    #oopr("test", pkg::base, {})
  })


})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("inheritance_spec",
{
  oopr("base",, { a <- 1L; })

  it("only allows access specifiers",
  {
    expect_error(
      oopr("test", { get:base; }, { b <- 1L; })
     ,class = "ooprInheritBadSpecifier"
    )
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("inheritance_get",
{
  it("throws if object not found",
  {
    expect_error(
      oopr("test", { aaaa; }, { b <- 1L; })
     ,class = "ooprInheritNotFound"
    );
    expect_error(
      oopr("test", { base::aaaa; }, { b <- 1L; })
     ,class = "ooprInheritNotFound"
    );
  })

  it("throws if object is not an ooprC",
  {
    expect_error(
      oopr("test", { sum; }, { b <- 1L; })
     ,class = "ooprInheritNotOopr"
    );
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("inheritance_set",
{
  oopr("base",, { a <- 1L; protected:b <- 2L; public:c <- 3L})

  it("does not inherit private members",
  {
    oopr("test", { private:base; }, {})
    expect_length(test@meta$subs("names", names = "a"), 0L);
    test@encl$this$base
  })

  it("records the inherited class in the meta",
  {
    oopr("test", { private:base; }, {})
    expect_setequal(test@meta$subs("inherit", names = c("b", "c")), "base");
  })

  it("inherits private classes members as private",
  {
    oopr("test", { private:base; }, {})
    expect_setequal(test@meta$subs("access", names = c("b", "c")), "private");
  })

  it("inherits protected class members as protected",
  {
    oopr("test", { protected:base; }, {})
    expect_setequal(test@meta$subs("access", names = c("b", "c")), "protected");
  })

  it("inherits public members as public, but not protected members",
  {
    oopr("test", { public:base; }, {})
    expect_equal(
      test@meta$subs("access", names = c("b", "c"))
     ,c("protected", "public")
    );
  })

  it("does not overwrite existing members",
  {
    oopr("test", { base; }, { a <- 1L; b <- 2L; c <- 3L})
    expect_length(test@meta$subs("names", TRUE, inherits = ""), 0L);
  })


})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("inheritance_definitions",
{
  it("inserts call when inherited class doesnt require args",
  {
    oopr("base",, {})
    oopr("test", { base; }, { })
    expect_identical(body(test@encl$this$test)[[c(2:1)]], quote(base::assign));
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("inheritance enclosure",
{
  it("includes the inherited class in the enclosure",
  {
    oopr("base",, { a <- 1L; protected:b <- 2L; public:c <- 3L})
    oopr("test", { public:base; }, {})
    expect_identical(test@encl$base, base);
  })

  it("does not change the environment of inherited methods/properties",
  {
    oopr("base",, { public:a <- \( ) { }; get:b <- \( ) { } })
    oopr("test", { public:base; }, {})
    expect_env(test@encl$this$a, base@encl);
    expect_env(activeBindingFunction('b', test@encl$this), base@encl);
  })

  it("creates symlinks for fields",
  {
    oopr("base",, { public:a <- 1L })
    oopr("test", { public:base; }, {})
    expect_true(bindingIsActive('a', test@encl$this));
    expect_env(activeBindingFunction('a', test@encl$this), base@encl);
  })

  it("appends publicly inherited class",
  {
    oopr("base",, { })

    oopr("test", { public:base; }, {})
    expect_equal(test@inhr, "base");
    expect_equal(class(test@encl$.this), c("test", "base", "oopr"));

    oopr("test", { protected:base; }, {})
    expect_equal(test@inhr, "base");
    expect_equal(class(test@encl$.this), c("test", "oopr"));
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("inheritance construct",
{
  it("creates an instance of the inherited class in the enclosure",
  {
    oopr("base",, { })
    oopr("test", { base; }, {})
    obj <- test();
    inhr <- parent.env(obj)$base;
    expect_true(is.oopr(inhr, "base"));
  })
})
