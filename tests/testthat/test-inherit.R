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

  it("throws if no package",
  {
    expect_error(
      oopr("test", pkg::base, {})
     ,class = "ooprInheritPackageNotFound"
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

    oopr("base",, {})
    oopr("test", { base; }, { test <- \( ) { 1L; } })
    expect_identical(body(test@encl$this$test)[[c(2:1)]], quote(base::assign));

    oopr('t', {base; test}, {})
    body(t@encl$this$t)
  })

  it("replaces call in place",
  {
    oopr("base",, {})
    oopr("test", { base; }, { test <- \( ) { 1L; base(); } })
    expect_identical(body(test@encl$this$test)[[c(3, 1)]], quote(base::assign));
  })

  it("requires initialization if derived class has non-default formals",
  {
    oopr("base",, { base <- \(x) { } })
    expect_error(
      oopr("test", { base; }, { })
     ,class = "ooprDefNoInit"
    );

    expect_error(
      oopr("test", { base; }, { test <- \( ) { } })
     ,class = "ooprDefNoInit"
    );

    oopr("base",, { base <- \(x = 1L) { } })
    expect_no_error(
      oopr("test", { base; }, { })
    );
  })

  it("does not allow multiple initialization of the same class",
  {
    oopr("base",, {})
    expect_error(
      oopr("test", { base; }, { test <- \( ) { base(); base(); }})
     ,class = "ooprDefMultipleInit"
    );
  })

  it("requires initialization to match the signature",
  {
    oopr("base",, { base <- \(x) { }})
    expect_error(
      oopr("test", { base; }, { test <- \( ) { base(y = 1) } })
     ,class = "ooprDefInitSignatureNotMatched"
    );

    oopr("base",, { base <- \(x, y = 1) { }})
    expect_error(
      oopr("test", { base; }, { test <- \( ) { base(y = 1) } })
     ,class = "ooprDefInitSignatureNotMatched"
    );
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("references_inheritance",
{
  it("does not allow referring to non-existant inherited classes members",
  {
    oopr("base",, { public:a <- 1L; })
    expect_error(
      oopr("test", { public:base; }, { b <- \( ) { base$b; } })
     ,class = "ooprRefNotDefined"
    );
    expect_no_error(
      oopr("test", { public:base; }, { b <- \( ) { base$a; } })
    );
  })

  it("does not allow accessing inherited classes private members",
  {
    oopr("base",, { private:a <- 1L; })
    expect_error(
      oopr("test", { public:base; }, { b <- \( ) { base$a; } })
     ,class = "ooprRefNotDefined"
    );
  })

  it("allows accessing inherited classes protected members",
  {
    oopr("base",, { protected:a <- 1L; })
    expect_no_error(
      oopr("test", { public:base; }, { b <- \( ) { base$a; } })
    );
  })

  it("follows same logic for properties, static, etc",
  {
    oopr("base",, { public:get:a <- \( ) { }; static:b <- 1L; c <- \(x) { } })

    expect_error(
      oopr("test", { public:base; }, { c <- \( ) { base$a <- 1L; } })
     ,class = "ooprRefBadAssignment"
    );
    expect_no_error(
      oopr("test", { public:base; }, { c <- \( ) { base$a; } })
    );

    expect_error(
      oopr("test", { public:base; }, { static:c <- \( ) { base$a; } })
     ,class = "ooprRefNotStatic"
    );
    expect_no_error(
      oopr("test", { public:base; }, { static:c <- \( ) { base$b; } })
    );
    expect_no_error(
      oopr("test", { public:base; }, { c <- \( ) { base$b; } })
    );

    expect_error(
      oopr("test", { public:base; }, { c <- \( ) { base$c(); } })
     ,class = "ooprRefUnmatchedCall"
    );

  })

  it("does not allow use of inherited class prior to initialization",
  {
    oopr("base",, { public:a <- 1L; })

    expect_error(
      oopr("test", { public:base; }, { test <- \( ) { base$a; base(); }})
     ,class = "ooprInheritUsageBeforeInit"
    );
    expect_error(
      oopr("test", { public:base; }, { test <- \( ) { this$a; base(); }})
     ,class = "ooprInheritUsageBeforeInit"
    );

    oopr("base2",, { public:b <- 1L; })
    expect_no_error(
      oopr("test", { base; base2; }, { test <- \( ) { this$b; base(); } })
    );
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
    expect_true(bindingIsLocked("base", test@encl));
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
    expect_true(bindingIsLocked("base", parent.env(obj)));
    expect_true(environmentIsLocked(inhr));
  })

  it("exposes inherited protected members",
  {
    oopr("base",, { protected:a <- 1L; })
    oopr("test", { protected:base; }, {})
    obj  <- test();
    inhr <- parent.env(obj)$base;
    expect_named(inhr, "a");

    oopr("test2", { test; }, {})
    obj  <- test2();
    inhr <- parent.env(obj)$test;
    expect_named(inhr, "a");

    inhr <- parent.env(inhr)$base;
    expect_named(inhr, "a");

    old1 <- str.oopr;
    old2 <- is.oopr;
    unlockBinding("str.oopr", asNamespace("oopr"));
    unlockBinding("is.oopr",  asNamespace("oopr"));
    on.exit({
      assign("str.oopr", old1, asNamespace("oopr"));
      lockBinding("str.oopr",  asNamespace("oopr"));
      assign("is.oopr", old2,  asNamespace("oopr"));
      lockBinding("is.oopr",  asNamespace("oopr"));
    })
    oopr("str.oopr", , { protected:a <- 1L; }, parent = asNamespace("oopr"))
    oopr("is.oopr", { oopr:::str.oopr; }, { }, parent = asNamespace("oopr"))

    # access via package / namespace
    obj  <- oopr:::is.oopr();
    inhr <- parent.env(obj)$str.oopr;
    expect_named(inhr, "a");

    # evaluate from a package namespace
    obj  <- evalq(is.oopr(), asNamespace("oopr"), NULL);
    inhr <- parent.env(obj)$str.oopr;
    expect_named(inhr, "a");
  })

  it("gives the inherited class a seperate enclosure",
  {
    oopr("base",, { protected:a <- 1L; })
    oopr("test", { base; }, {})
    obj  <- test();
    inhr <- parent.env(obj)$base;
    expect_env(parent.env(inhr), parent.env(obj), inverse = TRUE);
    expect_env(parent.env(inhr), base@encl, inverse = TRUE);
  })

  it("gives inherited members the inherited classes environment",
  {
    oopr("base",, { public:a <- 1L; b <- \( ) { }; get:c <- \( ) { } })
    oopr("test", { public:base; }, {})
    obj  <- test();
    this <- parent.env(obj)$this;
    inhr <- parent.env(obj)$base;

    expect_true(bindingIsActive("a", this));
    expect_false(bindingIsLocked("a", this));
    expect_equal(
      body(activeBindingFunction("a", this))
     ,quote(if(missing(x)) this$a else this$a <- x)
    );
    expect_env(activeBindingFunction("a", this), parent.env(inhr));

    expect_false(bindingIsActive("b", this));
    expect_true(bindingIsLocked("b", this));
    expect_env(this$b, parent.env(inhr));

    expect_true(bindingIsActive("c", this));
    expect_false(bindingIsLocked("c", this));
    expect_env(activeBindingFunction("c", this), parent.env(inhr));
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("inheritance",
{
  oopr("base",,
  {
    public:
      a     <- 1L;
      geta  <- \( ) { this$a; }
      seta  <- \(x) { this$a <- x; }
      get:b <- \( ) { this$a; }
      set:b <- \(x) { this$a <- x; }
  })

  it("public members",
  {
    oopr("test", { public:base; }, { public:a <- 1L; })
    obj <- test();
    obj$a <- 0L;
    expect_equal(obj$a, 0L);
    expect_equal(obj$geta(), 1L);
    expect_equal(obj$b, 1L);

    obj$seta(2L);
    expect_equal(obj$a, 0L);
    expect_equal(obj$geta(), 2L);
    expect_equal(obj$b, 2L);

    obj$b <- 3L;
    expect_equal(obj$a, 0L);
    expect_equal(obj$geta(), 3L);
    expect_equal(obj$b, 3L);
  })

  it("protected members",
  {
    oopr("test", { protected:base; },
    {
    public:
      a <- 1L;
      geta  <- \( ) { base$a; }
      seta  <- \(x) { base$a <- x; }
      get:b <- \( ) { base$a; }
      set:b <- \(x) { base$a <- x; }
    })
    obj <- test();
    obj$a <- 0L;
    expect_equal(obj$a, 0L);
    expect_equal(obj$geta(), 1L);
    expect_equal(obj$b, 1L);

    obj$seta(2L);
    expect_equal(obj$a, 0L);
    expect_equal(obj$geta(), 2L);
    expect_equal(obj$b, 2L);

    obj$b <- 3L;
    expect_equal(obj$a, 0L);
    expect_equal(obj$geta(), 3L);
    expect_equal(obj$b, 3L);
  })
})
