## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("evaluate_property",
{
  it("makes duplication exception for properties",
  {
    expect_error(
      oopr("test",, { get:a <- 1L; get:a <- 2L; })
     ,class = "ooprDuplicateMember"
    );
    expect_error(
      oopr("test",, { set:a <- 1L; set:a <- 2L; })
     ,class = "ooprDuplicateMember"
    );
    expect_error(
      oopr("test",, { a <- 1L; get:a <- 2L; })
     ,class = "ooprDuplicateMember"
    );
    expect_error(
      oopr("test",, { get:a <- \( ) { }; set:a <- \(x) { }; get:a <- 3L})
     ,class = "ooprDuplicateMember"
    );
    expect_no_error(
      oopr("test",, { get:a <- \( ) { }; set:a <- \(x) { } })
    );
    expect_no_error(
      oopr("test",, { public:get:a <- \( ) { }; set:a <- \(x) { } })
    );
    expect_no_error(
      oopr("test",, { public:get:a <- \( ) { }; public:set:a <- \(x) { } })
    );
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("specifiers_property",
{
  it("allows get or set",
  {
    oopr("test",, { get:a <- \( ) { } })
    expect_equal(test@meta$property$get(1L), "get");
    oopr("test",, { set:a <- \(x) { } })
    expect_equal(test@meta$property$get(1L), "set");
  })

  it("does not allow both get and set at the same time",
  {
    expect_error(
      oopr("test",, { set:get:a <- \( ) { } })
     ,class = "ooprMultiplePropertySpecifiers"
    );
  })

  it("allows get and set with two seperate definitions",
  {
    oopr("test",, { get:a <- \( ) { }; set:a <- \(x) { } })
    expect_equal(test@meta$property$get(1L), "both");
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("property_get",
{
  it("does not allow get properties with arguments",
  {
    expect_error(
      oopr("test",, { get:a <- 1L })
     ,class = "ooprGetPropertyHasArgs"
    );
    expect_error(
      oopr("test",, { get:a <- \(x) { } })
     ,class = "ooprGetPropertyHasArgs"
    );
    expect_no_error(
      oopr("test",, { get:a <- \( ) { } })
    );
  })

  oopr("test",, { get:a <- \( ) { } });
  fun  <- activeBindingFunction('a', test@encl$this);
  it("creates the property function",
  {
    expect_equal(formals(fun), as.pairlist(alist(x=)));
    expect_true(isname(body(fun)[[c(2, 2, 2)]], "x"));
    expect_true(iscall(body(fun)[[c(2, 4)]], "stop"));
  })
  it("carries the srcref",
  {
    expect_false(is.null(attr(fun, "srcref")));
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("property_set",
{
  it("does not allow set properties without one argument",
  {
    expect_error(
      oopr("test",, { set:a <- 1L })
     ,class = "ooprSetPropertyNotOneArg"
    );
    expect_error(
      oopr("test",, { set:a <- \( ) { } })
     ,class = "ooprSetPropertyNotOneArg"
    );
    expect_error(
      oopr("test",, { set:a <- \(x = 1L) { } })
     ,class = "ooprSetPropertyNotOneArg"
    );
    expect_no_error(
      oopr("test",, { set:a <- \(x) { } })
    );
  })

  oopr("test",, { set:a <- \(y) { } })
  fun  <- activeBindingFunction('a', test@encl$this);
  it("creates the property function",
  {
    expect_equal(formals(fun), as.pairlist(alist(y=)));
    expect_true(isname(body(fun)[[c(2, 2, 2)]], "y"));
    expect_true(iscall(body(fun)[[c(2, 3)]], "stop"));
  })
  it("carries the srcref",
  {
    expect_false(is.null(attr(fun, "srcref")));
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("propert_both",
{
  it("enforces get and set seperately",
  {
    expect_error(
      oopr("test",, { get:a <- 1L; set:a <- \(x) { } })
     ,class = "ooprGetPropertyHasArgs"
    );
    expect_error(
      oopr("test",, { get:a <- \( ) { }; set:a <- 1L })
     ,class = "ooprSetPropertyNotOneArg"
    );
  })

  it("requires set to be defined immediately after get",
  {
    expect_error(
      oopr("test",, { set:a <- \(x) { }; get:a <- \( ) { } })
     ,class = "ooprBothPropertyNotOrdered"
    );
    expect_error(
      oopr("test",, { get:a <- \(x) { }; b <- 2L; set:a <- \( ) { } })
     ,class = "ooprBothPropertyNotOrdered"
    );
    expect_no_error(
      oopr("test",, { get:a <- \( ) { }; set:a <- \(x) { } })
    );
  })

  it("must have the same access specifier for get & set",
  {
    expect_error(
      oopr("test",, { get:a <- \( ) { }; public:set:a <- \(x) { } })
     ,class = "ooprBothPropertyNotSameAccess"
    );
  })

  oopr("test",, { get:a <- \( ) { x; }; set:a <- \(y) { z; } })
  fun  <- activeBindingFunction('a', test@encl$this)

  it("places get and set in correct spot",
  {
    body <- body(fun);
    expect_equal(formals(fun), as.pairlist(alist(y=)));
    expect_equal(body[[c(2, 3)]], quote({ x; }));
    expect_equal(body[[c(2, 4)]], quote({ z; }));
  })

  it("carries the srcrefs",
  {
    expect_false(is.null(attr(fun, "srcref")))
  })

  it("removes the set property definition",
  {
    expect_false(hasName(test@encl$this, ".a"));
    expect_disjoint(".a", test@meta$names$data);
  })

})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("property static",
{
  it("enforces both get and set to be static",
  {
    expect_error(
      oopr("test",, { get:a <- \( ) { }; static:set:a <- \(x) { }})
     ,class = "ooprBothPropertyNotSameStatic"
    );
    expect_error(
      oopr("test",, { static:get:a <- \( ) { }; set:a <- \(x) { }})
     ,class = "ooprBothPropertyNotSameStatic"
    );
    expect_no_error(
      oopr("test",, { static:get:a <- \( ) { }; static:set:a <- \(x) { }})
    );
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("references_property",
{
  it("does not allow setting a get-only property",
  {
    expect_error(
      oopr("test",, { get:a <- \( ) { }; b <- \( ) { this$a <- 1L; }})
     ,class = "ooprRefBadAssignment"
    );
  })

  it("does not allow accessing a set-only property",
  {
    expect_error(
      oopr("test",, { set:a <- \(x) { }; b <- \( ) { this$a; }})
     ,class = "ooprRefBadAccess"
    );
  })

  it("does not allow calling a property",
  {
    expect_error(
      oopr("test",, { get:a <- \( ) { }; b <- \( ) { this$a(); }})
     ,class = "ooprRefBadCall"
    );
  })


})
