## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("ooprC",
{
  it("does not allow changing slots",
  {
    oopr("test",, { })
    expect_error(
      test@name <- 2L
     ,"ooprC objects are immutable"
    );
  })

  it("uses $ to access the interface",
  {
    oopr("test",, { a <- 2L })
    expect_error(test$a, "`a` is not a public static member");
    expect_error(test$a <- 2L, "`a` is not a public static member");
    oopr("test",, { public:static:a <- 2L })
    expect_no_error(test$a);
    expect_no_error(test$a <- 2L);
    expect_true(is.ooprC(test, "test"));

    # how to test this... lol
    oopr("test",, { public:static:get:a <- \( ) { stop("oh"); }})
  })

  it("can be checked with is.ooprC",
  {
    oopr("test",, { })
    expect_true(is.ooprC(test));
    expect_true(is.ooprC(test, "test"));
    expect_false(is.ooprC(1L));
    expect_false(is.ooprC(test, "test2"));
  })

  it("shows the names of static members",
  {
    oopr("test",, { public:a <- 1L; static:b <- 2L; })
    expect_equal(names(test), "b");
  })

  it("shows the address when format is used",
  {
    oopr("test",, {})
    expect_match(format(test), "^<test ooprC: 0x.+>$")
  })

  it("shows static members in print method",
  {
    oopr("test",, { public:a <- 1L; static:b <- 2L; });
    out <- capture.output(print(test));
    expect_match(out[1L], "^<test ooprC: 0x.+>$")
    expect_match(out[2L], "^Usage:$")
    expect_match(out[3L], "^  test\\(\\) $")
    expect_match(out[4L], "^Static Members:$")
    expect_match(out[5L], "\\$b: int 2$")
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("constructor",
{
  it("creates S4 object",
  {
    oopr("test",, { b <- 1L; })
    expect_s4_class(test, "ooprC");
  })

  it("uses the constructor method arguments",
  {
    oopr("test",, { test <- \(a, b = TRUE) { a; }; })
    expect_equal(formals(test@.Data), as.pairlist(alist(a=, b = TRUE)));
    expect_equal(body(test@.Data)[[4L]][-1L], as.call(alist(a, b)))
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("construct_make",
{
  oopr("test",, { public:a <- 1L; b <- \( ) { }; get:c <- \( ) { }})
  obj <- test();
  encl <- parent.env(obj);

  it("creates a new instance",
  {
    expect_env(encl, test@encl, inverse = TRUE);
    expect_env(encl$this, test@encl$this, inverse = TRUE);
    expect_env(encl$.this, test@encl$.this, inverse = TRUE);
    expect_s3_class(obj, c("test", "oopr"));
  })

  it("changes the environment for methods & properties",
  {
    expect_env(activeBindingFunction('a', encl$.this), encl);
    expect_env(obj$b, encl);
    expect_env(encl$this$b, encl);
    expect_true(bindingIsLocked('b', obj));
    expect_true(bindingIsLocked('b', encl$this));
    expect_true(bindingIsActive('c', obj));
    expect_true(bindingIsActive('c', encl$this));
    expect_env(activeBindingFunction('c', obj), encl);
    expect_env(activeBindingFunction('c', encl$this), encl);
  })

  it("keeps the enclosure as is",
  {
    encl <- test@encl;
    obj$a <- 2L;
    expect_equal(encl$this$a, 1L);
    expect_env(encl$this$b, encl);
    expect_env(activeBindingFunction('c', encl$this), encl);
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("construct_clean",
{
  it("removes the constructor method",
  {
    oopr("test",, { test <- \(a, b) { } } )
    obj <- test(1, 2);
    expect_false(hasName(obj, "test"));
  })

  it("registers destructor method",
  {
    env   <- new.env();
    env$a <- 1L;
    oopr("test",, { ~test <- \( ) { env$a <- 2L; } } )
    obj <- test();
    gc();
    expect_false(hasName(obj, "~test"));
    rm(obj);
    gc();
    expect_equal(env$a, 2L);
  })

  it("locks the enclosure",
  {
    oopr("test",, { } )
    obj  <- test();
    encl <- parent.env(obj);
    expect_true(environmentIsLocked(encl));
    expect_true(bindingIsLocked("this", encl));
    expect_true(bindingIsLocked(".this", encl));
  })
})
