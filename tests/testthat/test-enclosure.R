## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("interface",
{
  env <- new.env();
  env$a <- 1L;
  env$b <- \() { 2L; };
  makeActiveBinding("c", \(x) { if(missing(x)) env$a else env$a <- x; }, env);

  it("asserts",
  {
    expect_error(
      interface(1L)
     ,"`env` must be an environment"
    );
    expect_error(
      interface(env, 1L)
     ,"`nms` must be a character vector"
    );
    expect_error(
      interface(env, class = 1L)
     ,"`cls` must be a character vector"
    );
  })

  it("creates references to target env",
  {
    intf <- interface(env);
    expect_setequal(names(intf), c('a', 'b', 'c'));
    expect_true(bindingIsActive('a', intf));
    expect_false(bindingIsActive('b', intf));
    expect_true(bindingIsActive('c', intf));

    expect_identical(intf$a, env$a);
    expect_identical(intf$b, env$b);
    expect_identical(
      activeBindingFunction('c', intf)
     ,activeBindingFunction('c', env)
    );

    intf$a <- 2L;
    expect_identical(intf$a, env$a);
    intf$c <- 3L;
    expect_identical(intf$a, env$a);
    expect_identical(intf$c, env$c);
  })

  it("can choose only select names",
  {
    intf <- interface(env, 'a');
    expect_setequal(names(intf), 'a');
  })

  it("carries the class",
  {
    class(env) <- "test";
    intf <- interface(env);
    expect_s3_class(intf, "test");
  })

  it("carries locked status",
  {
    lockBinding('a', env)
    lockEnvironment(env);
    intf <- interface(env);
    expect_true(bindingIsLocked('a', intf));
    expect_false(bindingIsLocked('b', intf));
    expect_true(environmentIsLocked(intf));
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("enclosure",
{
  it("creates the enclosure",
  {
    test <- oopr("test",, { a <- \( ) { } })
    expect_true(is.environment(test@encl));
    expect_true(is.environment(test@encl$this));
    expect_true(is.environment(test@encl$.this));
    expect_true(environmentIsLocked(test@encl));
    expect_true(bindingIsLocked('a', test@encl$this));
    expect_env(parent.env(test@encl), environment())
  })

  it("changes the environment of functions",
  {
    test <- oopr("test",, { a <- \( ) { } })
    expect_env(test@encl$this$a, test@encl);
  })

  it("changes the environment of active bindings",
  {
    test <- oopr("test",, { get:a <- \( ) { } })
    expect_env(activeBindingFunction('a', test@encl$this), test@encl);
  })

  it("keeps fields as-is",
  {
    test <- oopr("test",, { a <- 1L });
    expect_false(bindingIsActive('a', test@encl$this));
    expect_equal(test@encl$this$a, 1L);
  })

  # it("does not lock static and places in .this",
  # {
  #   test <- oopr("test",, { static:a <- 1L });
  #   expect_false(bindingIsActive('a', test@encl$this));
  #   expect_equal(test@encl$this$a, 1L);
  # })

})
