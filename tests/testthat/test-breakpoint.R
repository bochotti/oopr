## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("OoprBreakpointsFunction",
{
  tmp <- tempfile(fileext = ".R");
  on.exit(unlink(tmp));
  text <- r"{
  oopr("test",,
  {
  public:
    method       <- \( )
    {
      1L;
      {
        2L;
      }
    }
    get:property <- \( )
    {
      3L;
      {
        4L;
      }
    }
  })
  }";
  cat(text, file = tmp);
  source(tmp, local = TRUE);
  obj <- OoprBreakpointsFunction("method", test);

  it("loads",
  {
    expect_equal(obj$name, "method");
    expect_equal(obj$ooprC, test);
    expect_equal(obj$encl, obj$ooprC@encl);
    expect_false(obj$property);
    expect_equal(obj$fun, test@encl$this$method)
  })

  it("knows when the function is out of sync with file",
  {
    expect_true(obj$isInSync());
    cat(strsplit(text, '\n')[[1L]][-9L], sep = '\n', file = tmp);
    expect_false(obj$isInSync());
    cat(text, file = tmp);
    expect_true(obj$isInSync());
  })

  it("can tell which line number covers the function",
  {
    expect_false(obj$hasLine(1L));
    expect_false(obj$hasLine(3L));
    expect_true(obj$hasLine(4L));
    expect_false(obj$hasLine(12L));
  })

  it("can get steps from a line",
  {
    expect_equal(obj$getSteps(7L), "2");
    expect_equal(obj$getSteps(9L), "3,2");
  })

  it("can set a breakpoint",
  {
    expect_no_error(obj$setBreakpoints(c("2", "3,2")));
    expect_s4_class(test@encl$this$method, "functionWithTrace");
    expect_equal(
      body(test@encl$this$method)[[c(2, 2)]]
     ,quote(.doTrace(browser()))
    );
    expect_equal(
      body(test@encl$this$method)[[c(3, 2, 2)]]
     ,quote(.doTrace(browser()))
    );
    expect_equal(obj$breaks, c("2" = 7, "3,2" = 9));
  })

  it("can remove breakpoints",
  {
    obj$setBreakpoints();
    expect_false(inherits(test@encl$this$method, "functionWithTrace"));
  })

  it("can set breakpoints on class instances",
  {
    obj$setBreakpoints("2");
    test1 <- test();
    expect_s4_class(test1$method, "functionWithTrace");
    obj$setBreakpoints();
    expect_false(inherits(test1$method, "functionWithTrace"));
    obj$setBreakpoints("2");
    expect_s4_class(test1$method, "functionWithTrace");
  })

  it("obtains the active binding function of a property",
  {
    obj <- OoprBreakpointsFunction("property", test);
    expect_true(obj$property);
    expect_identical(obj$fun, activeBindingFunction("property", obj$encl$this));
    obj$setBreakpoints("2");
    expect_s4_class(
      activeBindingFunction("property", obj$encl$this), "functionWithTrace"
    );
  })

  text <- r"{
  oopr("base",, { public:method <- \( ) { 1L; } })
  oopr("der1", public:base, { })
  oopr("der2", public:base, { public:method <- \( ) { 2L; } })
  oopr("der3", protected:base, { })
  oopr("test",, { public:memb <- base; })
  }";
  cat(text, file = tmp);
  source(tmp, local = TRUE);

  obj <- OoprBreakpointsFunction("method", base);
  it("sets breakpoints for inheriting classes",
  {
    d1 <- der1();
    d3 <- der3();
    obj$setBreakpoints("2");
    expect_s4_class(d1$method, "functionWithTrace");
    expect_s4_class(parent.env(d3)$this$method, "functionWithTrace");
    expect_s4_class(parent.env(d3)$base$method, "functionWithTrace");
    obj$setBreakpoints();
  })

  it("does not set breakpoint if method overridden",
  {
    d2 <- der2();
    obj$setBreakpoints("2");
    expect_false(inherits(d2$method, "functionWithTrace"));
    obj$setBreakpoints();
  })

  it("sets breakpoints inside class members",
  {
    t <- test();
    obj$setBreakpoints("2")
    expect_s4_class(t$memb$method, "functionWithTrace");
    obj$setBreakpoints();
  })

})


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("OoprBreakpointsClass",
{
  tmp <- tempfile(fileext = ".R");
  on.exit(unlink(tmp));
  text <- r"{
  oopr("test",,
  {
  public:
    method       <- \( )
    {
      1L;
      {
        2L;
      }
    }
    get:property <- \( )
    {
      3L;
      {
        4L;
      }
    }
  })
  }";
  cat(text, file = tmp);
  source(tmp, local = TRUE);

  obj <- OoprBreakpointsClass(test);

  it("loads",
  {
    expect_equal(obj$name, "test");
    expect_equal(obj$ooprC, test);
    expect_equal(obj$functions$size, 3L);
    expect_equal(obj$functions$keys, c("method", "property", "test"));
  })

  it("tells when a class has a function name and/or line",
  {
    expect_true(obj$has("method"));
    expect_true(obj$has("method"), 9L);
    expect_false(obj$has("a"))
    expect_false(obj$has("method", 15L));
  })

  it("knows when a function goes out of sync",
  {
    expect_true(obj$isInSync("method"));
    cat(strsplit(text, '\n')[[1L]][-16L], sep = '\n', file = tmp);
    expect_true(obj$isInSync("method"));
    expect_false(obj$isInSync("property"));
    cat(text, file = tmp);
    expect_true(obj$isInSync("property"));
  })

  it("gets steps in an rstudio format",
  {
    out <- obj$getSteps("method", 7L);
    expect_equal(out$name, "method", ignore_attr = TRUE);
    expect_equal(out$line, 7, ignore_attr = TRUE);
    expect_equal(out$at, "test:2", ignore_attr = TRUE);
  })

})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("OoprBreakpointsFile",
{
  tmp <- tempfile(fileext = ".R");
  on.exit(unlink(tmp));
  text <- r"{
  oopr("test1",,
  {
  public:
    method       <- \( )
    {
      1L;
      {
        2L;
      }
    }
  })
  oopr("test2",,
  {
    method <- \( )
    {
      3L;
      {
        4L;
      }
    }
  })
  }";
  cat(text, file = tmp);
  local <- environment();
  source(tmp, local = local);
  obj <- OoprBreakpointsFile(tmp, local);

  it("loads",
  {
    expect_equal(obj$file, tmp);
    expect_equal(obj$timestamp, file.mtime(tmp));
    expect_equal(obj$classes$keys, c("test1", "test2"));
  })

  it("can reload classes when file is modified",
  {
    cat(sub("1L", "5L", text), file = tmp);
    source(tmp, local = local);
    expect_false(obj$isInSync(name = "method"));
    obj$syncClassesWithFile(local);
    expect_true(obj$isInSync(name = "method"));
    expect_equal(obj$timestamp, file.mtime(tmp));
    expect_identical(obj$classes["test1"]$ooprC, test1);
  })

  it("indicates which class has a function and or line",
  {
    expect_equal(obj$has("method"), c(test1 = TRUE, test2 = TRUE));
    expect_equal(obj$has("method", 17L), c(test1 = FALSE, test2 = TRUE));
    expect_equal(obj$has("methoda"), c(test1 = FALSE, test2 = FALSE));
    expect_equal(obj$has("method", 1L), c(test1 = FALSE, test2 = FALSE));
  })

  it("can get steps from multiple lines",
  {
    steps <- obj$getSteps(name = "method", list(7L, 17L));
    expect_length(steps, 2L);
    expect_equal(steps[[1L]]$at[1L], "test1:2");
    expect_equal(steps[[2L]]$at[1L], "test2:2");

    steps <- obj$getSteps(name = "method", list(7L, 1L));
    expect_length(steps, 2L);
    expect_null(steps[[2L]]);
  })

  it("can set breakpoints",
  {
    orig <- test1@encl$this$method;
    obj$setBreakpoints("method", "test1", c("2", "3,2"));
    expect_s4_class(test1@encl$this$method, "functionWithTrace");
    expect_false(inherits(test2@encl$this$method, "functionWithTrace"));
    obj$setBreakpoints("method");
    expect_identical(test1@encl$this$method, orig);
  })

})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("OoprBreakpoints",
{
  skip();
  OoprBreakpoints$files$resize();
  tmp <- tempfile(fileext = ".R");
  on.exit(unlink(tmp));
  text <- r"{
  oopr("test1",,
  {
  public:
    method       <- \( )
    {
      1L;
      {
        2L;
      }
    }
  })
  oopr("test2",,
  {
    method <- \( )
    {
      3L;
      {
        4L;
      }
    }
  })
  method <- \( )
  {
    5L;
    {
      6L;
    }
  }
  }";
  cat(text, file = tmp);
  source(tmp, local = FALSE);
  on.exit(rm(test1, test2, envir = globalenv()));

  it("creates a file class when checking sync the first time",
  {
    expect_true(OoprBreakpoints$isFunctionInSync("method", tmp, ""));
    expect_equal(OoprBreakpoints$files$keys, tmp);
  })

  it("obtains steps",
  {
    expect_length(
      OoprBreakpoints$getFunctionSteps("method", tmp, "", list(7L)), 1L
    );
  })

  it("returns an environment",
  {
    expect_identical(
      OoprBreakpoints$getEnvironmentOfFunction("method", tmp, "")
     ,emptyenv()
    );
  })

  it("can set the breakpoint",
  {
    OoprBreakpoints$setFunctionBreakpoints("method", emptyenv(), "test1:2");
    expect_s4_class(test1@encl$this$method, "functionWithTrace");
  })


})
