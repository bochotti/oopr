## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("OoprSource",
{
  tmp <- tempfile();
  on.exit(rm(tmp));

  text <- r"{
  oopr("A",,
  {
  public:
    method <- \( )
    {
      return(1L);
    }
  })

  {
    oopr("B",,
    {
    public:
      method <- \( )
      {
        return(1L);
      }
    })
  }
  }";
  cat(text, file = tmp);

  obj <- OoprSource();
  obj$file <- tmp;

  it("can parse a file",
  {
    obj$parse();
    expect_length(obj$expr, 2L);
    expect_length(obj$defs, 2L);
  })

  it("can evaluate",
  {
    env <- new.env();
    obj$eval(env);
    expect_length(env, 0L);
    expect_length(obj$objs, 2L);
    expect_true(is.ooprC(obj$objs[[1L]]));
    expect_true(is.ooprC(obj$objs[[2L]]));
  })

  obj <- OoprSource();
  obj$file <- tmp;

  it("stops eval where row is",
  {
    env <- new.env();
    obj$row <- 4L;
    obj$col <- 1L;
    obj$parse();
    obj$eval(env);
    expect_null(obj$objs[[2L]]);
    expect_true(is.ooprC(obj$obj, "A"));
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("OoprSourceTry",
{
  tmp <- tempfile();
  on.exit(rm(tmp));
  text <- r"{
  oopr("test",,
  {
  public:
    method <- \( )
    {
      this$
    }
  })
  }";
  cat(text, file = tmp);

  obj <- OoprSourceTry(tmp, row = 7, col = 12);

  it("can parse when $ at end of line",
  {
    expect_no_error(obj$parse());
    expect_false(endsWith(obj$text[7L], "$"));
    expect_equal(obj$col, 11L);
    expect_no_error(obj$eval(new.env()));
    expect_identical(body(obj$obj@encl$this$method)[[2L]], quote(list));
  })

  text <- r"{
  oopr("test",,
  {
  public:
    method <- \( )
    {
      this$m
    }
  })
  }";
  cat(text, file = tmp);
  obj <- OoprSourceTry(tmp, row = 7, col = 13);
  it("does not remove $ when not at end of line",
  {
    expect_no_error(obj$parse());
    expect_true(endsWith(obj$text[7L], "m"));
    expect_equal(obj$col, 13L);
    expect_no_error(obj$eval(new.env()));
    expect_identical(body(obj$obj@encl$this$method)[[2L]], quote(list$m));
  })

  text <- r"{
  oopr("test",,
  {
  public:
    method <- \( )
    {
      for(i in this$)
    }
  })
  }";
  cat(text, file = tmp);
  obj <- OoprSourceTry(tmp, row = 7, col = 21);
  it("parses with incomplete control-flow",
  {
    expect_no_error(obj$parse());
    expect_true(endsWith(obj$text[7L], "}"));
    expect_equal(obj$col, 20L);
    expect_no_error(obj$eval(new.env()));
    expect_identical(body(obj$obj@encl$this$method)[[2:3]], quote(list));
  })

  text <- r"{
  oopr("test",,
  {
  public:
    method <- \(x)
    {
      this$method()
    }
  })
  }";
  cat(text, file = tmp);
  obj <- OoprSourceTry(tmp, row = 7, col = 19);
  it("evaluates with no args",
  {
    expect_no_error(obj$parse());
    expect_equal(obj$col, 19L);
    expect_no_error(obj$eval(new.env()));
    expect_identical(body(obj$obj@encl$this$method)[[2]], quote(list$method()));
  })

})
