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
    expect_equal(obj$col, 12L);
    expect_no_error(obj$eval(new.env()));
    expect_identical(body(obj$obj@encl$this$method)[[2L]], quote(c));
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
    expect_identical(body(obj$obj@encl$this$method)[[2L]], quote(c$m));
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
    expect_equal(obj$col, 21L);
    expect_no_error(obj$eval(new.env()));
    expect_identical(body(obj$obj@encl$this$method)[[2:3]], quote(c));
  })

  text <- r"{
  oopr("test",,
  {
  public:
    method <- \( )
    {
      for(i in this$) {
      }
    }
  })
  }";
  cat(text, file = tmp);
  obj <- OoprSourceTry(tmp, row = 7, col = 21);
  expect_no_error(obj$parse())

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
    expect_identical(body(obj$obj@encl$this$method)[[2]], quote(c()));
  })

})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("eval_context",
{

  it("matches evaluation string",
  {
    text <- "this$a$b['c']$d(e = f$g)$h$`=`$k";
    ctx <- \(text, row, col) .Call(Cpp_eval_context, text, row, col)[[1L]];
    expect_equal(ctx(text, 1L, 6L)$ctx, "this");
    expect_equal(ctx(text, 1L, 8L)$ctx, "this$a");
    expect_equal(ctx(text, 1L, 15L)$ctx, "this$a$b['c']");
    expect_equal(ctx(text, 1L, 23L)$ctx, "f");
    expect_equal(ctx(text, 1L, 26L)$ctx, "this$a$b['c']$d(e = f$g)");
    expect_equal(ctx(text, 1L, 32L)$ctx, "this$a$b['c']$d(e = f$g)$h$`=`");
  })

  it("can match over lines",
  {
    ctx <- \(text, row, col) .Call(Cpp_eval_context, text, row, col)[[1L]];
    text <- "
this$a[[b]](a = c$d)$e$f$
  g$
  "
    expect_equal(ctx(text, 3L, 5L)$ctx, sub("\\$$", "",trimws(text)));

    text <- "
this$a[[b]](a = c$d)$e$f$
  #)
  g$
  "
    expect_equal(ctx(text, 4L, 5L)$ctx, sub("\\$$", "",trimws(text)));

    text <- "aa
this$a[[b]](a = c$d)$e$f$
  `)`$
  g$
  "
    expect_equal(ctx(text, 4L, 5L)$ctx, gsub("^aa\n|\\$$", "",trimws(text)));

    text <- "
this$a[[b]](a = c$d)$e$f$
  ')'$
  g$
  "
    expect_equal(ctx(text, 4L, 5L)$ctx, sub("\\$$", "",trimws(text)));
  })

  it("can complete argument names",
  {
    text <- "this$a(,b = c$d)";
    ctx <- \(text, row, col) .Call(Cpp_eval_context, text, row, col);
    out <- ctx(text, 1L, 8L);
    expect_length(out, 2L);
    expect_null(out[[1L]]);
    expect_equal(out[[2L]]$ctx, "this$a");
    ctx(text, 1L, 14L)
  })

  it("can recursively find calling functions",
  {
    ctx  <- \(text, row, col) .Call(Cpp_eval_context, text, row, col);
    text <- "this$a(a = c(1, 2), b = this$d(e = this$g$h))";
    out  <- ctx(text, 1L, 43L);
    expect_length(out, 3L);
    expect_equal(out[[1L]]$ctx, "this$g");
    expect_equal(out[[2L]]$ctx, "this$d");
    expect_equal(out[[3L]]$ctx, "this$a");
  })


  it("can find calling functions over lines",
  {
    ctx  <- \(text, row, col) .Call(Cpp_eval_context, text, row, col);
    text <- "this$a(
   a = c(1, 2)
  ,b = this$d(e = this$g$h)
    )";
    expect_equal(ctx(text, 1L, 8L)[[2]]$ctx, "this$a");
    expect_equal(ctx(text, 3L, 4L)[[2]]$ctx, "this$a");
    out <- ctx(text, 3L, 13L); substr(text, 1, 23L + 13L);
    expect_length(out, 2L);
    expect_equal(out[[1L]]$ctx, "this");
    expect_equal(out[[2L]]$ctx, "this$a");
    out <- ctx(text, 3L, 15L); substr(text, 1, 23L + 15L);
    expect_length(out, 3L);
    expect_null(out[[1L]]);
    expect_equal(out[[2L]]$ctx, "this$d");
    expect_equal(out[[3L]]$ctx, "this$a");
    out <- ctx(text, 3L, 25L); substr(text, 1, 23L + 25L)
    expect_equal(out[[1L]]$ctx, "this");
    expect_equal(out[[2L]]$ctx, "this$d");
    expect_equal(out[[3L]]$ctx, "this$a");
  })

  "return(list(fun = steps, class = class));"
  text <- r"{
  = ;
  for (i in this$)
  }"
  .Call(Cpp_eval_context, text, 3L, 18L)
})
