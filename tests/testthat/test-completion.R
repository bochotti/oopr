## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("OoprSourceContext",
{
  tmp <- tempfile();
  on.exit(rm(tmp));

  text <- r"{
  oopr("test",,
  {
  public:
    method <- \( )
    {
      return(1L);
    }
  })
  }"
  cat(text, file = tmp);
  obj <- OoprSourceContext(file = tmp);

  it("collects the objects defined in the file",
  {
    expect_equal(obj$file, tmp);
    expect_true(is.call(obj$defs[[1L]]));
    expect_equal(
      paste(collapse = "\n", capture.output(obj$defs[[1L]]))
     ,regmatches(text, regexpr("(?<c>\\{([^{}]|(?&c))*\\})", text, perl = TRUE))
    );
  })

  it("can pull the ooprC from position",
  {
    expect_error(obj$getByPos(1));
    expect_error(obj$getByPos(3, 2));
    expect_true(is.ooprC(obj$getByPos(3, 3), "test"));
    expect_true(is.ooprC(obj$getByPos(9, 3), "test"));
    expect_error(obj$getByPos(9, 4));
    expect_error(obj$getByPos(10));
  })

  text <- r"{
  oopr("test",,
  {
  public:
    method <- \( )
    {
      return(1L);
    }
  })
  {
    oopr("test2", test,
    {
    public:
      method2 <- \( )
      {
        return(1L);
      }
    })
  }
  }"
  cat(text, file = tmp);
  obj <- OoprSourceContext(file = tmp);

  it("can collect more than one class",
  {
    expect_length(obj$defs, 2L);
    expect_true(is.ooprC(obj$getByPos(9, 1), "test"));
    expect_error(obj$getByPos(10));
    expect_error(obj$getByPos(12, 4));
    expect_true(is.ooprC(obj$getByPos(12, 5), "test2"));
    expect_true(is.ooprC(obj$getByPos(18, 5), "test2"));
    expect_error(obj$getByPos(18, 6));
  })

})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("OoprCompletion",
{
  skip_if_not(identical(.Platform$GUI, "RStudio"));
  skip_if_not_installed("rstudioapi");

  .rs.rpc.get_completions <- \(
    token      = ""
   ,string     = "this"
   ,envir      = parent.frame()
   ,documentId = get("id", envir = envir)
  )
  {
    .rs.getCompletionsDollar(token, string, NULL, envir, FALSE);
  }

  id <- rstudioapi::getSourceEditorContext()$path;
  on.exit2(rstudioapi::documentOpen(id))
  tmp <- tempfile(fileext = ".R");
  on.exit2(unlink(tmp));

  it("returns all names within the class",
  {
    text <- r"{
    oopr("test",,
    {
    public:
      method <- \( )
      {
        this$
      }
    private:
      field  <- 1L;
    })
    }"
    cat(text, file = tmp);
    id <- rstudioapi::documentOpen(tmp, 7, 14, TRUE);
    on.exit2(rstudioapi::documentClose(id, FALSE));
    expect_equal(.rs.rpc.get_completions()$results, c("method", "field"));
  })

  # it("places the members inside `this`",
  # {
  #   expect_named(this, c("method", "field"));
  #   expect_identical(
  #     .subset2(this, "method"), \( ) { this; },
  #    ,ignore_attr = TRUE, ignore_function_env = TRUE
  #   );
  #   expect_equal(.subset2(this, "field"), 1L);
  # })

  it("can complete partial names",
  {
    text <- r"{
    oopr("test",,
    {
    public:
      method <- \( )
      {
        this$f
      }
    private:
      field  <- 1L;
    })
    }"
    cat(text, file = tmp);
    id <- rstudioapi::documentOpen(tmp, 7, 15, TRUE);
    on.exit2(rstudioapi::documentClose(id, FALSE));
    expect_equal(.rs.rpc.get_completions("f")$results, c("field"));
  })

  it("can complete when part of control flow",
  {
    text <- r"{
    oopr("test",,
    {
    public:
      method <- \( )
      {
        for(i in this$)
      }
    private:
      field  <- 1L;
    })
    }"
    cat(text, file = tmp);
    id <- rstudioapi::documentOpen(tmp, 7, 23, TRUE);
    on.exit2(rstudioapi::documentClose(id, FALSE));
    expect_equal(.rs.rpc.get_completions()$results, c("method", "field"));
  })

  it("can complete on members",
  {
    text <- r"{
    oopr("test",,
    {
    public:
      method <- \( )
      {
        this$field$
      }
    private:
      field  <- list(a = 1, b = 2, c = 3);
    })
    }"
    cat(text, file = tmp);
    id <- rstudioapi::documentOpen(tmp, 7, 20, TRUE);
    on.exit2(rstudioapi::documentClose(id, FALSE));
    expect_equal(
      .rs.rpc.get_completions(string = "this$field")$results
     ,c("a", "b", "c")
    );
  })

  it("can complete on nested members",
  {
    text <- r"{
    oopr("test",,
    {
    public:
      method <- \( )
      {
        this$field$b$
      }
    private:
      field  <- list(a = 1, b = list(a = "a", b = "b"), c = 3);
    })
    }"
    cat(text, file = tmp);
    id <- rstudioapi::documentOpen(tmp, 7, 22, TRUE);
    on.exit2(rstudioapi::documentClose(id, FALSE));
    expect_equal(
      .rs.rpc.get_completions(string = "this$field$b")$results
     ,c("a", "b")
    );
  })

  it("can complete on class members",
  {
    text <- r"{
    oopr("memb",,
    {
    public:
      a <- list(a = 1, b = 2, c = 3);
    })

    oopr("test",,
    {
    public:
      method <- \( )
      {
        this$field$
      }
    private:
      field  <- memb;
    })
    }"
    cat(text, file = tmp);
    id <- rstudioapi::documentOpen(tmp, 13, 20, TRUE);
    on.exit2(rstudioapi::documentClose(id, FALSE));
    expect_equal(
      .rs.rpc.get_completions(string = "this$field")$results
     ,c("a")
    );
  })

  it("can complete on class members members",
  {
    text <- r"{
    oopr("memb",,
    {
    public:
      a <- list(a = 1, b = 2, c = 3);
    })

    oopr("test",,
    {
    public:
      method <- \( )
      {
        this$field$a$
      }
    private:
      field  <- memb;
    })
    }"
    cat(text, file = tmp);
    id <- rstudioapi::documentOpen(tmp, 13, 22, TRUE);
    on.exit2(rstudioapi::documentClose(id, FALSE));
    expect_equal(
      .rs.rpc.get_completions(string = "this$field$a")$results
     ,c("a", "b", "c")
    );
  })

  it("can complete on inherited class",
  {
    oopr("memb",,{})
    text <- r"{
    oopr("memb",,
    {
    public:
      a <- list(a = 1, b = list(a = 1, b = 2, c = 3), c = 3);
    protected:
      b <- NULL;
    private:
      c <- NULL;
    })

    oopr("test", memb,
    {
    public:
      method <- \( )
      {
        memb$
      }
    private:
      field  <- memb;
    })
    }"
    cat(text, file = tmp);
    id <- rstudioapi::documentOpen(tmp, 17, 14, TRUE);
    on.exit2(rstudioapi::documentClose(id, FALSE));
    expect_equal(
      .rs.rpc.get_completions(string = "memb")$results
     ,c("a", "b")
    );
  })

  it("can complete on inherited class members",
  {
    oopr("memb",,{})
    text <- r"{
    oopr("memb",,
    {
    public:
      a <- list(a = 1, b = list(a = 1, b = 2, c = 3), c = 3);
    protected:
      b <- NULL;
    private:
      c <- NULL;
    })

    oopr("test", memb,
    {
    public:
      method <- \( )
      {
        memb$a$
      }
    private:
      field  <- memb;
    })
    }"
    cat(text, file = tmp);
    id <- rstudioapi::documentOpen(tmp, 17, 16, TRUE);
    on.exit2(rstudioapi::documentClose(id, FALSE));
    expect_equal(
      .rs.rpc.get_completions(string = "memb$a")$results
     ,c("a", "b", "c")
    );
  })
})
