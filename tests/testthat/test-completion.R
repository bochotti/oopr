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
      a <- list(a = 1, b = list(e = 1, f = 2, g = 3), c = 3);
    protected:
      b <- NULL;
    private:
      c <- NULL;
    })

    oopr("memb2",,
    {
    public:
      b <- memb;
    protected:
      c <- NULL;
    private:
      d <- NULL;
    })

    oopr("memb3",,
    {
    public:
      c <- memb2;
    protected:
      d <- NULL;
    private:
      e <- NULL;
    })

    oopr("test",,
    {
    public:
      method <- \( )
      {
        this$field$c$b$a
      }
    private:
      field  <- memb3;
    })
    }"
    cat(text, file = tmp);
    id <- rstudioapi::documentOpen(tmp, 38, 20, TRUE);
    on.exit2(rstudioapi::documentClose(id, FALSE));
    expect_equal(
      .rs.rpc.get_completions(string = "this$field")$results
     ,c("c")
    );
    expect_equal(
      .rs.rpc.get_completions(string = "this$field$c")$results
     ,c("b")
    );
    expect_equal(
      .rs.rpc.get_completions(string = "this$field$c$b")$results
     ,c("a")
    );
    expect_equal(
      .rs.rpc.get_completions(string = "this$field$c$b$a")$results
     ,c("a", "b", "c")
    );
    expect_equal(
      .rs.rpc.get_completions(string = "this$field$c$b$a$b")$results
     ,c("e", "f", "g")
    );
  })

  it("can complete on static class members",
  {
    text <- r"{
    oopr("memb",,
    {
    public:
      a <- list(a = 1, b = list(e = 1, f = 2, g = 3), c = 3);
    protected:
      b <- NULL;
    private:
      c <- NULL;
    })

    oopr("memb2",,
    {
    public:
      static:b <- memb;
    protected:
      c <- NULL;
    private:
      d <- NULL;
    })

    oopr("memb3",,
    {
    public:
      static:c <- memb2;
    protected:
      d <- NULL;
    private:
      e <- NULL;
    })

    oopr("test",,
    {
    public:
      method <- \( )
      {
        this$field$c$b$a
      }
    private:
      static:field  <- memb3;
    })
    }"
    cat(text, file = tmp);
    id <- rstudioapi::documentOpen(tmp, 38, 20, TRUE);
    on.exit2(rstudioapi::documentClose(id, FALSE));
    expect_equal(
      .rs.rpc.get_completions(string = "this$field")$results
     ,c("c")
    );
    expect_equal(
      .rs.rpc.get_completions(string = "this$field$c")$results
     ,c("b")
    );
    expect_equal(
      .rs.rpc.get_completions(string = "this$field$c$b")$results
     ,c("a")
    );
    expect_equal(
      .rs.rpc.get_completions(string = "this$field$c$b$a")$results
     ,c("a", "b", "c")
    );
    expect_equal(
      .rs.rpc.get_completions(string = "this$field$c$b$a$b")$results
     ,c("e", "f", "g")
    );
  })

  it("can complete on inherited class",
  {
    oopr("memb",,{}); oopr("memb2",,{}); oopr("memb3",,{})
    text <- r"{
    oopr("memb",,
    {
    public:
      a <- list(a = 1, b = list(e = 1, f = 2, g = 3), c = 3);
      b <- \() { this$a; }
    protected:
      c <- NULL;
    private:
      d <- NULL;
    })

    oopr("memb2", public:memb,
    {
    protected:
      e <- 1L;
    })

    oopr("memb3", public:memb2,
    {
    private:
      g <- 2L;
    })

    oopr("test", public:memb3,
    {
    public:
      method <- \( )
      {
        memb3
      }
    private:
      field  <- memb3;
    })
    }"
    cat(text, file = tmp);
    id <- rstudioapi::documentOpen(tmp, 30, 14, TRUE);
    on.exit2(rstudioapi::documentClose(id, FALSE));
    expect_equal(
      .rs.rpc.get_completions(string = "memb3")$results
     ,c("e", "a", "b", "c")
    );
    expect_equal(
      .rs.rpc.get_completions(string = "memb3$a")$results
     ,c("a", "b", "c")
    );
    expect_equal(
      .rs.rpc.get_completions(string = "memb3$a$b")$results
     ,c("e", "f", "g")
    );
    expect_equal(
      .rs.rpc.get_completions(string = "memb2")$results
     ,character(0L)
    );
    expect_equal(
      .rs.rpc.get_completions(string = "this$field")$results
     ,c("a", "b")
    );
  })

  it("can complete on class containers",
  {
    text <- r"{
    oopr("memb",,
    {
    public:
      a <- list(a = 1, b = list(e = 1, f = 2, g = 3), c = 3);
      b <- NULL;
    private:
      c <- NULL;
    })

    oopr("memb2",,
    {
    public:
      b <- memb[];
    protected:
      c <- NULL;
    private:
      d <- NULL;
    })

    oopr("memb3",,
    {
    public:
      c <- memb2[];
    protected:
      d <- NULL;
    private:
      e <- NULL;
    })

    oopr("test",,
    {
    public:
      method <- \( )
      {
        this$field
      }
    private:
      field  <- memb3[];
    })
    }"
    cat(text, file = tmp);
    id <- rstudioapi::documentOpen(tmp, 36, 20, TRUE);
    on.exit2(rstudioapi::documentClose(id, FALSE));
    names <- c(
      "class", "empty", "size", "data", "insert", "emplace", "resize", "erase"
     ,"swap", "apply", "[", "[<-"
    );
    expect_equal(
      .rs.rpc.get_completions(string = "this$field")$results
     ,names
    );
    expect_equal(
      .rs.rpc.get_completions(string = "this$field[1L]")$results
     ,c("c")
    );
    expect_equal(
      .rs.rpc.get_completions(string = "this$field[1L]$c")$results
     ,names
    );
    expect_equal(
      .rs.rpc.get_completions(string = "this$field[1L]$c[1L]")$results
     ,c("b")
    );
    expect_equal(
      .rs.rpc.get_completions(string = "this$field[1L]$c[1L]$b")$results
     ,names
    );
    expect_equal(
      .rs.rpc.get_completions(string = "this$field[1L]$c[1L]$b[1L]")$results
     ,c("a", "b")
    );
    expect_equal(
      .rs.rpc.get_completions(string = "this$field[1L]$c[1L]$b[1L]$a")$results
     ,c("a", "b", "c")
    );
    expect_equal(
      .rs.rpc.get_completions(string = "this$field[1L]$c[1L]$b[1L]$a$b")$results
     ,c("e", "f", "g")
    );
  })

  it("can complete on static class containers",
  {
    text <- r"{
    oopr("memb",,
    {
    public:
      a <- list(a = 1, b = list(e = 1, f = 2, g = 3), c = 3);
      b <- NULL;
    private:
      c <- NULL;
    })

    oopr("memb2",,
    {
    public:
      static:b <- memb[];
    protected:
      c <- NULL;
    private:
      d <- NULL;
    })

    oopr("memb3",,
    {
    public:
      static:c <- memb2[];
    protected:
      d <- NULL;
    private:
      e <- NULL;
    })

    oopr("test",,
    {
    public:
      method <- \( )
      {
        this$field
      }
    private:
      static:field  <- memb3[];
    })
    }"
    cat(text, file = tmp);
    id <- rstudioapi::documentOpen(tmp, 36, 20, TRUE);
    on.exit2(rstudioapi::documentClose(id, FALSE));
    names <- c(
      "class", "empty", "size", "data", "insert", "emplace", "resize", "erase"
     ,"swap", "apply", "[", "[<-"
    );
    expect_equal(
      .rs.rpc.get_completions(string = "this$field")$results
     ,names
    );
    expect_equal(
      .rs.rpc.get_completions(string = "this$field[1L]")$results
     ,c("c")
    );
    expect_equal(
      .rs.rpc.get_completions(string = "this$field[1L]$c")$results
     ,names
    );
    expect_equal(
      .rs.rpc.get_completions(string = "this$field[1L]$c[1L]")$results
     ,c("b")
    );
    expect_equal(
      .rs.rpc.get_completions(string = "this$field[1L]$c[1L]$b")$results
     ,names
    );
    expect_equal(
      .rs.rpc.get_completions(string = "this$field[1L]$c[1L]$b[1L]")$results
     ,c("a", "b")
    );
    expect_equal(
      .rs.rpc.get_completions(string = "this$field[1L]$c[1L]$b[1L]$a")$results
     ,c("a", "b", "c")
    );
    expect_equal(
      .rs.rpc.get_completions(string = "this$field[1L]$c[1L]$b[1L]$a$b")$results
     ,c("e", "f", "g")
    );
  })

})
