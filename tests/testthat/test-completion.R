## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("OoprCompletion",
{
  oopr("OoprCompletionTest", public:OoprCompletionSource,
  {
  public:
    isAvailable <- \( )
    {
      if(is.null(get_in_stack(".DollarNames"))) return(FALSE);
      this$load(globalenv(), this$file, NULL, this$row, this$col);
      return(TRUE);
    }
  })
  old <- OoprCompletion$source;
  OoprCompletion$source <- OoprCompletionTest();
  on.exit(OoprCompletion$source <- old);

  tmp <- tempfile(fileext = ".R");
  on.exit2(unlink(tmp));

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  it("returns names for simple `this$` call",
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
    OoprCompletion$source$file <- tmp;
    OoprCompletion$source$row  <- 7L;
    OoprCompletion$source$col  <- 14L;
    expect_equal(.DollarNames(this), c("method", "field"), ignore_attr = TRUE);
  })

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  it("returns names for a class member",
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
        this$field$c$b$a$b$z
      }
    private:
      field  <- memb3;
    })
    }"
    cat(text, file = tmp);
    OoprCompletion$source$file <- tmp;
    OoprCompletion$source$row  <- 37L;

    OoprCompletion$source$col  <- 20L;
    expect_equal(.DollarNames(this$field), c("c"), ignore_attr = TRUE);

    OoprCompletion$source$col  <- 22L;
    expect_equal(.DollarNames(this$field$c), c("b"), ignore_attr = TRUE);

    OoprCompletion$source$col  <- 24L;
    expect_equal(.DollarNames(this$field$c$b), c("a"), ignore_attr = TRUE);

    OoprCompletion$source$col  <- 26L;
    expect_equal(
      .DollarNames(this$field$c$b$a), c("a", "b", "c"), ignore_attr = TRUE
    );

    OoprCompletion$source$col  <- 28L;
    expect_equal(
      .DollarNames(this$field$c$b$a$b), c("e", "f", "g"), ignore_attr = TRUE
    );

  })

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  it("returns names for inherited class",
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
        memb3$a$b$z
        memb2$z
        this$field$a
      }
    private:
      field  <- memb3;
    })
    }"
    cat(text, file = tmp);
    OoprCompletion$source$file <- tmp;
    OoprCompletion$source$row  <- 30L;

    OoprCompletion$source$col  <- 15L;
    expect_equal(
      .DollarNames(memb3), c("e", "a", "b", "c"), ignore_attr = TRUE
    );

    OoprCompletion$source$col  <- 17L;
    expect_equal(
      .DollarNames(memb3$a), c("a", "b", "c"), ignore_attr = TRUE
    );

    OoprCompletion$source$col  <- 19L;
    expect_equal(
      .DollarNames(memb3$a$b), c("e", "f", "g"), ignore_attr = TRUE
    );

    OoprCompletion$source$row  <- 31L;
    OoprCompletion$source$col  <- 15L;
    expect_equal(
      .DollarNames(memb2), character(0L), ignore_attr = TRUE
    );

    OoprCompletion$source$row  <- 32L;
    OoprCompletion$source$col  <- 14L;
    expect_equal(
      .DollarNames(this), c("method", "field", "e", "a", "b", "c")
     ,ignore_attr = TRUE
    );

    OoprCompletion$source$col  <- 20L;
    expect_equal(
      .DollarNames(this$field), c("a", "b"), ignore_attr = TRUE
    );
  })

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  it("returns names for class containers",
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
        this$field$size
        this$field[1L]$c[1L]$b[1L]$a$b$z
      }
    private:
      field  <- memb3[];
    })
    }"
    cat(text, file = tmp);
    OoprCompletion$source$file <- tmp;
    OoprCompletion$source$row  <- 36L;

    OoprCompletion$source$col  <- 20L;
    names <- c(
      "class", "empty", "size", "data", "insert", "emplace", "resize", "erase"
     ,"swap", "apply", "[", "[<-"
    );
    expect_equal(
      .DollarNames(this$field), names, ignore_attr = TRUE
    );

    OoprCompletion$source$row  <- 37L;
    OoprCompletion$source$col  <- 24L;
    expect_equal(
      .DollarNames(this$field[1L]), c("c"), ignore_attr = TRUE
    );

    OoprCompletion$source$col  <- 30L;
    expect_equal(
      .DollarNames(this$field[1L]$c[1L]), c("b"), ignore_attr = TRUE
    );

    OoprCompletion$source$col  <- 36L;
    expect_equal(
      .DollarNames(this$field[1L]$c[1L]$b[1L]), c("a", "b"), ignore_attr = TRUE
    );

    OoprCompletion$source$col  <- 38L;
    expect_equal(
      .DollarNames(this$field[1L]$c[1L]$b[1L]$a), c("a", "b", "c")
     ,ignore_attr = TRUE
    );

    OoprCompletion$source$col  <- 40L;
    expect_equal(
      .DollarNames(this$field[1L]$c[1L]$b[1L]$a$b), c("e", "f", "g")
     ,ignore_attr = TRUE
    );
  })

})
