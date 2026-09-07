## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("evaluate_expr",
{
  it("only allows assignments in top-level",
  {
    expect_error(
      oopr("test",, { a; })
     ,class = "ooprNotATopLevelAssignment"
    );
    expect_error(
      oopr("test",, { a(b); })
     ,class = "ooprNotATopLevelAssignment"
    );
    expect_error(
      oopr("test",, { a <<- 1L; })
     ,class = "ooprNotATopLevelAssignment"
    );
    expect_no_error(
      oopr("test",, { a <- 1L; })
    );
    expect_no_error(
      oopr("test",, { a = 1L; })
    );
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("evaluate_lhs",
{
  it("only accepts `:`",
  {
    expect_error(
      oopr("test",, { a(b) <- 1L})
     ,class = "ooprLHSInvalidCall"
    );
    expect_error(
      oopr("test",, { b:a(b) <- 1L})
     ,class = "ooprLHSInvalidCall"
    );

    expect_no_error(
      oopr("test",, { public:b <- 1L})
    );
    expect_no_error(
      oopr("test",, { public:get:c <- \( ) { }})
    );
  })

  it("accepts `~` only for the name",
  {
    expect_no_error(
      oopr("test",, { ~c <- \( ) { } })
    );
    expect_no_error(
      oopr("test",, { public:~c <- \( ) { } })
    );
    expect_error(
      oopr("test",, { ~a:~c <- \( ) { } })
     ,class = "ooprLHSInvalidCall"
    );
    expect_error(
      oopr("test",, { a:b~c <- \( ) { } })
     ,class = "ooprLHSInvalidCall"
    );
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("evaluate_nme",
{
  it("does not allow hidden names",
  {
    expect_error(
      oopr("test",, { .a <- 1L })
     ,class = "ooprHiddenMember"
    );
  })

  it("does not allow duplicates",
  {
    expect_error(
      oopr("test",, { a <- 1L; a <- 2L; })
     ,class = "ooprDuplicateMember"
    );
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("evaluate_rhs",
{
  it("captures error on evaluation",
  {
    expect_error(
      oopr("test",, { a <- 1/"a"; })
     ,class = "ooprRHSError"
    );
  })

  it("can find variables in the parent environment",
  {
    blah <- 1L
    expect_no_error(
      oopr("test",, { a <- blah; })
    );
  })

  it("enforces a constructor method",
  {
    oopr("test",, { a <- 1L; })
    expect_equal(test@meta$names$get(2L), "test");
    expect_true(test@meta$method$get(2L));
    expect_true(is.function(test@encl$this[["test"]]));
  })

  it("enforces a braced body for construct method",
  {
    oopr("test",, { test <- \( ) 1L; })
    body <- body(test@encl$this$test);
    expect_true(iscall(body, "{"));
    expect_true(!is.null(attr(body, "srcref")));
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("evaluate_src",
{
  it("maintains specifiers in srcref",
  {
    oopr("test",, { static:a <- \( ) { 1L; } })
    src <- attr(test@encl$this$a, "srcref");
    expect_true(startsWith(as.character(src), "static"));
  })

  it("removes access specifier and any comments inbetween",
  {
    oopr("test",,
    {
      public:
        #aaaaa
        a <- \( ) { 1L; }
    })
    src <- attr(test@encl$this$a, "srcref");
    expect_equal(src[1L], src[3L]);
    expect_true(startsWith(as.character(src), "a"));

    oopr("test",, { public:a <- \( ) { 1L; } })
    src <- attr(test@encl$this$a, "srcref");
    expect_equal(src[1L], src[3L]);
    expect_true(startsWith(as.character(src), "a"));
  })
})
