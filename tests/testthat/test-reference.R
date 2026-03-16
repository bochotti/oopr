## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("findMemberRefs",
{
  it("identifies access",
  {
    expr <- quote({
      a$b;
      (a$b);
      fun(a$b, a$b);
      a <- a$b;
    });
    n   <- 5L
    out <- findMemberRefs(expr);
    expect_length(out$at, n);
    lapply(out$at, \(i, x) expect_identical(x[[i]], quote(a$b)), expr);
    expect_equal(out$type, rep("access", n));
    expect_equal(out$oper, rep("$", n));
    expect_equal(out$encl, rep("a", n));
    expect_equal(out$memb, rep("b", n));
    lapply(out$expr, \(x, y) expect_identical(x, y), quote(a$b));
  })

  it("identifies assignment",
  {
    expr <- quote({
      a$b <- 1L;
      a(a$b) <- 1L;
      a <- a$b <- 1L;
      a$b <- a <- 1L;
      a$b[1L] <- 1L
    });
    n   <- 5L
    out <- findMemberRefs(expr);
    expect_length(out$at, n);
    expect_identical(lapply(out$at, \(i, x) x[[i]], expr), as.list(quote({
      a$b <- 1L;
      a(a$b) <- 1L;
      a$b <- 1L;       ## <--
      a$b <- a <- 1L;
      a$b[1L] <- 1L
    }))[-1], ignore_attr = TRUE);
    expect_equal(out$type, rep("assign", n));
    expect_equal(out$oper, rep("$", n));
    expect_equal(out$encl, rep("a", n));
    expect_equal(out$memb, rep("b", n));
  })

  it("identifies calls",
  {
    expr <- quote({
      a$b();
      a(a$b());
      a(b, a$b());
      (a$b)();
      ((a$b))(a);
      {a$b}();
      {{a$b}}();
      {a; a$b}();
    });
    n   <- 8L
    out <- findMemberRefs(expr);
    expr2 <- quote({
      a$b();
      a$b();       ## <--
      a$b();       ## <--
      (a$b)();
      ((a$b))(a);
      {a$b}();
      {{a$b}}();
      {a; a$b}();
    });
    expect_identical(
      lapply(out$at, \(i, x) x[[i]], expr)
     ,as.list(expr2)[-1]
     ,ignore_attr = TRUE
    );
    expect_length(out$at, n);
    expect_equal(out$type, rep("call", n));
    expect_equal(out$oper, rep("$", n));
    expect_equal(out$encl, rep("a", n));
    expect_equal(out$memb, rep("b", n));
  })

  it("doesnt misidentify",
  {
    expr <- quote({
      sum(a, a$b)();
      {a$b; a}
    })
    out <- findMemberRefs(expr);
    expect_equal(out$type, rep("access", 2L))
  })

  it("supports environments/lists",
  {
    env <- new.env();
    env$a <- specifiers_access;
    env$b <- specifiers_dupes;
    env$c <- 1L;
    out <- findMemberRefs(env, c("b", "c", "a"));
    expect_length(out, 3L);
    expect_named(out, c("b", "c", "a"));
    expect_null(out$c)
    list <- as.list(env);
    out <- findMemberRefs(list, c("b", "c", "a"));
    expect_length(out, 3L);
    expect_named(out, c("b", "c", "a"));
    expect_null(out$c);
    expect_null(findMemberRefs(1L));
  })

})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("findSrcRef",
{
  it("gets the srcref",
  {
    expr <- quote({ a$b; })
    attr(expr, "srcref") <- list(NULL, 1:4);
    expect_equal(findSrcRef(2L, expr), 1:4);

    expr <- quote({ a$b; { a$b(a$b); }})
    attr(expr[[c(3L)]], "srcref") <- list(NULL, 1:4);
    expect_equal(findSrcRef(3:2, expr), 1:4);
  })

  it("return NULL when no srcref",
  {
    fun <- \( ) { a$b; }
    attr(fun, "srcref") <- NULL;
    attr(body(fun), "srcref") <- NULL;
    expect_null(findSrcRef(2L, fun));
  })

  it("asserts",
  {
    expect_error(findSrcRef("a", "a"), "`at` must be an integer");
    expect_error(findSrcRef(1L, "a"), "`expr` must be a call object");
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("findInExpr",
{
  it("can find expressions",
  {
    expr <- quote({
      a$b$c$d
      {
        a$b$c$d
      }
    });
    out <- findInExpr(expr, \(e) iscall(e, "$") && isname(e[[2L]], "a"));
    out <- findInExpr(expr, \(e) identical(e, quote(a$b)));
    expect_equal(expr[[out[[1]]]], quote(a$b));
    expect_equal(out, list(c(2,2,2), c(3,2,2,2)));
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("at_lt",
{
  it("knows if an integer position is above another",
  {
    expect_true(at_lt(1L, 2L));
    expect_true(at_lt(1L, 1:2));
    expect_true(at_lt(1:2, c(1, 3)));
    expect_true(at_lt(1:2, 2L));
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("getMissingVars",
{
  it("finds variables not defined",
  {
    expect_equal(getMissingVars(\( ) { a; })$var, "a");
    a <- 1L
    expect_equal(getMissingVars(\( ) { a; })$var, character(0L));
  })

  it("knows about assigning variables",
  {
    expect_equal(getMissingVars(\( ) { a <- 1L; a; })$var, character(0L));
    expect_equal(getMissingVars(\( ) { a; a <- 1L; })$var, "a");
  })

  it("will use the formals of a function",
  {
    expect_equal(getMissingVars(\(a) { a; })$var, character(0L));
  })

  it("considers RHS of subsetting expressions",
  {
    expect_equal(getMissingVars(\(a) { a$b; })$var, character(0L));
    expect_equal(getMissingVars(\(a) { a[[b]]; })$var, "b");
    expect_equal(getMissingVars(\(a) { a[["b"]]; })$var, character(0L));
  })

  it("knows that loops create new variables",
  {
    expect_equal(getMissingVars(\( ) { for(i in 1L) { } })$var, character(0L));
  })

  it("does not collect items inside a function",
  {
    expect_equal(getMissingVars(\( ) { f <- \( ) { a <- 1L; }; a})$var, "a");
  })

})
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("references_exist",
{
  it("doesnt allow referring to members which do not exist",
  {
    expect_error(
      oopr("test",, { a <- \( ) { this$b; } })
     ,class = "ooprRefNotDefined"
    );
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("references_assign",
{
  it("doesnt allow assigning into methods",
  {
    expect_error(
      oopr("test",, { a <- \( ) { }; b <- \( ) { this$a <- 1L; } })
     ,class = "ooprRefBadAssignment"
    );
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("references_call",
{
  it("requires method calls to match definition",
  {
    expect_error(
      oopr("test",, { a <- \( ) { }; b <- \( ) { this$a(1L); } })
     ,class = "ooprRefUnmatchedCall"
    );
    expect_no_error(
      oopr("test",, { a <- \(...) { }; b <- \( ) { this$a(1L); } })
     ,class = "ooprRefUnmatchedCall"
    );
  })

  it("doesnt allow calling non-methods",
  {
    expect_error(
      oopr("test",, { a <- 1L; b <- \( ) { this$a(); } })
     ,class = "ooprRefBadCall"
    );
    expect_error(
      oopr("test",, { get:a <- \( ) { }; b <- \( ) { this$a(); } })
     ,class = "ooprRefBadCall"
    );
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("references_this",
{
  it("doesnt allow assigning into this",
  {
    expect_error(
      oopr("test",, { a <- \( ) { this <- 1L; } })
     ,class = "ooprRefAssigningThis"
    );
  })

  it("doesnt allow calling this",
  {
    expect_error(
      oopr("test",, { a <- \( ) { this(); } })
     ,class = "ooprRefCallingThis"
    );
  })
})
