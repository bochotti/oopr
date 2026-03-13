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

  it("supports environments",
  {
    env <- new.env();
    env$a <- specifiers_access;
    env$b <- specifiers_dupes;
    out <- findMemberRefs(env, c("b", "a"));
    expect_length(out, 2L);
    expect_named(out, c("b", "a"));
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
})
