## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("find_member_refs",
{
  find_member_refs <- \(x) .Call(Cpp_find_member_refs, x)
  it("identifies access",
  {
    expr <- quote({
      a$b;
      (a$b);
      fun(a$b, a$b);
      a <- a$b;
    });
    n   <- 5L
    out <- find_member_refs(expr);
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
    out <- find_member_refs(expr);
    expect_length(out$at, n);
    lapply(out$at, \(i, x) expect_identical(x[[i]], quote(a$b)), expr);
    expect_equal(out$type, rep("assign", n));
    expect_equal(out$oper, rep("$", n));
    expect_equal(out$encl, rep("a", n));
    expect_equal(out$memb, rep("b", n));
    expect_identical(do.call(call, c('{', out$expr), TRUE), quote({
      a$b <- 1L;
      a(a$b) <- 1L;
      a$b <- 1L;       ## <--
      a$b <- a <- 1L;
      a$b[1L] <- 1L
    }));
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
    out <- find_member_refs(expr);
    expect_length(out$at, n);
    lapply(out$at, \(i, x) expect_identical(x[[i]], quote(a$b)), expr);
    expect_equal(out$type, rep("call", n));
    expect_equal(out$oper, rep("$", n));
    expect_equal(out$encl, rep("a", n));
    expect_equal(out$memb, rep("b", n));
    expr2 <- quote({
      a$b();
      a$b();       ## <--
      a$b();       ## <--
      (a$b)();
      ((a$b))(a);
      {a$b}();
      {{a$b}}();
      {a; a$b}();
    })
    expect_identical(do.call(call, c('{', out$expr), TRUE), expr2);
  })

  it("more $ includes `<-` and `()` in expr",
  {
    expr <- quote({
      a$b$c;
      a$b$c$d
      a$b$c   <- 1L;
      a$b$c$d <- 1L;
      a$b$c();
      a$b$c$d();
    });
    invisible(find_member_refs(quote({a$b$c$d})))
    n   <- 6
    out <- find_member_refs(expr);
    expect_length(out$at, n);
    lapply(out$at, \(i, x) expect_identical(x[[i]], quote(a$b)), expr);
    expect_equal(out$type, rep(c("access", "assign", "access"), c(2,2,2)));
    expect_equal(out$oper, rep("$", n));
    expect_equal(out$encl, rep("a", n));
    expect_equal(out$memb, rep("b", n));
    expect_identical(do.call(call, c('{', out$expr), TRUE), expr);
  })

  it("doesnt misidentify",
  {
    expr <- quote({
      sum(a, a$b)();
      sum(a$b$c)
      {a$b; a}
    })
    out <- find_member_refs(expr);
    expect_equal(out$type, rep("access", 3L))
    expect_identical(out$expr, list(quote(a$b), quote(a$b$c), quote(a$b)));
  })

  it("supports environments/lists",
  {
    env <- new.env();
    env$a <- specifiers_access;
    env$b <- specifiers_dupes;
    env$c <- 1L;
    out <- find_member_refs(env);
    expect_length(out, 3L);
    expect_named(out, c("a", "b", "c"));
    expect_null(out$c)
    list <- as.list(env);
    out <- find_member_refs(list);
    expect_length(out, 3L);
    expect_named(out, c("a", "b", "c"));
    expect_null(out$c);
    expect_null(find_member_refs(1L));
  })

})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("find_src_ref",
{
  it("gets the srcref",
  {
    expr <- quote({ a$b; })
    attr(expr, "srcref") <- list(NULL, 1:4);
    expect_equal(find_src_ref(2L, expr), 1:4);

    expr <- quote({ a$b; { a$b(a$b); }})
    attr(expr[[c(3L)]], "srcref") <- list(NULL, 1:4);
    expect_equal(find_src_ref(3:2, expr), 1:4);
  })

  it("return NULL when no srcref",
  {
    fun <- \( ) { a$b; }
    attr(fun, "srcref") <- NULL;
    attr(body(fun), "srcref") <- NULL;
    expect_null(find_src_ref(2L, fun));
  })

  it("asserts",
  {
    expect_error(find_src_ref("a", "a"), "`at` must be an integer");
    expect_error(find_src_ref(1L, "a"), "`expr` must be a call object");
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
test_that("get_missing_vars",
{
  get_missing_vars <- \(x) .Call(Cpp_get_missing_vars, x, globalenv())
  it("finds variables not defined",
  {
    expect_equal(get_missing_vars(\( ) { a; })$var, "a");
    a <- 1L
    expect_equal(get_missing_vars(\( ) { a; })$var, character(0L));
  })

  it("knows about assigning variables",
  {
    expect_equal(get_missing_vars(\( ) { a <- 1L; a; })$var, character(0L));
    expect_equal(get_missing_vars(\( ) { a; a <- 1L; })$var, "a");
  })

  it("will use the formals of a function",
  {
    expect_equal(get_missing_vars(\(a) { a; })$var, character(0L));
  })

  it("considers RHS of subsetting expressions",
  {
    expect_equal(get_missing_vars(\(a) { a$b; })$var, character(0L));
    expect_equal(get_missing_vars(\(a) { a[[b]]; })$var, "b");
    expect_equal(get_missing_vars(\(a) { a[["b"]]; })$var, character(0L));
  })

  it("knows that loops create new variables",
  {
    expect_equal(get_missing_vars(\( ) { for(i in 1L) {} })$var, character(0L));
  })

  it("does not collect items inside a function",
  {
    expect_equal(get_missing_vars(\( ) { f <- \( ) { a <- 1L; }; a})$var, "a");
  })

  it("ignores symbols inside quote",
  {
    expect_length(get_missing_vars(\( ) { quote(.); })$var, 0L)
    expect_length(get_missing_vars(\( ) { base::quote(.); })$var, 0L)
    expect_length(get_missing_vars(\( ) { substitute(.); })$var, 0L)
    expect_length(get_missing_vars(\( ) { base::substitute(.); })$var, 0L)
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
