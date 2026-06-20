## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("isname",
{
  it("knows about symbols",
  {
    expect_false(isname("a", "a"));
    expect_true(isname(quote(a), "a"));
    expect_true(isname(quote(a)));
  })

  it("supports multiple strings",
  {
    expect_false(isname(quote(a), c("b", "b")));
    expect_true(isname(quote(a), c("a", "b")));
    expect_true(isname(quote(a), c("b", "a")));
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("iscall",
{
  it("knows about language",
  {
    expect_false(iscall("a", "a"));
    expect_false(iscall(quote(a), "a"));
    expect_true(iscall(quote(a()), "a"));
  })

  it("supports multiple strings",
  {
    expect_false(iscall(quote(a()), c("b", "b")));
    expect_true(iscall(quote(a()), c("a", "b")));
    expect_true(iscall(quote(a()), c("b", "a")));
  })

  it("can do packages",
  {
    expect_false(iscall(quote(a::b())), "b");
    expect_false(iscall(quote(a::b())), "b", "b");
    expect_true(iscall(quote(a::b()), "b", "a"));
    expect_true(iscall(quote(a:::b()), "b", "a"));
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("symlink",
{
  parent <- new.env();
  target <- new.env(parent = parent);
  parent$target <- target;
  target$a <- 1L;
  link <- new.env();

  it("asserts",
  {
    expect_error(
      symlink(1L, "target", link, "a")
     ,"`tenv` must be an environment"
    );
    expect_error(
      symlink(target, "target", 1L, "a")
     ,"`env` must be an environment"
    );
    expect_error(
      symlink(target, 1L, link, "a")
     ,"`tname` must be a symbol or single character vector"
    );
    expect_error(
      symlink(target, "a", link, "a")
     ,"`tname` does not exist in the parent environment of `tenv`"
    );
    expect_error(
      symlink(target, "target", link, 1L)
     ,"`name` must be a symbol or single character vector"
    );
    expect_error(
      symlink(target, "target", link, "b")
     ,"`name` does not exist in `tenv`"
    );
    link$a <- "a"
    expect_error(
      symlink(target, "target", link, "a")
     ,"`name` already exists in `env`"
    );
    rm(a, envir = link)
  })

  it("creates a reference to another environment",
  {
    symlink(target, "target", link, "a");
    expect_equal(link$a, target$a);
    link$a <- 2L;
    expect_equal(link$a, target$a);
    expect_identical(
      environment(activeBindingFunction("a", link))
     ,parent.env(target)
    );
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("roxy_tag_parse.roxy_tag_intern",
{
  expect_equal(roxy_tag_parse.roxy_tag_intern(1L), 1L);
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("%||%",
{
  expect_equal(1L %||% NULL, 1L);
  expect_equal(NULL %||% 1L, 1L);
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("matchsig",
{
  it("returns error when arguments dont match",
  {
    out <- matchsig(\(){}, quote(a(x)));
    expect_s3_class(out, "error");
    expect_equal(out$message, "unused argument (x)");
  })

  it("returns error when missing non-default argument",
  {
    out <- matchsig(\(x){}, quote(a()));
    expect_s3_class(out, "error");
    expect_equal(out$message, "missing non-default argument (x)");
  })

  it("returns the call when matched",
  {
    out <- matchsig(\(x){}, quote(a(x = 1L)));
    expect_true(is.call(out));
    expect_identical(out, quote(a(x = 1L)));
  })
})
