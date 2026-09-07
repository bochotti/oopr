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
    expect_false(iscall(str2lang("a::b()")), "b");
    expect_false(iscall(str2lang("a::b()")), "b", "b");
    expect_true(iscall(str2lang("a::b()"), "b", "a"));
    expect_true(iscall(str2lang("a::b()"), "b", "a"));
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("symlink",
{
  it("asserts",
  {
    parent <- new.env();
    target <- new.env(parent = parent);
    assign("target", target, envir = parent);
    assign("afield", 1L, envir = target);
    link <- new.env();

    expect_error(
      symlink(1L, "target", link, "afield")
     ,"`tenv` must be an environment"
    );
    expect_error(
      symlink(target, "target", 1L, "afield")
     ,"`env` must be an environment"
    );
    expect_error(
      symlink(target, 1L, link, "afield")
     ,"`tname` must be a symbol or single character vector"
    );
    expect_error(
      symlink(target, "afield", link, "afield")
     ,"`tname` does not exist in the parent environment of `tenv`"
    );
    expect_error(
      symlink(target, "target", link, 1L)
     ,"`name` must be a symbol or single character vector"
    );
    expect_error(
      symlink(target, "target", link, "bfield")
     ,"`name` does not exist in `tenv`"
    );
    assign("afield", "a", envir = link);
    expect_error(
      symlink(target, "target", link, "afield")
     ,"`name` already exists in `env`"
    );
    rm(list = "afield", envir = link)
  })

  it("creates a reference to another environment",
  {
    parent <- new.env();
    target <- new.env(parent = parent);
    assign("target", target, envir = parent);
    assign("cfield", 1L, envir = target);
    link <- new.env();

    symlink(target, "target", link, "cfield");
    expect_equal(link$cfield, target$cfield);
    link$cfield <- 2L;
    expect_equal(link$cfield, target$cfield);
    expect_identical(
      environment(activeBindingFunction("cfield", link))
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
