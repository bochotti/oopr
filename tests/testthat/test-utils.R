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
    expect_false(isname(quote(a), c("b", "b")));
    expect_true(isname(quote(a), c("a", "b")));
    expect_true(isname(quote(a), c("b", "a")));
  })
})

test_that("benchmark",
{
  skip("Benchmarking");
  isname2 <- \(x, names) is.name(x) && !is.na(match(as.character(x), names))
  microbenchmark::microbenchmark(
    isname2(quote(a), "a")
   ,isname(quote(a), "a")
   ,check = "equal"
  );
})
