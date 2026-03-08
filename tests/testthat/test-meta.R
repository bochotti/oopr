## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("vector",
{
  it("provides the size of the data",
  {
    vec <- vector("logical", 2L);
    expect_equal(vec$size, 2L);
    expect_no_error(vec$push(FALSE));
    expect_equal(vec$size, 3L);
  })

  it("can get and set",
  {
    vec <- vector("integer", 2L);
    expect_equal(vec$get(1L), 0L);
    expect_no_error(vec$set(1L, 1L));
    expect_equal(vec$get(1L), 1L);
  })

  it("can push",
  {
    vec <- vector("integer", 2L);
    expect_no_error(vec$push(3L));
    expect_equal(vec$size, 3L);
    expect_equal(vec$get(vec$size), 3L);
  })

  it("can perform a subset",
  {
    vec <- vector("integer", 0L);
    vec$push(c(1:10));
    expect_true(is.logical(vec$subs(2L)));
    expect_equal(which(vec$subs(2L)), 2L);
    expect_equal(which(vec$subs(c(2L, 4L))), c(2L, 4L));
  })

  it("can remove a record",
  {
    vec <- vector("integer", 0L);
    vec$push(c(1:10));
    vec$rmve(1);
    expect_equal(vec$data, 2:10);
    vec$rmve(rep(c(FALSE, TRUE), c(8, 1)));
    expect_equal(vec$data, 2:9);
  })

  it("prints underlying data",
  {
    vec <- vector("integer", 2L);
    expect_output(print(vec), capture.output(print(vec$data)), fixed = TRUE);
  })

  it("can be locked",
  {
    vec <- vector("integer", 0L);
    vec$lock();
    expect_setequal(names(vec), c("subs", "get", "size", "data"));
    expect_true(bindingIsLocked("data", vec));
    expect_true(environmentIsLocked(vec))
  })

})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("meta",
{
  meta <- meta();

  it("can be pushed into",
  {
    meta$push(names = 'a');
    expect_equal(meta$size, 1L);
    expect_equal(meta$names$get(1L), 'a');
  })

  it("can have a record removed",
  {
    meta$rmve(1L);
    expect_equal(meta$size, 0L);
    expect_equal(meta$names$get(1L), NA_character_);
  })

  it("can perform a subset",
  {
    meta$push(names = 'a', access = "public");
    meta$push(names = 'b', access = "public", method = TRUE);
    meta$push(names = 'c', access = "protected", property = TRUE);
    meta$push(names = 'd', access = "protected", static = TRUE);
    meta$push(names = 'e', access = "private", property = TRUE);
    meta$push(names = 'f', access = "private", static = TRUE);
    expect_equal(which(meta$subs(access = "public")), 1:2);
    expect_equal(which(meta$subs(method = TRUE)), 2);
    expect_equal(which(meta$subs(static = TRUE)), c(4, 6));
    expect_equal(meta$subs("names", property = "TRUE"), c('c', 'e'))
    expect_equal(meta$subs("names", TRUE, method = FALSE), 'b')
  })

  it("can be locked",
  {
    meta$lock();
    expect_disjoint(names(meta), c("push", "rmve", "lock"));
    expect_true(environmentIsLocked(meta));
    expect_true(bindingIsLocked("access", meta));
    expect_true(environmentIsLocked(meta$access));
    expect_true(bindingIsLocked("data", meta$access));
  })

})
