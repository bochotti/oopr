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

  it("can push, peek & pop",
  {
    vec <- vector("integer", 2L);
    expect_no_error(vec$push(3L));
    expect_equal(vec$size, 3L);
    expect_equal(vec$peek(), 3L);
    expect_no_error(vec$pop())
    expect_equal(vec$size, 2L);
    expect_equal(vec$peek(), 0L);
  })

  it("prints underlying data",
  {
    vec <- vector("integer", 2L);
    expect_output(print(vec), capture.output(print(vec$data)), fixed = TRUE);
  })

})
