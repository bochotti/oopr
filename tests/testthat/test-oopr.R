## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("oopr asserts",
{
  it("must have a single character for name",
  {
    expect_error(oopr(1L));
    expect_error(oopr(letters));
    expect_error(oopr(NA_character_));
    expect_error(oopr(""));
    expect_no_error(oopr("class",,{}));
  })

  it("must have a { enclosure for definition",
  {
    expect_error(oopr("test"));
    expect_error(oopr("test",,a));
    expect_no_error(oopr("test",,{ a <- 1L; }));
  })

  it("must have an environment for a parent",
  {
    expect_error(oopr("class",,{}, parent = NULL));
    expect_no_error(oopr("class",,{}, parent = emptyenv()));
  })

})
