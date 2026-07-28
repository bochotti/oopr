## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("OoprRoxySection",
{
  obj <- OoprRoxySection("title");

  it("has a title",
  {
    expect_equal(obj$title, "title");
  })

  it("can insert content",
  {
    obj$insert("content1");
    expect_equal(obj$content, "content1");
  })

  it("can convert to Rd format",
  {
    expect_match(obj$toRd(), "\\\\subsection");
  })

  it("can erase content",
  {
    obj$erase();
    expect_length(obj$content, 0L);
  })
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
}) ## OoprRoxySection
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("OoprRoxyDescribe",
{
  obj <- OoprRoxyDescribe();

  it("has title",
  {
    expect_equal(obj$title, "Fields");
  })

  it("inserts field tag",
  {
    tag <- roxygen2::roxy_tag_parse(roxygen2::roxy_tag("field", "name val"));
    obj$insert(tag);
    expect_identical(obj$content, c(name = "val"));
  })

  it("creates describe list",
  {
    rd <- obj$toRd();
    expect_match(rd, "\\\\describe");
    expect_match(rd, "\\\\item");
  })
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
}) ## OoprRoxyDescribe
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("OoprRoxyUsage",
{
  it("creates usage from function",
  {
    fun <- \(x = 1L) { }
    obj <- OoprRoxyUsage(fun, "fun");
    expect_equal(obj$content, "fun(x = 1L)");
  })

  it("linebreaks function if too wide",
  {
    fun <- \(super_long_argument_name_40_characters = 1L) { }
    obj <- OoprRoxyUsage(fun, "fun");
    expect_match(obj$content, "fun\\(\n.*\n\\)");
  })

  it("graveticks fun name",
  {
    `[` <- \( ) { };
    obj <- OoprRoxyUsage(`[`, "[");
    expect_equal(obj$content, "`[`()");
  })
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
}) ## OoprRoxyUsage
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("multiplication works",
{
  skip();
  x <- create_blocks(r"{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @name test
#' @title a test
#' @export
#' @description
#' A description
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("test",,
{
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field a a
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  a <- 1L;
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field b b
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:b <- \( ) { return(this$a); }
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description c
  #' @details deets
  #' @returns ret
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  c <- \( ) { }
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description d
  #' @param D d
  #' @details deets
  #' @returns ret
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  d <- \(d) { }
})
}")

  obj <- OoprRoxyClass(x[[1L]]);
  obj$makeFields()
  obj$makeMethods()
  obj$sections
})
