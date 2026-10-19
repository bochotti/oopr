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

  it("replaces gravetick title with sQuote",
  {
    obj <- OoprRoxySection("`title`");
    expect_match(obj$toRd(), "\\\\sQuote\\{title\\}");
  })

  it("can add a horizontal rule",
  {
    obj <- OoprRoxySection("title", hr = TRUE);
    expect_match(obj$toRd(), "\\\\hr\\{\\}title");
  })

  it("can use a hyperref",
  {
    obj <- OoprRoxySection("title", rf = "pre");
    expect_match(obj$toRd(), "\\\\ht\\{pre-title\\}\\{title\\}");
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

  it("wraps in performatted",
  {
    obj <- OoprRoxyUsage(sum, "sum");
    expect_match(obj$toRd(), "\\\\preformatted\\{sum\\(\\)\\}");
  })
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
}) ## OoprRoxyUsage
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("OoprRoxyArguments",
{
  tag <- \(x) { roxygen2::roxy_tag_parse(roxygen2::roxy_tag("param", x, x)); }

  it("can insert tags",
  {
    obj <- OoprRoxyArguments(list(tag("hello world"), tag("goodbye earth")));
    expect_equal(obj$content, c(hello = "world", goodbye = "earth"));
  })

  it("replaces \\cr with \\br",
  {
    obj <- OoprRoxyArguments(list(tag("arg top\\crbottom")));
    expect_no_match(obj$content, "\\\\cr");
    expect_match(obj$content, "\\\\br");
  })

  it("formats",
  {
    obj <- OoprRoxyArguments(list(tag("hello world"), tag("goodbye earth")));
    rd  <- obj$toRd();
    expect_match(rd, "\\\\tabular\\{ll\\}\\{");
    expect_match(rd, "\\\\code\\{hello\\} \\\\tab world \\\\cr");
    expect_match(rd, "\\\\code\\{goodbye\\} \\\\tab earth\\}");
  })
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
}) ## OoprRoxyArguments
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("OoprRoxyMethod",
{
  tag <- \(tag, x) { roxygen2::roxy_tag_parse(roxygen2::roxy_tag(tag, x, x)); }
  d   <- tag("description", "a");
  r   <- tag("returns", "a");
  it("requires description and return tags, automatically makes usage tag",
  {
    .this <- new.env();
    class(.this) <- "cls";
    fun <- \( ) { };
    expect_message(
      OoprRoxyMethod("fun", list(), fun)
     ,"Issue/s with method \"cls\\$fun\".*@description and @returns"
    );
    expect_message(
      OoprRoxyMethod("fun", list(d), fun)
     ,"Issue/s with method \"cls\\$fun\".*Requires @returns"
    );
    expect_message(
      OoprRoxyMethod("fun", list(r), fun)
     ,"Issue/s with method \"cls\\$fun\".*@description tag"
    );
    expect_no_message(obj <- OoprRoxyMethod("fun", list(d, r), fun));
    expect_equal(obj$sections$keys, c("Description", "Usage", "Returns"));
    expect_true(is.oopr(obj$sections["Description"], "OoprRoxySection"));
    expect_true(is.oopr(obj$sections["Usage"], "OoprRoxyUsage"));
    expect_true(is.oopr(obj$sections["Returns"], "OoprRoxySection"));
  })

  it("warns when missing argument doc",
  {
    fun <- \(x) { }
    expect_message(
      OoprRoxyMethod("fun", list(d, r), fun)
     ,"Argument \"x\" is not documented"
    );
    expect_no_message(
      OoprRoxyMethod("fun", list(d, r, tag("param", "x y")), fun)
    );
    fun <- \( ) { }
    expect_message(
      OoprRoxyMethod("fun", list(d, r, tag("param", "x y")), fun)
     ,"Documented argument \"x\" is not in the signature"
    );
  })

  it("orders tags",
  {
    fun <- \( ) { }
    obj <- OoprRoxyMethod("fun", list(r, d), fun);
    expect_equal(obj$sections$keys, c("Description", "Usage", "Returns"));
  })

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
}) ## OoprRoxyMethod
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("OoprRoxyClass",
{
  on.exit(OoprRoxy$classes$resize());
  text <- r"{
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @name atest
  #' @title a test
  #' @export
  #' @description
  #' A description
  #' @details
  #' A detail
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  oopr("test",,
  {
  public:
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
    #' @field a a
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
    a <- 1L;
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
    #' @field b b
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
    get:b <- \( ) { return(this$a); }
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
    #' @description c
    #' @details deets
    #' @returns ret
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
    c <- \( ) { }
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
    #' @description d
    #' @param d arg
    #' @details deets
    #' @returns ret
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
    d <- \(d) { }
  })
  }"
  x <- create_blocks(text);
  obj <- OoprRoxyClass(x[[1L]]);

  it("collects information",
  {
    expect_equal(obj$title, "test");
    expect_equal(obj$access, "public");
    expect_equal(obj$rdname, "atest");
  })

  it("seperates the member tags",
  {
    expect_named(obj$members, letters[1:4]);
    expect_equal(unname(lapply(obj$members, length)), list(1L, 1L, 3L, 4L));
    expect_length(obj$block$tags, length(x[[1]]$tags) - (1 + 1 + 3 + 4));
  })

  obj$makeUsage();

  it("moves description & details into the section",
  {
    obj$makeSections();
    expect_equal(obj$sections$keys, c("Description", "Details"));
    expect_length(obj$block$tags, length(x[[1]]$tags) - (1 + 1 + 3 + 4 + 2));
  })

  it("creates a section for fields",
  {
    obj$makeFields()
    expect_true(obj$sections$exists("Fields"));
    expect_equal(obj$sections["Fields"]$names, c("a", "b"));
  })

  it("creates sections for methods",
  {
    obj$makeMethods()
    expect_true(obj$sections$exists("Methods"));
    expect_equal(
      obj$sections["Methods"]$names
      ,sprintf("\\hl{atest-test-%s}{%s}", c("c", "d"), c("c", "d"))
    );
    expect_true(obj$sections$exists("c"));
    expect_true(obj$sections$exists("d"));
  })

  it("creates a section for the class section",
  {
    obj$makeTag();
    expect_equal(obj$block$tags[[7]]$tag, "section");
  })

  text2 <- paste0(text, "\n", r"{
  #' @name test2
  #' @title test22
  #' @export
  oopr("test2", public:test,
  {
  })
  }")
  x <- create_blocks(text2);
  OoprRoxy$addBlock(x[[1L]]);
  OoprRoxy$addBlock(x[[2L]]);
  bse <- OoprRoxy$classes["test"];
  obj <- OoprRoxy$classes["test2"];

  it("copies documentation from parent class",
  {
    expect_equal(obj$sections$keys, c("Fields", "Methods", "c", "d"));
    expect_equal(
      obj$sections["Fields"]$content
     ,bse$sections["Fields"]$content
    );
    expect_equal(
      unname(obj$sections["Methods"]$content)
     ,unname(bse$sections["Methods"]$content)
    );
  })

  it("forwards constructor args to fields",
  {
    x <- create_blocks(r"{
    oopr("test",,
    {
    public:
      #' @param x an arg
      test <- \(x) { }
      x    <- 1L;
    })
    }");
    obj <- OoprRoxyClass(x[[1L]]);
    tag <- obj$members$x[[1L]];
    expect_equal(tag$tag, "field");
    expect_equal(tag$val, list(name = "x", description = "an arg"));
  })

  it("allows @inherits tag",
  {
    x <- create_blocks(r"{
    oopr("class1",,
    {
    public:
      #' @description
      #' aaa
      #' @returns
      #' bbb
      method <- \( ) { }
    })

    oopr("class2",,
    {
    public:
      #' @inherit class1$method
      method <- \( ) { }
    })
    }");
    OoprRoxy$addBlock(x[[1L]]);
    OoprRoxy$addBlock(x[[2L]]);
    one <- OoprRoxy$classes["class1"];
    two <- OoprRoxy$classes["class2"];
    expect_equal(
      two$sections["method"]$toRd()
     ,sub("class1", "class2", one$sections["method"]$toRd())
    )
  })

  it("shows warning for non-documented members",
  {
    x <- create_blocks(r"{
    #' @description a
    #' @export
    oopr("test",,
    {
    public:
      x <- 1L;
    })
    }")
    expect_message(
      OoprRoxyClass(x[[1L]])
     ,"Member \"x\" in class \"test\""
    );
  })
})
