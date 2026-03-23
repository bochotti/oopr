## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("OoprVec$OoprVec",
{
  expect_error(OoprVec(1L));

  it("replaces the arguments for emplace method",
  {
    oopr("test",, { test <- \(x) { this$a <- x; }; public:a <- 0L; })
    vec <- OoprVec(test);
    expect_named(formals(vec$emplace), c(".", "x"));
    expect_identical(body(vec$emplace)[[c(2, 3, 3)]], list(quote(x)));
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("OoprVec$insert",
{
  oopr("test",, { test <- \(x) { this$a <- x; }; public:a <- 0L; })

  it("asserts class instance",
  {
    vec <- OoprVec(test);
    expect_error(vec$insert(, 1L));
    expect_error(vec$insert(, test));
    oopr("test2",,{})
    expect_error(vec$insert(, test2()));
  })

  it("inserts new class instance",
  {
    vec <- OoprVec(test);
    vec$insert(, test(1L));
    expect_true(is.oopr(vec$data[[1L]], "test"))
    expect_equal(vec$data[[1L]]$a, 1L);

    obj <- test(2L)
    vec$insert(, obj);
    expect_env(vec$data[[2L]], obj)

    vec$insert(0L, test(3L));
    expect_true(is.oopr(vec$data[[1L]], "test"))
    expect_equal(vec$data[[1L]]$a, 3L);
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("OoprVec$emplace",
{
  oopr("test",, { test <- \(x) { this$a <- x; }; public:a <- 0L; })

  it("constructs a new class instance",
  {
    vec <- OoprVec(test);

    vec$emplace(,1)
    expect_true(is.oopr(vec$data[[1L]], "test"))
    expect_equal(vec$data[[1L]]$a, 1L);

    vec$emplace(,2)
    expect_true(is.oopr(vec$data[[2L]], "test"))
    expect_equal(vec$data[[2L]]$a, 2L);
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("OoprVec$resize",
{
  oopr("test",, { test <- \(x) { this$a <- x; }; public:a <- 0L; })
  vec <- OoprVec(test);

  it("asserts n",
  {
    expect_error(vec$resize("a"));
    expect_error(vec$resize(1.5));
  })

  it("can enlarge data",
  {
    vec$resize(10L);
    expect_equal(vec$size, 10L);
    vec$data[[1L]] <- test(1L);
    expect_equal(vec$data[[1L]]$a, 1L);
    vec$resize(15L);
    expect_equal(vec$size, 15L);
  })

  it("can shrink data",
  {
    vec$resize(1L);
    expect_equal(vec$size, 1L);
    expect_equal(vec$data[[1L]]$a, 1L);

    vec$resize(0L);
    expect_true(vec$empty);
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("OoprVec$erase",
{
  oopr("test",, { test <- \(x) { this$a <- x; }; public:a <- 0L; })
  vec <- OoprVec(test);

  it("can erase at a position",
  {
    vec$emplace(, 1L)$emplace(, 2L)$emplace(, 3L);
    vec$erase();
    expect_equal(vec$size, 2L);
    expect_equal(vapply(vec$data, `[[`, integer(1L), "a"), 1:2);
    vec$erase(1L);
    expect_equal(vapply(vec$data, `[[`, integer(1L), "a"), 2);
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("OoprVec$swap",
{
  oopr("test",, { test <- \(x) { this$a <- x; }; public:a <- 0L; })
  vec <- OoprVec(test);
  vec$emplace(, 1L)$emplace(, 2L)$emplace(, 3L);

  it("swaps elements",
  {
    vec$swap(2, 1);
    expect_equal(vec$data[[1L]]$a, 2L);
    expect_equal(vec$data[[2L]]$a, 1L);

    vec$swap(1, 3);
    expect_equal(vec$data[[1L]]$a, 3L);
    expect_equal(vec$data[[3L]]$a, 2L);
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("OoprVec$apply",
{
  oopr("test",, { test <- \(x) { this$a <- x; }; public:a <- 0L; })
  vec <- OoprVec(test);
  vec$emplace(, 1L)$emplace(, 2L)$emplace(, 3L);

  it("can apply a function over the container",
  {
    expect_equal(vec$apply(\(x) x$a + 1L), as.list(1:3 + 1L))
  })

  it("can change the class instances in place",
  {
    vec$apply(\(x) x$a <- x$a + 1L);
    expect_equal(vec$apply(\(x) x$a), as.list(1:3 + 1L))
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("OoprMap$OoprMap",
{
  expect_error(OoprMap(1L));

  it("replaces the arguments for emplace method",
  {
    oopr("test",, { test <- \(x) { this$a <- x; }; public:a <- 0L; })
    vec <- OoprMap(test);
    expect_named(formals(vec$emplace), c(".", "x"));
    expect_identical(body(vec$emplace)[[c(2, 3, 3)]], list(quote(x)));
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("OoprMap$insert",
{
  oopr("test",, { test <- \(x) { this$a <- x; }; public:a <- 0L; });
  vec <- OoprMap(test);

  it("asserts key",
  {
    expect_error(vec$insert(1L, test(1L)));
    expect_error(vec$insert(c("a", "b"), test(1L)));
    expect_error(vec$insert(NA_character_, test(1L)));
    expect_error(vec$insert(, test(1L)));
  })

  it("inserts by key",
  {
    vec$insert("a", test("a"));
    expect_equal(vec$size, 1L);
    expect_true(vec$exists("a"));
    expect_true(is.oopr(vec$data[["a"]], "test"))
    expect_equal(vec$data[["a"]]$a, "a");
  })

  it("inserts to back for new keys",
  {
    vec$insert("b", test("b"));
    expect_equal(vec$size, 2L);
    expect_true(vec$exists("b"));
    expect_true(is.oopr(vec$data[[2L]], "test"))
    expect_equal(vec$data[[2L]]$a, "b");
  })

  it("will replace existing keys",
  {
    vec$insert("a", test("A"));
    expect_equal(vec$size, 2L);
    expect_true(vec$exists("a"));
    expect_true(is.oopr(vec$data[[1L]], "test"))
    expect_equal(vec$data[[1L]]$a, "A");
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("OoprMap$data",
{
  oopr("test",, { test <- \(x) { this$a <- x; }; public:a <- 0L; });
  vec <- OoprMap(test);
  vec$emplace("a", "a")$emplace("b", "b")$emplace("c", "c");

  it("allows changing inner class' members",
  {
    expect_no_error(vec$data$a$a <- "A");
    expect_equal(vec$data$a$a, "A");
  })

  it("does not allow assigning non-class",
  {
    expect_error(vec$data$b <- 1L);
    expect_equal(vec$data$b$a, "b");
    expect_error(vec$data$b <- vec$data$c);
    expect_equal(vec$data$b$a, "b");
  })

  it("does not allow changing keys",
  {
    expect_error(names(vec$data) <- toupper(names(vec$data)));
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("OoprMap$apply",
{
  oopr("test",, { test <- \(x) { this$a <- x; }; public:a <- 0L; });
  vec <- OoprMap(test);
  vec$emplace("a", "a")$emplace("b", "b")$emplace("c", "c");

  it("passes key & val as first two arguments",
  {
    expect_equal(vec$apply(\(key, val) { key; }), list(a="a", b="b", c="c"));
    expect_equal(vec$apply(\(key, val) { val; }), vec$data);
    expect_equal(
      vec$apply(\(key, val, const) { const; }, 1L)
     ,list(a = 1L, b = 1L, c = 1L)
    );
  })

  it("can change the instances in place",
  {
    vec$apply(\(key, val) { val$a <- toupper(key); });
    expect_equal(vec$apply(\(key, val) { val$a; }), list(a="A", b="B", c="C"));
  })
})
