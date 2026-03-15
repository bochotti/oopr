## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("onInstall",
{
  testthat::skip_on_cran();
  local_packageInstall(files = c(code = r"{
  oopr::oopr("test",,  { public:get:a <- \( ) { }; })
  oopr::oopr("test2",, { public:static:a <- 1L; })
  .onLoad <- \(libname, pkgname)
  {
    oopr::oopr_onLoad();
  }
  oopr::oopr_onInstall();
  }"))
  test  <- ooprTest:::test;
  test2 <- ooprTest:::test2

  it("maintains active bindings",
  {
    expect_true(bindingIsActive('a', test@encl$this))
    expect_true(bindingIsActive('a', test2@encl$.this))
    expect_equal(test2$a, 1L);
    expect_no_error(test2$a <- 2L);
    expect_equal(test2$a, 2L);
  })

  it("maintains package parent environments",
  {
    expect_env(parent.env(test@encl), asNamespace("ooprTest"));
    expect_env(parent.env(test@encl$this), test@encl);
    expect_env(activeBindingFunction('a', test@encl$this), test@encl);
  })
})

test_that("oopr_onInstall",
{
  it("asserts arguments",
  {
    expect_error(oopr_onInstall(globalenv()), "`ns` must be a namespace");
    expect_error(oopr:::.onLoad("aaaa", "aaaa"), "there is no package")
  })

  it("allows for missing arguments",
  {
    wrap <- \() {
      libname <- pkgname <- "aaaa";
      oopr_onLoad();
    }
    expect_error(wrap(), "there is no package called 'aaaa'")

    wrap <- \() {
      oopr_onInstall();
    }
    environment(wrap) <- globalenv();
    expect_error(wrap(), "`ns` must be a namespace");
  })

})
