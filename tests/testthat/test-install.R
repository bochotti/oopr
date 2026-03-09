## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("onInstall",
{
  testthat::skip_on_cran();
  local_packageInstall(files = c(code = r"{
  oopr::oopr("test",,  { get:a <- \( ) { }; })
  oopr::oopr("test2",, { static:a <- 1L; })
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
