## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
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

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("oopr_onLoad",
{
  testthat::skip_on_cran();
  testthat::skip_if_not_installed(c("withr", "callr"));
  local_packageInstall(files = c(code = r"{
  oopr::oopr("test",,  { public:get:a    <- \( ) { } })
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

  it("can be reloaded",
  {
    library(ooprTest);
    expect_no_error(detach("package:ooprTest", unload = TRUE));
    expect_no_error(library(ooprTest));
    expect_true(bindingIsActive("a", ooprTest:::test2@encl$.this))
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("oopr_onLoad inherit",
{
  testthat::skip_on_cran();
  testthat::skip_if_not_installed(c("withr", "callr"));

  local_packageInstall(
    name      = "ooprA"
   ,namespace = "export(A)"
   ,files     = c(
      code = r"{
      oopr::oopr("A",,
      {
      public:
        Af        <- 1L;
        get:Ap    <- \( ) { }
        Am        <- \( ) { }
        static:As <- 1L;
      })
      .onLoad <- \(libname, pkgname)
      {
        oopr::oopr_onLoad();
      }
      oopr::oopr_onInstall();
      }"
    )
  )

  local_packageInstall(
    name      = "ooprB"
   ,namespace = "import(ooprA)\nexport(B)"
   ,imports   = c("oopr", "ooprA")
   ,files     = c(
      code = r"{
      oopr::oopr("B", public:ooprA::A,
      {
      public:
        Bf        <- 1L;
        get:Bp    <- \( ) { }
        Bm        <- \( ) { }
        static:Bs <- 1L;
      })
      .onLoad <- \(libname, pkgname)
      {
        oopr::oopr_onLoad();
      }
      oopr::oopr_onInstall();
      }"
    )
  )

  it("carries over inherited classes from other packages",
  {
    A <- ooprA::A;
    B <- ooprB::B;
    expect_env(B@encl$A@encl, A@encl);
    expect_env(activeBindingFunction("Af", B@encl$this), A@encl);
    expect_env(activeBindingFunction("Ap", B@encl$this), A@encl);
    expect_env(B@encl$this$Am, A@encl);
    expect_env(activeBindingFunction("As", B@encl$this), A@encl);
  })

  it("refers static members to original package env",
  {
    A <- ooprA::A;
    B <- ooprB::B;
    B$As   <- 2L;
    expect_equal(A$As, 2L);
    obj    <- B();
    obj$As <- 3L;
    expect_equal(A$As, 3L);
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("oopr_onLoad classmem",
{
  testthat::skip_on_cran();
  testthat::skip_if_not_installed(c("withr", "callr"));

  local_packageInstall(
    name      = "ooprA"
   ,namespace = "import(oopr)\nexport(A)"
   ,files     = c(
      code = r"{
      oopr::oopr("A",,
      {
      public:
        Af        <- 1L;
        get:Ap    <- \( ) { }
        Am        <- \( ) { }
        static:As <- 1L;
      })
      .onLoad <- \(libname, pkgname)
      {
        oopr::oopr_onLoad();
      }
      oopr::oopr_onInstall();
      }"
    )
  )

  local_packageInstall(
    name      = "ooprB"
   ,namespace = "import(oopr)\nimport(ooprA)\nexport(B)"
   ,imports   = c("oopr", "ooprA")
   ,files     = c(
      code = r"{
      oopr::oopr("B",,
      {
      public:
        Bc <- ooprA::A;
        static:Bs <- ooprA::A;
      })
      .onLoad <- \(libname, pkgname)
      {
        oopr::oopr_onLoad();
      }
      oopr::oopr_onInstall();
      }"
    )
  )

  it("carries over class members from other packages",
  {
    A <- ooprA::A;
    B <- ooprB::B;
    expect_env(B@encl$this$Bc@encl, A@encl);
    expect_true(is.oopr(B@encl$this$Bs, "A"));
    expect_env(parent.env(parent.env(B@encl$this$Bs)), asNamespace("ooprA"));
  })

  it("refers static members to original package env",
  {
    A <- ooprA::A;
    B <- ooprB::B;
    expect_env(activeBindingFunction("As", B@encl$this$Bs), A@encl);
    B$Bs$As   <- 2L;
    expect_equal(A$As, 2L);
    obj    <- B();
    obj$Bs$As <- 3L;
    expect_equal(A$As, 3L);
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#TODO: test combination of inherited classes and class members
#TODO: what about chained static class members??
