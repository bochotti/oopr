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
#' Using serialize creates copies for each reference to a single object.
#' So test that the onLoad places the refeences back
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("oopr_onLoad local",
{
  if(!requireNamespace("FAKE_PKG_123", quietly = TRUE))
  {
    at <- findInExpr(body(loadNamespace), \(e) {
      iscall(e, "<-") && isname(e[[2L]], "makeNamespace");
    });
    eval(body(loadNamespace)[[at[[1L]]]]);
    makeNamespace("FAKE_PKG_123");
  }
  top <- getNamespace("FAKE_PKG_123");

  oopr("inhr",,
  {
  public:
    f             <- 1L;
    static:sf     <- 1L;
    get:p         <- \( ) { return(this$f);  }
    static:get:sp <- \( ) { return(this$sf); }
    m             <- \( ) { return(this$f);  }
    static:sm     <- \( ) { return(this$sf); }
  }, top)

  oopr("memb", public:inhr, { }, top)

  oopr("test", public:inhr,
  {
  public:
    cls          <- memb;
    static:scls  <- memb;
    cont         <- memb[];
    static:scont <- memb[];
  }, top)

  oopr("test2", public:test,
  {
  public:
    cls2          <- test;
    static:scls2  <- test;
  }, top)

  oopr_onInstall(top);
  oopr_onLoad("top", top);

  compare_memfuns <- \(x, y, ignore = character(0L))
  {
    sx <- substitute(x);
    sy <- substitute(y);
    names <- intersect(names(x), names(y));
    pull  <- \(sym, env)
    {
      if(bindingIsActive(sym, env))
      {
        return(activeBindingFunction(sym, env));
      }
      else
      {
        return(get0(sym, env, "function", FALSE));
      }
    }
    for(name in setdiff(names, ignore))
    {
      elx <- pull(name, x);
      ely <- pull(name, y);
      if(!isS4(elx) && is.function(elx) && !isS4(ely) && is.function(ely))
      {
        xlab <- deparse1(call("$", sx, name));
        ylab <- deparse1(call("$", sy, name));
        expect_equal(
          sexp_ptr(elx), sexp_ptr(ely), label = xlab, expected.label = ylab
        );
      }
    }
  }

  it("maintains the same ooprC",
  {
    expect_equal(sexp_ptr(top$test@encl$inhr),      sexp_ptr(top$inhr));
    expect_equal(sexp_ptr(top$test@encl$this$cls),  sexp_ptr(top$memb));
    expect_equal(sexp_ptr(top$test@encl$this$cont), sexp_ptr(OoprVec));
  })

  it("maintains the same inherited functions",
  {
    compare_memfuns(top$test@encl$.this, top$test@encl$this);
    compare_memfuns(top$test@encl$.this, top$inhr@encl$.this, "sf");
    compare_memfuns(top$test@encl$.this, top$inhr@encl$this);
    compare_memfuns(top$test@encl$this,  top$inhr@encl$.this, "sf");
    compare_memfuns(top$test@encl$this,  top$inhr@encl$this);
  })

  it("maintains the same static classes",
  {
    expect_equal(
      sexp_ptr(top$test@encl$this$scls)
     ,sexp_ptr(top$test2@encl$this$scls)
    );
    # expect_equal(
    #   sexp_ptr(activeBindingFunction("scls", top$test@encl$this))
    #  ,sexp_ptr(activeBindingFunction("scls", top$test2@encl$this))
    # );
  })

  top$test2@encl$this$scls2$scls
  top$test2@encl$this$scls
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("oopr_onLoad",
{
  testthat::skip_on_cran();
  testthat::skip_if_not_installed(c("withr", "callr"));
  testthat::skip_on_os("windows");
  local_packageInstall(files = c(code = r"{
  oopr::oopr("test",,  { public:get:a    <- \( ) { } })
  oopr::oopr("test2",, { public:static:a <- 1L; })
  .onLoad <- \(libname, pkgname)
  {
    oopr::oopr_onLoad();
  }
  oopr::oopr_onInstall();
  }"))
  test  <- eval(str2lang("ooprTest:::test"));
  test2 <- eval(str2lang("ooprTest:::test2"));

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
    eval(str2lang("library(ooprTest)"));
    expect_no_error(detach("package:ooprTest", unload = TRUE));
    expect_no_error(eval(str2lang("library(ooprTest)")));
    expect_true(
      bindingIsActive("a", eval(str2lang("ooprTest:::test2@encl$.this")))
    );
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("oopr_onLoad inherit",
{
  testthat::skip_on_cran();
  testthat::skip_if_not_installed(c("withr", "callr"));
  testthat::skip_on_os("windows");

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
    A <- eval(str2lang("ooprA::A"));
    B <- eval(str2lang("ooprB::B"));
    expect_env(B@encl$A@encl, A@encl);
    expect_env(activeBindingFunction("Af", B@encl$this), A@encl);
    expect_env(activeBindingFunction("Ap", B@encl$this), A@encl);
    expect_env(B@encl$this$Am, A@encl);
    expect_env(activeBindingFunction("As", B@encl$this), A@encl);
  })

  it("refers static members to original package env",
  {
    A <- eval(str2lang("ooprA::A"));
    B <- eval(str2lang("ooprB::B"));
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
  testthat::skip_on_os("windows");

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
    A <- eval(str2lang("ooprA::A"));
    B <- eval(str2lang("ooprB::B"));
    expect_env(B@encl$this$Bc@encl, A@encl);
    expect_true(is.oopr(B@encl$this$Bs, "A"));
    expect_env(parent.env(parent.env(B@encl$this$Bs)), asNamespace("ooprA"));
  })

  it("refers static members to original package env",
  {
    A <- eval(str2lang("ooprA::A"));
    B <- eval(str2lang("ooprB::B"));
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
