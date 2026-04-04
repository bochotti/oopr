## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("evaluate_classmem",
{
  oopr("memb",, {})
  it("marks classes as members",
  {
    oopr("test",, { a <- memb; })
    expect_true(test@meta$class$get(1L));

    oopr("test",, { a <- memb(); })
    expect_true(test@meta$class$get(1L));

    old1 <- str.oopr;
    unlockBinding("str.oopr", asNamespace("oopr"));
    on.exit({
      assign("str.oopr", old1, asNamespace("oopr"));
      lockBinding("str.oopr",  asNamespace("oopr"));
    })
    oopr("str.oopr",, {}, parent = asNamespace("oopr"))

    oopr("test",, { a <- oopr:::str.oopr; })
    expect_true(test@meta$class$get(1L));

    oopr("test",, { a <- oopr:::str.oopr(); })
    expect_true(test@meta$class$get(1L));
  })

  it("can denote containers",
  {
    oopr("test",, { public:a <- memb[]; })
    expect_true(test@meta$class$get(1L));
    expect_true(is.ooprC(test@encl$this$a, "OoprVec"));
    obj <- test();
    expect_true(is.oopr(obj$a, "OoprVec"));

    oopr("test",, { public:a <- memb[[]]; })
    expect_true(test@meta$class$get(1L));
    expect_true(is.ooprC(test@encl$this$a, "OoprMap"));
    obj <- test();
    expect_true(is.oopr(obj$a, "OoprMap"));

    expect_error(oopr("test",, { public:a <- sum[]; }));
    expect_no_error(oopr("test",, { public:a <- (1:10)[1]; }));
    expect_false(test@meta$class$get(1L));
  })

})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("definitions_classmem",
{
  it("inserts call into constructor method",
  {
    oopr("memb",, {})
    oopr("test",, { a <- memb; })
    expect_identical(body(test@encl$this$test)[[2:1]], quote(base::assign));

    oopr("test",, { a <- memb(); })
    expect_identical(body(test@encl$this$test)[[2:1]], quote(base::assign));

    oopr("memb",, { memb <- \(x) { }})
    oopr("test",, { a <- memb(1L); })
    expect_identical(body(test@encl$this$test)[[2:3]], quote(this$a(x = 1L)));

    oopr("memb",, { memb <- \(x = 1) { }})
    oopr("test",, { a <- memb; })
    expect_identical(body(test@encl$this$test)[[2:3]], quote(this$a()));

    oopr("test",, { static:a <- memb; })
    expect_identical(body(test@encl$this$test), quote({}));
  })

  it("requires initialization of classes with non-default arguments",
  {
    oopr("memb",, { memb <- \(x) { }})
    expect_error(
      oopr("test",, { a <- memb; })
     ,class = "ooprDefNoInit"
    );
    expect_error(
      oopr("test",, { test <- \( ) { this$a(); }; a <- memb; })
     ,class = "ooprDefInitSignatureNotMatched"
    );
    expect_no_error(
      oopr("test",, { test <- \( ) { this$a(1); }; a <- memb; })
    );
  })

  it("does not allow multiple initializations",
  {
    oopr("memb",, { memb <- \( ) { }})
    expect_error(
      oopr("test",, { test <- \( ) { this$a(); this$a(); }; a <- memb; })
     ,class = "ooprDefMultipleInit"
    );
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("references_classmem",
{
  it("does not allow usage prior to init",
  {
    oopr("memb",, { public:a <- 1L; })
    expect_error(
       oopr("test",, { a <- memb; test <- \( ) { this$a$a; this$a(); } })
      ,class = "ooprClassMemUsageBeforeInit"
    );
    expect_no_error(
       oopr("test",, { a <- memb; test <- \( ) { this$a(); this$a$a; } })
    );
  })

  it("does not allow usage of undefined members",
  {
    oopr("memb",, { public:a <- 1L; })
    expect_error(
       oopr("test",, { a <- memb; test <- \( ) { this$a$b; } })
      ,class = "ooprRefNotDefined"
    );
    expect_error(
       oopr("test",, { a <- memb; b <- \( ) { this$a$b; } })
      ,class = "ooprRefNotDefined"
    );

    expect_no_error(
       oopr("test",, { a <- memb; test <- \( ) { this$a(); this$a$a; } })
    );
    expect_no_error(
       oopr("test",, { a <- memb; b <- \( ) { this$a$a; } })
    );
  })

  it("does not allow usage of non-public members",
  {
    oopr("memb",, { protected:a <- 1L; })
    expect_error(
       oopr("test",, { b <- \( ) { this$a$a; }; a <- memb; })
      ,class = "ooprRefNotDefined"
    );
  })

  it("does not allow access / call / assign if not allowed",
  {
    oopr("memb",, { public:a <- 1L; })
    expect_error(
      oopr("test",, { a <- memb; b <- \( ) { this$a$a(); }})
     ,class = "ooprRefBadCall"
    );

    oopr("memb",, { public:a <- \(x) { }; })
    expect_error(
      oopr("test",, { a <- memb; b <- \( ) { this$a$a(); }})
     ,class = "ooprRefUnmatchedCall"
    );
    expect_error(
      oopr("test",, { a <- memb; b <- \( ) { this$a$a <- 1L; }})
     ,class = "ooprRefBadAssignment"
    );

    oopr("memb",, { public:get:a <- \( ) { }; })
    expect_error(
      oopr("test",, { a <- memb; b <- \( ) { this$a$a <- 1L; }})
     ,class = "ooprRefBadAssignment"
    );

    oopr("memb",, { public:set:a <- \(x) { }; })
    expect_error(
      oopr("test",, { a <- memb; b <- \( ) { this$a$a; }})
     ,class = "ooprRefBadAccess"
    );

    oopr("memb",, { public:static:a <- 1L; })
    expect_error(
      oopr("test",, { a <- memb; static:b <- \( ) { this$a$a; }})
     ,class = "ooprRefNotStatic"
    );
  })

  it("continues to disallow when nested",
  {
    oopr("memb",,
    {
    public:
      a        <- 1L;
      get:b    <- \( ) { }
      set:c    <- \(x) { }
      d        <- \(x) { }
      static:e <- 1L;
    })
    oopr("memb2",, { public:b <- memb; })

    expect_error(
      oopr("test",, { a <- memb2;  b <- \( ) { this$a$b$z; } })
     ,class = "ooprRefNotDefined"
    );
    expect_error(
      oopr("test",, { a <- memb2; b <- \( ) { this$a$b$a() } })
     ,class = "ooprRefBadCall"
    );
    expect_error(
      oopr("test",, { a <- memb2; b <- \( ) { this$a$b$b <- 1L } })
     ,class = "ooprRefBadAssignment"
    );
    expect_error(
      oopr("test",, { a <- memb2; b <- \( ) { this$a$b$c; } })
     ,class = "ooprRefBadAccess"
    );
    expect_error(
      oopr("test",, { a <- memb2; b <- \( ) { this$a$b$d() } })
     ,class = "ooprRefUnmatchedCall"
    );
    expect_error(
      oopr("test",, { a <- memb2; b <- \( ) { ((this$a$b$d))() } })
     ,class = "ooprRefUnmatchedCall"
    );
    expect_error(
      oopr("test",, { a <- memb2; static:b <- \( ) { this$a$b; } })
     ,class = "ooprRefNotStatic"
    );
  })

  it("continues to disallow when members are not used at the top-level",
  {
    oopr("memb",, { public:get:a <- \( ) { }; public:set:b <- \(x) { }})

    expect_error(
      oopr("test",, { a <- memb2;  b <- \( ) { t(,this$a$z); } })
     ,class = "ooprRefNotDefined"
    );
    expect_error(
      oopr("test",, { a <- memb2;  b <- \( ) { t(t(,this$a$z)); } })
     ,class = "ooprRefNotDefined"
    );
    expect_error(
      oopr("test",, { a <- memb; b <- \( ) { t(,this$a$a()); }})
     ,class = "ooprRefBadCall"
    );
    expect_error(
      oopr("test",, { a <- memb; b <- \( ) { t(t(,this$a$a())); }})
     ,class = "ooprRefBadCall"
    );

    expect_error(
      oopr("test",, { a <- memb; b <- \( ) { t(,this$a$a <- 1L); }})
     ,class = "ooprRefBadAssignment"
    );
    expect_error(
      oopr("test",, { a <- memb; b <- \( ) { t(t(,this$a$a <- 1L)); } })
     ,class = "ooprRefBadAssignment"
    );

    expect_error(
      oopr("test",, { a <- memb; b <- \( ) { t(,this$a$b); }})
     ,class = "ooprRefBadAccess"
    );
    expect_error(
      oopr("test",, { a <- memb; b <- \( ) { t(t(,this$a$b)); }})
     ,class = "ooprRefBadAccess"
    );

    expect_error(
      oopr("test",, { a <- memb; b <- \( ) { ((this$a$b))(); }})
     ,class = "ooprRefBadCall"
    );
  })

  it("allows for access of the class",
  {
    oopr("memb",, { })
    expect_no_error(
      oopr("test",, { a <- memb; b <- \( ) { this$a; }})
    );
    expect_error(
      oopr("test",, { a <- memb; b <- \( ) { this$a <- 1L; }})
     ,class = "ooprRefBadAssignment"
    );
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("references_containers",
{
  oopr("memb",,
  {
  public:
    a        <- 1L;
    b        <- \(x) { }
    get:c    <- \( ) { }
    set:d    <- \(x) { }
    static:e <- 1L;
  })

  it("handles containers",
  {
    expect_error(
      oopr("test",, { a <- memb[]; b <- \(x) { this$a$a; } })
     ,class = "ooprRefNotDefined"
    );
    expect_no_error(
      oopr("test",, { a <- memb[]; b <- \(x) { this$a$size; } })
    );

    expect_error(
      oopr("test",, { a <- memb[]; b <- \(x) { this$a[1L]$z; } })
     ,class = "ooprRefNotDefined"
    );
    expect_error(
      oopr("test",, { a <- memb[]; b <- \(x) { this$a[1L]$a(); } })
     ,class = "ooprRefBadCall"
    );
    expect_no_error(
      oopr("test",, { a <- memb[]; b <- \(x) { this$a[1L]$a; } })
    );

    expect_error(
      oopr("test",, { a <- memb[]; b <- \(x) { this$a[1L]$b(); } })
     ,class = "ooprRefUnmatchedCall"
    );
    expect_no_error(
      oopr("test",, { a <- memb[]; b <- \(x) { this$a[1L]$b(1L); } })
    );

    expect_error(
      oopr("test",, { a <- memb[]; b <- \(x) { this$a[1L]$c <- 1L; } })
     ,class = "ooprRefBadAssignment"
    );

    expect_error(
      oopr("test",, { a <- memb[]; b <- \(x) { this$a[1L]$d; } })
     ,class = "ooprRefBadAccess"
    );
    expect_no_error(
      oopr("test",, { a <- memb[]; b <- \(x) { this$a[1L]$d <- 1L; } })
    );

    expect_no_error(
      oopr("test",, { static:a <- memb[]; static:b <- \(x) { this$a[1L]$a; } })
    );
    expect_no_error(
      oopr("test",, { static:a <- memb[]; static:b <- \(x) { this$a$size; } })
    );
    expect_error(
      oopr("test",, { a <- memb[]; static:b <- \(x) { this$a[1L]$e; } })
     ,class = "ooprRefNotStatic"
    );

  })

  it("handles nested containers",
  {
    oopr("memb2",, { public:c <- memb[]; static:s <- memb[];})

    expect_error(
      oopr("test",, { a <- memb2[]; b <- \(x) { this$a[1L]$c$a; }})
     ,class = "ooprRefNotDefined"
    );
    expect_no_error(
      oopr("test",, { a <- memb2[]; b <- \(x) { this$a[1L]$c$size; }})
    );

    expect_error(
      oopr("test",, { a <- memb2[]; b <- \(x) { this$a[1L]$s$a; }})
     ,class = "ooprRefNotDefined"
    );
    expect_no_error(
      oopr("test",, { a <- memb2[]; b <- \(x) { this$a[1L]$s$size; }})
    );

    expect_error(
      oopr("test",, { a <- memb2[]; b <- \(x) { this$a[1L]$c[1L]$z; }})
     ,class = "ooprRefNotDefined"
    );
    expect_error(
      oopr("test",, { a <- memb2[]; b <- \(x) { this$a[1L]$c[1L]$a(); }})
     ,class = "ooprRefBadCall"
    );
    expect_no_error(
      oopr("test",, { a <- memb2[]; b <- \(x) { this$a[1L]$c[1L]$a; }})
    );

    expect_error(
      oopr("test",, { a <- memb2[]; b <- \(x) { this$a[1L]$c[1L]$b(); } })
     ,class = "ooprRefUnmatchedCall"
    );
    expect_no_error(
      oopr("test",, { a <- memb2[]; b <- \(x) { this$a[1L]$c[1L]$b(1L); } })
    );

    expect_error(
      oopr("test",, { a <- memb2[]; b <- \(x) { this$a[1L]$c[1L]$c <- 1L; } })
     ,class = "ooprRefBadAssignment"
    );

    expect_error(
      oopr("test",, { a <- memb2[]; b <- \(x) { this$a[1L]$c[1L]$d; } })
     ,class = "ooprRefBadAccess"
    );
    expect_no_error(
      oopr("test",, { a <- memb2[]; b <- \(x) { this$a[1L]$c[1L]$d <- 1L; } })
    );

    expect_no_error(
      oopr("test",, { static:a <- memb2[]; static:b <- \(x) { this$a[1L]$c[1L]$a; } })
    );
    expect_no_error(
      oopr("test",, { static:a <- memb2[]; static:b <- \(x) { this$a[1L]$c$size; } })
    );
    expect_error(
      oopr("test",, { a <- memb2[]; static:b <- \(x) { this$a[1L]$c[1L]$e; } })
     ,class = "ooprRefNotStatic"
    );

  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("classmem",
{
  oopr("memb",, { public:a <- 1L; });
  oopr("test",, { public:a <- memb; })
  expect_true(is.ooprC(test@encl$this$a, "memb"));
  obj <- test();
  expect_true(is.oopr(obj$a, "memb"));
  obj$a$a <- 2L;
  expect_equal(obj$a$a, 2L);
})
