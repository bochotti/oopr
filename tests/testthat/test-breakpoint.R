## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("OoprBreakpointsMap",
{
  tmp <- tempfile(fileext = ".R");
  on.exit(unlink(tmp));
  text <- r"{
  old <- \( )
  {
    "hello";
    "world";
  }
  new <- \( )
  {
    "hello";
    "there";
    "world";
  }
  }";
  cat(text, file = tmp);
  source(tmp, local = TRUE, keep.source = TRUE);

  map <- OoprBreakpointsMap(old, new);

  it("maps old line numbers to new ones",
  {
    expect_equal(map$toNew(4L), 9L);
    expect_equal(map$toNew(5L), 11L);
  })

  it("maps new line numbers to old ones",
  {
    expect_equal(map$toOld(9L),  4L);
    expect_equal(map$toOld(10L), NA_integer_);
    expect_equal(map$toOld(11L), 5L);
  })

  it("can reload",
  {
    map$load(old, old);
    expect_equal(map$toNew(4L), 4L);
    expect_equal(map$toNew(5L), 5L);
  })

})


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("OoprBreakpointsFunction",
{
  tmp <- tempfile(fileext = ".R");
  on.exit(unlink(tmp));
  text <- r"{
    method <- \( )
    {
      1L;
      {
        2L;
      }
    }
  }";
  cat(text, file = tmp);
  source(tmp, local = TRUE, keep.source = TRUE);
  obj <- OoprBreakpointsFunction(method);

  it("loads",
  {
    expect_equal(obj$fun,    method);
    expect_equal(obj$body,   body(method));
    expect_equal(obj$srcref, attr(method, "srcref"));
    expect_equal(attr(obj$body, "srcref"), attr(body(method), "srcref"));
  })

  it("knows when the function is out of sync with file",
  {
    expect_true(obj$synced());
    cat(strsplit(text, '\n')[[1L]][-4L], sep = '\n', file = tmp);
    expect_false(obj$synced());
    cat(text, file = tmp);
    expect_true(obj$synced());
    unlink(tmp);
    expect_false(obj$synced());
    cat(text, file = tmp);
  })

  it("can tell which line number covers the function",
  {
    expect_false(obj$has(1L));
    expect_true(obj$has(4L));
    expect_true(obj$has(7L));
    expect_false(obj$has(9L));
  })

  it("can set a breakpoint",
  {
    expect_no_error(obj$add(c(4L, 6L)));
    expect_true(has_breakpoint(method));
    expect_equal(
      body(method)[[c(2, 2)]]
     ,quote(if(isTRUE(try(TRUE, TRUE))) .doTrace(browser()))
    );
    expect_equal(
      body(method)[[c(3, 2, 2)]]
     ,quote(if(isTRUE(try(TRUE, TRUE))) .doTrace(browser()))
    );
    expect_equal(obj$breaks$line, c(4L, 6L));
    expect_equal(obj$body, attr(body(method), "orig"));
  })

  it("can remove breakpoints",
  {
    obj$rm(4L);
    expect_equal(obj$breaks$line, c(6L));
    expect_equal(body(method)[[c(2)]], 1L);
    expect_equal(
      body(method)[[c(3, 2, 2)]]
     ,quote(if(isTRUE(try(TRUE, TRUE))) .doTrace(browser()))
    );
    obj$rm(6L);
    expect_false(has_breakpoint(method));
  })

  text <- r"{
    method <- \( )
    {
      "hello";
      "world";
    }
  }"
  cat(text, file = tmp);
  source(tmp, local = TRUE, keep.source = TRUE);

  obj <- OoprBreakpointsFunction(method);
  obj$add(5L);

  writeLines(append(readLines(tmp, warn = FALSE), '"there";', 4L), tmp);
  source(tmp, local = TRUE, keep.source = TRUE);

  it("changes displayed line numbers if srcref changes",
  {
    obj$load(method);
    expect_equal(obj$breaks$line, 6L);
    expect_true(has_breakpoint(obj$fun));
  })

  it("can remove breaks from new line numbers",
  {
    expect_false(obj$has(5L));
    obj$rm(6L);
    expect_equal(nrow(obj$breaks), 0L);
    expect_false(has_breakpoint(obj$fun));
  })

  it("does not allow adding breaks on old line numbers",
  {
    expect_error(obj$add(5L));
    expect_equal(nrow(obj$breaks), 0L);
    expect_false(has_breakpoint(obj$fun));
  })

  it("can add new breaks on new line numbers",
  {
    obj$add(6L);
    expect_equal(nrow(obj$breaks), 1L);
    expect_true(has_breakpoint(obj$fun));
  })

  it("can load a new function",
  {
    obj$load(method, new = TRUE);
    expect_equal(obj$breaks$line, 6L);
    expect_equal(obj$breaks$steps[[1L]], 4L);
    expect_true(has_breakpoint(obj$fun));

    expect_no_error(obj$add(5L));
    expect_equal(obj$breaks$line, 6:5);
    expect_equal(obj$breaks$steps[[2L]], 3L);
    expect_identical(
      body(obj$fun)[[3L]]
     ,quote({ if(isTRUE(try(TRUE, TRUE))) .doTrace(browser()); "there" })
    );
    expect_identical(
      body(obj$fun)[[4L]]
     ,quote({ if(isTRUE(try(TRUE, TRUE))) .doTrace(browser()); "world" })
    );
  })

  it("removes lines",
  {
    writeLines(readLines(tmp, warn = FALSE)[-5L], tmp);
    source(tmp, local = TRUE, keep.source = TRUE);
    obj$load(method);
    expect_equal(obj$breaks$line, 5L);
    obj$rm();
    expect_equal(obj$breaks$line, integer(0L));
    expect_false(is.null(attr(method, "srcref")));
    expect_false(is.null(attr(body(method), "srcref")));
  })

  it("handles null srcrefs",
  {
    obj$add(5L);
    expect_no_error(
      obj$load(`attr<-`(method, "srcref", NULL), FALSE)
    );
    expect_length(obj$breaks$line, 0L);
    expect_error(obj$add(5L), "cannot set `lines`");

    obj$load(method, FALSE);

    obj$add(5L);
    expect_no_error(
      obj$load(`attr<-`(method, "srcref", NULL), TRUE)
    );
    expect_length(obj$breaks$line, 0L);
    expect_error(obj$add(5L), "cannot set `lines`");
    expect_false(obj$synced());

    obj$load(method, TRUE);

    obj <- OoprBreakpointsFunction(`attr<-`(method, "srcref", NULL));
    expect_error(obj$add(5L), "cannot set `lines`");
  })

  it("asserts",
  {
    expect_error(obj$add(6L, logical(2L)), "`lines` and `conds`");
    expect_error(obj$add(0L), "cannot set `lines`");
  })

  it("does not cause duplicate steps",
  {
    text <- r"{
      method <- \( )
      {
        {
          {
            {
              {
                c( "hello"
                  ,"world");
              }
            }
          }
        }
      }
    }"
    cat(text, file = tmp);
    source(tmp, local = TRUE, keep.source = TRUE);
    obj <- OoprBreakpointsFunction(method);
    obj$add(8:9);
    expect_equal(nrow(obj$breaks), 1L);
  })

  it("handles start and end lines",
  {
    text <- r"{
      method <- \( )
      {
        "hello";
      }
    }";
    cat(text, file = tmp);
    source(tmp, local = TRUE, keep.source = TRUE);
    obj <- OoprBreakpointsFunction(method);
    expect_error(obj$add(3L), "cannot set `lines` 3L");
    expect_error(obj$add(5L), "cannot set `lines` 5L");
  })

  it("handles non { bodies",
  {
    text <- r"{
      method <- \( ) "hello";
    }";
    cat(text, file = tmp);
    source(tmp, local = TRUE, keep.source = TRUE);
    obj <- OoprBreakpointsFunction(method);
    expect_error(obj$add(2L), "non \\{ body");

    text <- r"{
      method <- \( )
      {
        "hello"
      }
    }";
    cat(text, file = tmp);
    source(tmp, local = TRUE, keep.source = TRUE);
    obj <- OoprBreakpointsFunction(method);
    obj$add(4L);
    text <- r"{
      method <- \( )
        "hello"
    }";
    cat(text, file = tmp);
    source(tmp, local = TRUE, keep.source = TRUE);
    obj$load(method);
    expect_equal(obj$breaks$line, 3L);

    text <- r"{
      method <- \( )
      {
        "hello"
      }
    }";
    cat(text, file = tmp);
    source(tmp, local = TRUE, keep.source = TRUE);
    obj <- OoprBreakpointsFunction(method);
    obj$add(4L);
    text <- r"{
      method <- \( )
        "hello"
    }";
    cat(text, file = tmp);
    source(tmp, local = TRUE, keep.source = TRUE);
    obj$load(method, TRUE);
    expect_length(obj$breaks$line, 0L);
  })

})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("OoprBreakpointsClass",
{
  tmp <- tempfile(fileext = ".R");
  on.exit(unlink(tmp));
  text <- r"{
  oopr("test",,
  {
  public:
    method       <- \( )
    {
      1L;
      {
        2L;
      }
    }
    get:property <- \( )
    {
      3L;
      {
        4L;
      }
    }
    field <- 1L;
  })
  }";
  cat(text, file = tmp);
  source(tmp, local = TRUE, keep.source = TRUE);

  obj <- OoprBreakpointsClass(test);

  it("loads",
  {
    expect_equal(obj$name, "test");
    expect_equal(obj$ooprC, test);
  })

  it("tells when a class has a function name and/or line",
  {
    expect_true(obj$has(7));
    expect_equal(obj$where(7), "method");
    expect_true(obj$has(14));
    expect_equal(obj$where(14), "property");

    expect_false(obj$has(4))
    expect_equal(obj$where(4), "");
  })

  it("knows when a function goes out of sync",
  {
    expect_true(obj$synced());
    cat(strsplit(text, '\n')[[1L]][-16L], sep = '\n', file = tmp);
    expect_false(obj$synced());
    expect_true(obj$synced("method"));
    expect_false(obj$synced("property"));
    expect_true(obj$synced("a"));
    cat(text, file = tmp);
    expect_true(obj$synced());
  })

  it("can set and remove breakpoints on methods and properties",
  {
    inst <- test();

    obj$add(7L, list(TRUE));
    expect_equal(obj$breaks$line, 7L);
    expect_equal(obj$breaks$name, "method");
    expect_equal(
      body(test@encl$this$method)[[2L]]
     ,quote({ if(isTRUE(try(TRUE, TRUE))) .doTrace(browser()); 1L; })
    );
    expect_equal(
      body(inst$method)[[2L]]
     ,quote({ if(isTRUE(try(TRUE, TRUE))) .doTrace(browser()); 1L; })
    );
    obj$rm(7);
    expect_equal(nrow(obj$breaks), 0L);
    expect_equal(body(test@encl$this$method)[[2L]], 1L);
    expect_equal(body(inst$method)[[2L]], 1L);

    obj$add(14L, list(TRUE));
    expect_equal(obj$breaks$line, 14L);
    expect_equal(obj$breaks$name, "property");
    expect_equal(
      body(activeBindingFunction("property", test@encl$this))[[c(2L, 3L, 2L)]]
     ,quote({ if(isTRUE(try(TRUE, TRUE))) .doTrace(browser()); 3L; })
    );
    expect_equal(
      body(activeBindingFunction("property", inst))[[c(2L, 3L, 2L)]]
     ,quote({ if(isTRUE(try(TRUE, TRUE))) .doTrace(browser()); 3L; })
    );
    obj$rm(14L);
    expect_equal(nrow(obj$breaks), 0L);
    expect_equal(
      body(activeBindingFunction("property", test@encl$this))[[c(2L, 3L, 2L)]]
     ,3L
    );
    expect_equal(
      body(activeBindingFunction("property", inst))[[c(2L, 3L, 2L)]]
     ,3L
    );

    obj$add(c(7L, 14L), list(TRUE, TRUE));
    expect_equal(obj$breaks$line, c(7L, 14L));
    expect_equal(obj$breaks$name, c("method", "property"));
    obj$rm(c(7L, 14L));
    expect_equal(nrow(obj$breaks), 0L);

    obj$add(c(7L, 14L), list(TRUE, TRUE));
    obj$rm();
    expect_equal(nrow(obj$breaks), 0L);
  })

  it("can map srcref lines",
  {
    obj$add(c(7L, 14L), list(TRUE, TRUE));
    writeLines(
      c("# a comment ontop", append(readLines(tmp, warn = FALSE), "'a';", 13L))
     ,tmp
    );
    source(tmp, local = TRUE, keep.source = TRUE);
    obj$load(test);
    expect_equal(obj$breaks$line, c(8L, 16L));
  })

  it("will remove a function if removed from file",
  {
    writeLines(readLines(tmp, warn = FALSE)[-c(13:20)], tmp);
    source(tmp, local = TRUE, keep.source = TRUE);
    obj$load(test);
    expect_equal(nrow(obj$breaks), 1L);
    expect_false(obj$has(lines = 16L));
  })

  it("will add new function if reloaded",
  {
    writeLines(text, tmp);
    source(tmp, local = TRUE, keep.source = TRUE);
    obj$load(test, new = TRUE);
    expect_equal(nrow(obj$breaks), 1L);
    expect_equal(obj$breaks$line, 7L);
    expect_true(obj$has(lines = 14L));
  })

  it("can set breakpoints on virtual methods",
  {
    text <- r"{
    oopr("base",,
    {
    public:
      virtual:method <- \( ) { } = 0;
    })
    oopr("derived", public:base,
    {
    public:
      method <- \( )
      {
        3L;
        {
          4L;
        }
      }
    })
    }";
    cat(text, file = tmp);
    source(tmp, local = TRUE, keep.source = TRUE);
    obj <- OoprBreakpointsClass(derived);
    x <- derived();
    obj$add(12, TRUE);
    expect_true(has_breakpoint(parent.env(parent.env(x)$base)$this$method));
  })

})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("OoprBreakpointsFile",
{
  create_pkg("FAKE_PKG_123");
  text <- r"{
  oopr("test1",,
  {
  public:
    method       <- \( )
    {
      7L;
      {
        9L;
      }
    }
  })
  oopr("test2",,
  {
    method <- \( )
    {
      17L;
      {
        19L;
      }
    }
  })
  }";
  cat(text, file = tmp);
  source(tmp, local = top, keep.source = TRUE);
  obj <- OoprBreakpointsFile(tmp);

  it("loads",
  {
    expect_equal(normalizePath(obj$file), normalizePath(tmp));
    expect_equal(obj$pkg, "FAKE_PKG_123");
    expect_equal(obj$timestamp, file.mtime(tmp));
    expect_identical(obj$top, top);
  })

  it("can be compared to file paths",
  {
    expect_false(obj == tempfile());
    expect_true (obj == obj$file);
    expect_false(obj == 1L);
  })

  it("can set breakpoints",
  {
    obj$add(c(9L, 19L), c(TRUE, TRUE));
    expect_equal(nrow(obj$breaks), 2L);
    expect_equal(obj$breaks$class, c("test1", "test2"));
  })

  it("knows if file has been amended",
  {
    expect_false(obj$modified);
    writeLines(append(readLines(tmp, warn = FALSE), "#", 8L), tmp);
    expect_true(obj$modified);
    expect_all_false(obj$synced());
    obj$load(FALSE);
    expect_false(obj$modified);
    expect_equal(obj$breaks$line, c(10L, 20L));
  })

  it("handles removing an entire class",
  {
    writeLines(readLines(tmp, warn = FALSE)[-c(14:23)], tmp);
    obj$load(FALSE);
    expect_equal(obj$breaks$line, 10L);
  })

  it("can add an entire class",
  {
    cat(text, file = tmp);
    source(tmp, local = top, keep.source = TRUE);
    obj$load(TRUE);
    expect_equal(obj$breaks$line, 9L);
    expect_true(obj$has(19L));
  })

  it("handles symbol change to non-class",
  {
    test2 <- top$test2;
    top$test2 <- "hello world";
    obj$load(TRUE);
    expect_equal(obj$breaks$line, 9L);
    top$test2 <- test2;
    obj$load(TRUE);
    obj$add(19L, TRUE);
  })

  it("knows if namespace is reloaded",
  {
    expect_false(obj$reloaded);
    writeLines(append(readLines(tmp, warn = FALSE), "#", 8L), tmp);
    unloadNamespace("FAKE_PKG_123");
    top <<- fake_namespace("FAKE_PKG_123");
    source(tmp, local = top, keep.source = TRUE);
    expect_true(obj$reloaded);
    obj$load(TRUE);
    expect_equal(obj$breaks$line, c(10L, 20L));
    expect_true(has_breakpoint(top$test1@encl$this$method));
    expect_true(has_breakpoint(top$test2@encl$this$method));
  })

  it("can remove breakpoints",
  {
    obj$rm(c(10L, 20L));
    expect_equal(nrow(obj$breaks), 0L);
    expect_false(has_breakpoint(top$test1@encl$this$method));
    expect_false(has_breakpoint(top$test2@encl$this$method));

    obj$add(c(10L, 20L), list(TRUE, TRUE));
    obj$rm();
    expect_equal(nrow(obj$breaks), 0L);
  })

  it("works with non-package / globalenv",
  {
    tmp <- tempfile();
    on.exit(unlink(tmp));
    cat(text, file = tmp);
    obj <- OoprBreakpointsFile(tmp);
    expect_false(nzchar(obj$pkg));
    expect_identical(obj$top, globalenv());

    source(tmp, globalenv(), keep.source = TRUE);
    on.exit(rm(list = c("test1", "test2"), envir = globalenv()), TRUE);
    expect_true(obj$reloaded);

    obj$load();
    expect_false(obj$reloaded);
    assign("test2", 1L, envir = globalenv());
    expect_true(obj$reloaded);

    dir <- file.path(tempdir(), "a", "R");
    dir.create(dir, recursive = TRUE);
    on.exit(unlink(file.path(tempdir(), "a"), recursive = TRUE), TRUE);
    tmp2 <- tempfile(tmpdir = dir);
    cat("#\n", file = tmp2);
    obj <- OoprBreakpointsFile(tmp2);
    cat("", file = file.path(dirname(dir), "DESCRIPTION"));
    obj <- OoprBreakpointsFile(tmp2);
    expect_false(nzchar(obj$pkg));
  })

})


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("OoprBreakpointsImpl",
{
  try(OoprBreakpointsImpl(), TRUE);
  create_pkg("FAKE_PKG_123");
  volatile <- OoprBreakpointsImpl[["volatile"]];
  quiet    <- OoprBreakpointsImpl[["quiet"]];
  on.exit(TRUE, FALSE, expr =
  {
    OoprBreakpointsImpl[["volatile"]] <- volatile;
    OoprBreakpointsImpl[["quiet"]]    <- quiet;
  });
  OoprBreakpointsImpl[["volatile"]] <- TRUE;
  OoprBreakpointsImpl[["quiet"]]    <- FALSE;
  rm("OoprBreakpointsImpl");
  text <- r"{
  oopr("test",,
  {
  public:
    method1 <- \( )
    {
      7L;
      {
        9L;
      }
    }
    method2 <- \( )
    {
      14L;
      {
        16L;
      }
    }
  })
  }";
  cat(text, file = tmp);
  source(tmp, local = top, keep.source = TRUE);

  it("asserts",
  {
    expect_error(
      OoprBreakpointsImpl$add(tmp, 1L, TRUE)
     ,"`conds` must be a list of expressions"
    );
    expect_error(
      OoprBreakpointsImpl$add(tmp, 1L, list(TRUE, TRUE))
     ,"`conds` must be the same length as `lines`"
    );
  })

  it("adds and removes",
  {
    OoprBreakpointsImpl$add(tmp, c(7L, 9L, 14L));
    expect_output(
      print(OoprBreakpointsImpl)
     ,r"{test.*?method1.*?7 \[TRUE\].*?9 \[TRUE\].*?method2.*?\[TRUE\]}"
    );
    OoprBreakpointsImpl$rm(tmp);
    expect_equal(nrow(OoprBreakpointsImpl$breaks), 0L);
    OoprBreakpointsImpl$add(tmp, c(7L, 9L, 14L));
  })

  it("warns when line cannot be set",
  {
    expect_warning(
      OoprBreakpointsImpl$add(tmp, c(6L))
     ,"Lines 6L unable to be set"
    );
  })

  it("checks source changes and reacts",
  {
    writeLines(c("#", readLines(tmp, warn = FALSE)[-c(12:18)]), tmp);
    msg <- tryCatch(OoprBreakpointsImpl$breaks, message = identity);
    expect_true(inherits(msg, "OoprBreakpointsChange"));
    expect_match(msg$message, r"{\.R \[changed\].*?test \[changed\]}");
    expect_match(msg$message, r"{method1 \[changed\].*?7[^\n]+8.*?9[^\n]+10}");
    expect_match(msg$message, r"{method2 \[removed\]}");
    expect_equal(OoprBreakpointsImpl$breaks$line, c(8, 10));
  })

  it("converts errors to warnings",
  {
    writeLines(c(text, sub("9L", "this$method2<-1L", text)), tmp);
    expect_warning(
      OoprBreakpointsImpl$breaks
     ,class = "OoprBreakpointsCompileError"
    );
    writeLines(c(text, sub("9L", "\\\\", text)), tmp);
    expect_warning(
      OoprBreakpointsImpl$breaks
     ,class = "OoprBreakpointsParseError"
    );
  })

  it("checks environment changes and reacts",
  {
    writeLines(c(text, sub("test", "test2", text)), tmp);
    unloadNamespace("FAKE_PKG_123");
    tmp2 <- tmp;
    create_pkg("FAKE_PKG_123");
    tmp  <- tmp2;
    source(tmp, local = top, keep.source = TRUE);
    msg <- tryCatch(OoprBreakpointsImpl$breaks, message = identity);
    expect_true(inherits(msg, "OoprBreakpointsChange"));
    expect_match(msg$message, r"{\.R \[changed\].*?test \[changed\]}");
    expect_match(msg$message, r"{method1 \[changed\].*?8[^\n]+7.*?10[^\n]+9}");
    expect_match(msg$message, r"{method2 \[added\].*?test2 \[added\]}");
    expect_equal(OoprBreakpointsImpl$breaks$line, c(7, 9));
    OoprBreakpointsImpl$rm(tmp, c(7, 9));
  })

  tryCatch(OoprBreakpointsImpl$breaks, message = identity);
})


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
test_that("OoprBreakpoints",
{
  try(OoprBreakpoints(), TRUE);

  if(!identical(.Platform$GUI, "RStudio"))
  {
    expect_error(OoprBreakpoints[["rstudio"]] <- TRUE);
  }

  expect_output(
    str(`class<-`(data.frame(), c("OoprBreakpointsBreak", "data.frame")))
   ,capture.output(str(data.frame()))
   ,fixed = TRUE
  );

  rstudio  <- OoprBreakpoints[["rstudio"]];
  volatile <- OoprBreakpoints[["volatile"]];
  expect_equal(volatile, OoprBreakpointsImpl[["volatile"]]);

  on.exit({
    OoprBreakpoints[["volatile"]]            <- volatile;
    if(rstudio) OoprBreakpoints[["rstudio"]] <- rstudio;
  });
  if(rstudio) OoprBreakpoints[["rstudio"]]  <- FALSE;
  OoprBreakpoints[["volatile"]]             <- !volatile;
  expect_equal(OoprBreakpoints[["volatile"]], OoprBreakpointsImpl[["volatile"]])

  obj <- OoprBreakpointsCond();
  obj$push("hello");
  obj$child("key");
  obj$clear()
  expect_null(obj$list());

})
