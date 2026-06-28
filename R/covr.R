## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @name oopr_covr
#' @title covr for oopr
#' @include init.R
#' @description
#' Include `oopr` classes in package coverage, or test the coverage of a
#' single class.
#'
#' @details
#' For package coverage, write code to initialize this class inside
#' `./tests/testthat/setup.R`
#'
#' @examples
#' \dontrun{
#' # ./tests/testthat/setup.R
#' OoprCovr();}
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
NULL
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname oopr_covr
#' @export
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprCovr",,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
OoprCovr <- \( )
{
  if(!this$covrIsRunning()) return();

  root <- this$covr$package_root(".");
  pkg  <- read.dcf(file.path(root, "DESCRIPTION"), "Package")[[1L]];
  ns   <- getNamespace(pkg);

  for(name in names(ns))
  {
    if(is.ooprC(ns[[name]])) this$traceOoprC(ns[[name]]);
  }

  this$rplc <- this$covr$compact(this$rplc);

  lapply(this$rplc, this$covr$replace);

  the <- this$covr$the;
  the$replacements <- c(the$replacements, this$rplc);
}
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Test a single class.
  #'
  #' @param ooprC  `ooprC` \cr
  #'               `ooprC` object.
  #'
  #' @param file   `character(1L)` \cr
  #'               The path to a test file. If `NULL`, then it is guessed.
  #'
  #' @param report `logical(1L)` \cr
  #'               Whether to run [`covr::report`].
  #'
  #' @details
  #' `file` guessing assumes `testhat` is being used in a "local" package.
  #'
  #' @returns
  #' An object of class `coverage`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:class <- \(ooprC, file = NULL, report = TRUE)
  {
    stopifnot(
      requireNamespace("covr", quietly = TRUE)
     ,is.ooprC(ooprC)
     ,is.null(file) || (is.character(file) && length(file) == 1L)
     ,is.logical(report) && length(report) == 1L && !is.na(report)
    );
    file <- file %||% this$findFileFromClass(ooprC);
    if(!file.exists(file))
    {
      stop(sprintf("`file` \"%s\" does not exist", file));
    }

    fun <- this$covr$environment_coverage;
    body(fun)[[c(2L, 3L, 2L)]] <- quote(parent.env(parent.env(env)));
    x <- fun(ooprC@encl$this, file);

    if(report) this$covr$report(x);
    return(x)
  }
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  rplc <- list();
  get:size <- \( ) { return(length(this$rplc)); }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  covrIsRunning <- \( )
  {
    return(
         requireNamespace("covr", quietly = TRUE)
      && Sys.getenv("R_COVR") == "true"
    );
  }
  static:get:covr <- \( ) { return(getNamespace("covr")); }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  traceOoprC <- \(ooprC)
  {
    meta <- ooprC@meta;
    thiz <- ooprC@encl$this;
    for(i in seq_len(meta$size))
    {
      if(nzchar(meta$inherit$get(i))) next;
      name <- meta$names$get(i);

      if(meta$method$get(i))
      {
        fun <- .subset2(thiz, name);
      }
      else if(nzchar(meta$property$get(i)))
      {
        fun <- activeBindingFunction(name, thiz);
      }
      else next;

      this$rplc[[this$size + 1L]] <- this$covr$replacement(name, thiz, fun);
    }
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  static:findFileFromClass <- \(ooprC)
  {
    src <- attr(attr(ooprC, "srcref"), "srcfile")$filename;
    if(is.null(src))                return(NA_character_);
    tst <- sprintf("test-%s", basename(src));
    return(file.path(".", "tests", "testthat", tst));
  }
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
})
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
