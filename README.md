
<!-- README.md is generated from README.Rmd. Please edit that file -->

# oopr

<!-- badges: start -->

[![R-CMD-check](https://github.com/bochotti/oopr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bochotti/oopr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This package was created to assist with writing classes in R code.
`methods::setRefClass()` and `R6::R6Class()` already provide such
functionality, but this package implements the idea differently.

1.  Class implementations are defined within a single brace `{`.

2.  Members can have different behavior via the use of specifiers.

3.  Upon creating a class, a series of checks are performed to reduce
    the risk of run-time errors.

4.  Code-completion and breakpoints are supported in RStudio.

Classes are useful for bundling data and actions together, and to
“group” common functions.

## Installation

``` r
# install.packages("remotes")
remotes::install_github("bochotti/oopr")
```

## Getting started

See `vignette("oopr")`.

## Example

``` r
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#  human as a class
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("Human",,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
Human <- \(first, last, age)
{
  stopifnot(
    this$isScalar("character", first)
   ,this$isScalar("character", last)
   ,this$isScalar("integer"  , age)
  );
  this$first_ <- first;
  this$last_  <- last;
  this$age_   <- age;
}
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:name <- \( )
  {
    return(sprintf(
      "%s %s", this$first_, this$last_
    ));
  }
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  greet <- \( )
  {
    cat(sprintf(
      "Hello, my name is %s, aged %i\n"
     ,this$name, this$age_
    ));
    return(invisible(this));
  }
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  first_ <- character(1L);
  last_  <- character(1L);
  age_   <- integer(0L);
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  isScalar <- \(type, x)
  {
    return(
         length(x) == 1L
      && typeof(x) == type
    );
  }
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
}) ## Human
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

john <- Human("john", "smith", 50L);
print(john);
#> <Human: 0x6103bc3ecb58>
#>  ├─$name : chr "john smith"
#>  └─$greet:\()
john$greet();
#> Hello, my name is john smith, aged 50
```
