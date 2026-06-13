# Completion for oopr

Code-completion / intellisense for `oopr` classes.

## Usage

``` r
this
```

## Format

An object of class `oopr_this` (inherits from `oopr`) of length 0.

## Details

While typing inside an `oopr` definition, use dollarnames on `this` to
know what members are available. Inherited classes can also be accessed,
if they exist on the search path.

Currently only implemented for RStudio.

## Examples

``` r
if (FALSE) { # \dontrun{
oopr("memb",,
{
public:
  a <- 1L;
  b <- list(c = 1L, b = "b");
})

oopr("test", memb,
{
public:
  memb   <- memb;
  method <- \( )
  {
    #    v press TAB here
    this$memb$b
    #         ^ or here

    memb$b
    #    ^ and even here
  }
})} # }
```
