# Is it an oopr?

Check whether an object is `oopr` or `ooprC`.

## Usage

``` r
is.oopr(x, name = character(0L))

is.ooprC(x, name = character(0L))
```

## Arguments

- x:

  Any object.

- name:

  [`character()`](https://rdrr.io/r/base/character.html)  
  Check for any class name.

## Value

`logical(1L)`

## Examples

``` r
oopr('a',, {})
obj <- a();

is.ooprC(a);
#> [1] TRUE
is.ooprC(a, 'a');
#> [1] TRUE
is.ooprC(a, 'b');
#> [1] FALSE
is.ooprC(obj);
#> [1] FALSE

is.oopr(obj);
#> [1] TRUE
is.oopr(obj, 'a');
#> [1] TRUE
is.oopr(obj, 'b');
#> [1] FALSE
is.oopr(a);
#> [1] FALSE
```
