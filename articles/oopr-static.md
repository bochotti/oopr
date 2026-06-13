# Static

Static members belong to a class itself, not the instances of a class.
Static members across multiple instances share the same state.

``` r

library(oopr)
```

## Usage

Members can be declared static with the `static:` specifier.

``` r

oopr("StaticExample",,
{
public:
  static:field  <- 1L;
})
```

``` r

obj <- StaticExample();

# can be used from the class
print(StaticExample);
#> <StaticExample ooprC: 0x559bf0af4f68>
#> Usage:
#>   StaticExample() 
#> Static Members:
#>  └─$field: int 1
StaticExample$field <- 2L;

# instances share the same state with the class
print(obj);
#> <StaticExample: 0x559bf1b32918>
#>  └─$field: int 2
```

## Requirements

1.  Static methods may only refer to other static members via `this$`.
2.  Static methods cannot be specified `virtual:`.

## Use-cases

### Global members

It can be useful for multiple instances to share the same variable:

``` r

oopr("GlobalStaticExample",,
{
public:
  static:tally <- 0L;
  method <- \( ) { this$tally <- this$tally + 1L; }
})
```

``` r

obj1 <- GlobalStaticExample();
obj2 <- GlobalStaticExample();

obj1$method();
print(obj2$tally);
#> [1] 1

obj2$method();
print(obj1$tally);
#> [1] 2
```

### Grouping common functions

Grouping common functions together in a single class can keep a
namespace clean.

``` r

oopr("UtilsExample",,
{
public:
  static:add      <- \(x, y) { return(x + y); }
  static:multiply <- \(x, y) { return(x * y); }
  static:subtract <- \(x, y) { return(x - y); }
})
```

``` r

UtilsExample$add(1, 1);
#> [1] 2
UtilsExample$multiply(1, 1);
#> [1] 1
UtilsExample$subtract(1, 1);
#> [1] 0
```
