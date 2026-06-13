# Methods

``` r

library(oopr)
```

## Protection

When running the
[`oopr()`](https://bochotti.github.io/oopr/reference/oopr.md) function,
the body of methods are traversed to ensure that the members used exist
and are correctly used.

This is designed to reduce bugs introduced from typos or changing
code-bases, which aren’t caught until creating instances. Many stops are
employed to maintain the assumptions of `oopr` classes.

``` r

oopr("Bad",,
{
public:
  get:prop <- \( ) { return(1L); }
  method   <- \( )
  {
    this$X;          # this does not exist
    this$prop();     # this is not a method
    this$method(1L); # this does not have any arguments
    this$prop <- 1L; # this property has no setter
  }
})
#>   Member `method` is attempting to refer to an undefined member
#>   `this$X`.
#> 
#>   Member `method` is attempting to call property `this$prop()`.
#> 
#>   Member `method` call to method `this$method(1L)` does not match its
#>   signature: "unused argument (1)".
#> 
#>   Member `method` is attempting to assign into get property
#>   `this$prop`.
#> Error in `oopr()`:
#> ! Compilation errors
```

## `this` and Method Chaining

Inside methods, `this` allows an instance to refer to itself. `this` is
an environment with no S3 class which makes it faster to access. It
contains all members regardless of their access specifier.

The object returned from constructing an instance is a different
environment, with S3 classes and only containing the public members. It
can be accessed via `.this`.

The body of each method is traversed for calls `return(this)` and
`return(invisible(this))`. If found, `this` is replaced with `.this`. If
the developer wishes to return the instance but avoid using `return`,
then they should use `.this` instead.

Using `return(invisible(this))` allows for method chaining:

``` r

oopr("Sentence",, 
{
public:
  x      <- character(1L);
  print  <- \( ) 
  { 
    cat(trimws(this$x), "\n"); 
    return(invisible(this)); 
  }
  append <- \(word) 
  { 
    this$x <- sprintf("%s %s", this$x, trimws(word)); 
    return(invisible(this)); 
  }
})
```

``` r

obj <- Sentence();
obj$append("hello")$print()$append("world")$print();
#> hello 
#> hello world
```

## Constructor

Constructor methods can be used to perform actions when constructing an
instance of a class. For example, allowing a user to provide initial
values for members, or opening connections elsewhere.

To define a constructor method, define a method with the same name as
the class. Constructor methods can only be private, as they cannot be
re-used after an instance is created.

``` r

oopr("ConstructorExample",,
{
ConstructorExample <- \(x) 
{
  cat(sprintf("Constructing with x = %s\n", deparse1(x)));
}
})
```

``` r

ConstructorExample(2L);
#> Constructing with x = 2L
#> <ConstructorExample: 0x55cdefc5c6c0>
```

## Destructor

Destructor methods run an action when an instance is destroyed. This is
useful for tidying any externalities created by a class. In R, an object
is destroyed if it cannot be reached when the garbage collector runs,
this is different in some other languages.

To define a destructor method, define a method as you would a
constructor method, prefixed with `~`. A destructor method *cannot* have
any arguments.

``` r

oopr("DestructorExample",,
{
~DestructorExample <- \( )
{
  print("Destructing!");
}
})
```

``` r

obj <- DestructorExample();
# do work
rm(obj);
invisible(gc());
#> [1] "Destructing!"
```
