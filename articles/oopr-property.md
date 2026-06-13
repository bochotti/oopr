# Properties

Properties allow members to be used as fields, but access and assignment
actually execute functions. This feature is implemented using
[`base::makeActiveBinding()`](https://rdrr.io/r/base/bindenv.html).

``` r

library(oopr)
```

## Usage

Properties can be set using the `get:` and `set:` specifiers.

``` r

oopr("PropertyExample",,
{
public:
  get:x <- \( ) { return(this$x_); }
  set:x <- \(x) { this$x_ <- x; }
private:
  x_ <- integer(1L);
})
```

``` r

obj <- PropertyExample();

# calls set
obj$x <- 1L;

# calls get
print(obj$x);
#> [1] 1
```

## Requirements

1.  Members with the `get:` specifier must be a zero argument function.
2.  Members with the `set:` specifier must be a one argument function
    with no default value.
3.  A member with both `get:` and `set:`
    - Must be defined sequentially.
    - Must have the same access specifier.
    - Must both be `static:` if at least one is `static:`.

## Use-cases

Properties are useful for encapsulation of fields. Use-cases include:

### Read-only

A field could serve as information passed to the user of the class,
without allowing the user to edit it.

``` r

oopr("ReadOnly",,
{
public:
  get:x <- \( ) { return(this$x_); }
private:
  x_ <- integer(1L);
})
```

``` r

obj <- ReadOnly();

# can read
print(obj$x)
#> [1] 0

# but not write
obj$x <- 1L;
#> Error:
#> ! `x` is read-only
```

### Data Validation

As R is not type-safe, the member of a class can be set to any arbitrary
object by a user. This has the potential for errors if methods assume a
specific type, such as conducting arithmetic on a numeric field.

Using a property setter can assert incoming values to ensure they meet
the expectations of a member. Such as being numeric and a single length
vector.

``` r

oopr("DataValidation",,
{
public:
  get:x <- \( ) { return(this$x_); }
  set:x <- \(x)
  {
    if(!(is.integer(x) && length(x) == 1L))
    {
      stop("`x` must be a single integer!", call. = FALSE);
    }
    this$x_ <- x;
    return(x);
  }
private:
  x_ <- integer(1L);
})
```

``` r

obj <- DataValidation();

# cannot write with unwanted objects
obj$x <- 1:2;
#> Error:
#> ! `x` must be a single integer!
```

## When to use

Properties are like a hybrid between fields and methods, so when should
they be preferred?

### Public fields

All fields to be accessed and/or assigned by users should always be
properties so the data can be asserted before entering the class.

Fields for classes internal for a package are optional, as the author
should understand the requirements.

### Multiple Arguments

If setting a field requires more than one argument (being the incoming
value), then use a method.

### Computationally Heavy

If the get property uses lots of resources, use a method instead.
Operations that inspect an instance (such as printing) will likely call
the getter function.

### Side effects

If setting a field results in side-effects, it would be better
implemented as a method. This way the name of the method can describe
the action.
