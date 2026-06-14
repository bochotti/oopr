# Get Started with oopr

``` r

library(oopr)
```

## Terminology

This package uses terminology used in Object Oriented Programming (OOP):

Class
:   A data type, similar to `list` or `environment` in R. A class is
    like a blueprint, which defines its contents by name and type.

Member
:   The contents of a class are its members. A member is named and can
    either hold data (*field*), or conduct an action (*method*).
:   - Field: Data members which hold objects. An example being a number
      or a string.
:   - Method: Members which conduct an action. These are functions
      within a class.

Instance
:   An instance is an object that follows the blueprint of a class. Each
    instance holds its own members as defined by the class, and work
    independently of each-other.

## Usage

To get started, a simple example is demonstrated below. More in-depth
discussions of the functionality in this package can be found in the
relevant vignettes.

### Definition and Implementation

Lets create a class which represents a counter. The counter should hold
a number to record how many times it has counted, and a method to
increment the number:

``` r

oopr("Counter",,
{
public:
  x    <- 0L;
  incr <- \( ) { this$x <- this$x + 1L; }
})
print(Counter)
#> <Counter ooprC: 0x562e5ea6da68>
#> Usage:
#>   Counter()
```

There are a few things to note here:

1.  The name of the class is given as a string in the first argument.
2.  The second argument is blank (reserved for inheritance).
3.  The definition of the class is within `{ ... }` for the third
    argument.

Within the definition two members are defined:

1.  `x`: a field representing an integer.
2.  `incr()`: a method which increases `x` by `1L`.

The use of `this` inside a method allows an object of a class to refer
to its own members. In this case, `incr()` can access `x` via `this$x`.

The use of `public:` allows the user of the class to access these
members, more on this later.

### Initialization

To create instances of a class, call the class object as if it were a
function:

``` r

# initialize the class for a new instance
obj <- Counter();
print(obj);
#> <Counter: 0x562e5e73d728>
#>  ├─$x   : int 0
#>  └─$incr:\()
```

To access the members of a class instance, use the `$` operator:

``` r

# use the method
obj$incr();

# access the field
print(obj$x);
#> [1] 1
```

### Access specifiers

This class has an issue: the `$incr()` method assumes `$x` is always an
integer. The user could accidentally break this assumption:

``` r

# whoops!
obj$x <- "a";

# now the instance is broken
obj$incr();
#> Error in `this$x + 1L`:
#> ! non-numeric argument to binary operator
```

To prevent the user amending `$x` outside of using `$incr()`, the
`private:` specifier can be used. A private member can only be accessed
from inside the class.

``` r

oopr("Counter",,
{
public:
  x    <- \( ) { return(this$x_); }
  incr <- \( ) { this$x_ <- this$x_ + 1L; }
private:                                   # <- note private: here
  x_   <- 0L;
})
```

The integer member is now named `$x_` and the use of `private:` prevents
it from being directly accessed by a user while still allowing the
`$incr()` to access it. Users may need to access the count so a public
getter method `$x()` is introduced.

``` r

obj <- Counter();

# can still access x
print(obj$x());
#> [1] 0

# but cannot overwrite it
obj$x <- "a";
#> Error in `obj$x <- "a"`:
#> ! cannot change value of locked binding for 'x'
```

This relies on the restriction that class methods cannot be re-defined.

### Get specifier

A quality of life improvement is the ability to treat the `$x` member as
a normal field instead of needing to call a method.

``` r

oopr("Counter",,
{
public:
  get:x <- \( ) { return(this$x_); }        # <- note get: here
  incr  <- \( ) { this$x_ <- this$x_ + 1L; }
private:
  x_   <- 0L;
})
```

The `get:` specifier can be prepended to a zero-argument function to
treat it as a property.

``` r

obj <- Counter();

# x looks like a normal field
print(obj$x);
#> [1] 0

# but it has no setter, making it more robust
obj$x <- "a";
#> Error:
#> ! `x` is read-only
```

## Specifiers

There are multiple specifiers available to change members of a class,
which are explained in further vignettes.

The `:` operator is used to specify different behaviour of class
members. Both sides of `:` must be symbols, and the right-most symbol
will become the name of the member.
