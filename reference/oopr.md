# oopr

Create a class generator.

## Usage

``` r
oopr(
  name
 ,inherits   = NULL
 ,definition
 ,parent     = parent.frame()
)
```

## Arguments

- name:

  `character(1L)`  
  The name of the class.

- inherits:

  `expression`  
  Other classes to inherit.

- definition:

  `expression`  
  An expression defining members.

- parent:

  `environment`  
  The environment to assign this class to, and acts as a parent
  environment for each member.

## Value

`NULL` invisibly. An `ooprC` object with the slots below is assigned to
symbol `name` in `parent`.

To construct a new class instance, simply call the `ooprC` object as a
normal function.

**DO NOT** use an assignment operator.

## Details

Each assignment inside `definition` will become members of the class,
see **Examples**.

Specifiers can be prefixed to the name of each member, separated by `:`.
For more information on specifiers see `...`.

## Slots

- `name`:

  `character(1L)`  
  The name of the class.

- `inhr`:

  [`character()`](https://rdrr.io/r/base/character.html)  
  Base classes, if applicable.

- `meta`:

  `environment`  
  Information on all members of the class.

- `encl`:

  `environment`  
  A template of what the class looks like.

## Examples

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
#> <Human: 0x563747d2fe90>
#>  ├─$name : chr "john smith"
#>  └─$greet:\()  
john$greet();
#> Hello, my name is john smith, aged 50
```
