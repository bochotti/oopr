# Class Members

Fields of a class can be defined as another class. When this happens the
member becomes a class instance.

``` r

library(oopr)
```

## Usage

Simply assign a class object to a member.

``` r

oopr("MemberClass",, { public:x <- 1L; })
oopr("ClassMemExample",,
{
public:
  mem <- MemberClass;
})
```

``` r

obj <- ClassMemExample();
print(obj);
#> <ClassMemExample: 0x5645fda02e08>
#>  └─$mem:<MemberClass: 0x5645fda01a48>
#>          └─$x: int 1
```

## Protection

Class members are automatically protected via an active binding. When
assigning into a class member, the incoming value is checked for the
same class:

``` r

# allowed
obj$mem$x <- 2L;

# not allowed
obj$mem <- 1L;
#> Error:
#> ! Incoming value to member `mem` must be oopr class `MemberClass`

# allowed
obj$mem <- MemberClass();

# not allowed
obj$mem <- ClassMemExample();
#> Error:
#> ! Incoming value to member `mem` must be oopr class `MemberClass`
```

Using set properties (or the absence of one for read-only) with classes
may not work as intended as R expressions are evaluated from right to
left. An expression such as `obj$mem$a <- 1L;` will evaluate
`mem$a <- 1L;` before the set property for `obj$mem` is called.

A useful set property to ensure the class instance is never replaced
could be:

``` r

oopr("ClassMemPropertyExample",,
{
public:
  get:x <- \( ) { return(this$x_); }
  set:x <- \(rhs)
  {
    if(!identical(this$x_, rhs)) 
    { 
      stop(call. = FALSE, "`x` cannot be replaced"); 
    }
    return(this$x_);
  }
private:
  x_ <- MemberClass;
})
```

``` r

obj <- ClassMemPropertyExample();

# allowed
obj$x$x <- 2L;

# not allowed
obj$x <- MemberClass();
#> Error:
#> ! `x` cannot be replaced
```

## Containers

Members can become containers of classes using syntactic sugar.
Suffixing `[]` uses
[`OoprVec()`](https://bochotti.github.io/oopr/reference/oopr_containers.md)
and `[[]]` uses
[`OoprMap()`](https://bochotti.github.io/oopr/reference/oopr_containers.md).

``` r

oopr("ClassMemExample",,
{
public:
  ClassMemExample <- \( )
  {
    for(i in 1:3)
    {
      this$mem$emplace();
      this$mem[i]$x <- i;
    }
  }
  mem <- MemberClass[]; # <- use of []
})
```

``` r

obj <- ClassMemExample();
print(obj);
#> <ClassMemExample: 0x5645fb967360>
#>  └─$mem:<OoprVec: 0x5645fb9223c0>
#>          ├─$class  : chr "MemberClass"
#>          ├─$empty  : logi FALSE
#>          ├─$size   : int 3
#>          ├─$data   :List of 3
#>          │           $:<MemberClass: 0x5645fb923eb8>
#>          │            ..└─$x: int 1
#>          │           $:<MemberClass: 0x5645fb936b30>
#>          │            ..└─$x: int 2
#>          │           $:<MemberClass: 0x5645fb950760>
#>          │            ..└─$x: int 3
#>          ├─$insert :\(pos = this$size, x)  
#>          ├─$emplace:\(. = this$size)  
#>          ├─$resize :\(n)  
#>          ├─$erase  :\(pos = this$size)  
#>          ├─$swap   :\(pos1, pos2)  
#>          ├─$apply  :\(fun, ...)  
#>          ├─$[      :\(i, j, ..., drop)  
#>          └─$[<-    :\(i, j, ..., value)
```

## Construction

If the class member has a constructor method, then the member will need
to:

1.  Be initialized within the constructor method, or
2.  Have its arguments provided where the member is being defined.

If the member is specified as `static:`, then 2. is the only option.

``` r

oopr("MemberClass",, 
{ 
public:
  MemberClass <- \(x) { this$x <- x; }
  x <- 1L; 
})
```

``` r

# not allowed, needs constructing
oopr("ClassMemExample",,
{
public:
  mem <- MemberClass;
})
#>   Class `mem` must be initialized in the constructor method via
#>   `this$mem(...)`.
#> Error in `oopr()`:
#> ! Compilation errors
```

``` r

# both approaches
oopr("ClassMemExample",,
{
public:
  ClassMemExample <- \( )
  {
    this$mem1(1L);         # <- args given in constructor
  }
  mem1 <- MemberClass;
  mem2 <- MemberClass(2L); # <- args given in-line
})
```

``` r

obj <- ClassMemExample();
print(obj);
#> <ClassMemExample: 0x5645fb35e788>
#>  ├─$mem1:<MemberClass: 0x5645fb35b898>
#>  │        └─$x: int 1
#>  └─$mem2:<MemberClass: 0x5645fb357f28>
#>           └─$x: int 2
```
