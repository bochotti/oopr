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
#> <ClassMemExample: 0x55e0e71296a0>
#>  └─$mem:<MemberClass: 0x55e0e7125de8>
#>          └─$x: int 1
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
ClassMemExample <- \( )
{
  for(i in 1:3)
  {
    this$mem$emplace();
    this$mem[i]$x <- i;
  }
}
public:
  mem <- MemberClass[]; # <- use of []
})
```

``` r

obj <- ClassMemExample();
print(obj);
#> <ClassMemExample: 0x55e0e7e5fe68>
#>  └─$mem:<OoprVec: 0x55e0e7e29ac8>
#>          ├─$class  : chr "MemberClass"
#>          ├─$empty  : logi FALSE
#>          ├─$size   : int 3
#>          ├─$data   :List of 3
#>          │           $:<MemberClass: 0x55e0e7e2d7b8>
#>          │            ..└─$x: int 1
#>          │           $:<MemberClass: 0x55e0e7e42df0>
#>          │            ..└─$x: int 2
#>          │           $:<MemberClass: 0x55e0e7e52a70>
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
MemberClass <- \(x) { this$x <- x; }
public:x    <- 1L; 
})

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
ClassMemExample <- \( )
{
  this$mem1(1L);           # <- args given in constructor
}
public:
  mem1 <- MemberClass;
  mem2 <- MemberClass(2L); # <- args given in-line
})
```

``` r

obj <- ClassMemExample();
print(obj);
#> <ClassMemExample: 0x55e0e61f1980>
#>  ├─$mem1:<MemberClass: 0x55e0e61ec018>
#>  │        └─$x: int 1
#>  └─$mem2:<MemberClass: 0x55e0e61e8398>
#>           └─$x: int 2
```
