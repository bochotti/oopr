# Inheritance

Inheritance is a way to re-ruse code and establish a hierarchical
relationship between classes.

A relationship handles at least two classes: a base class and a derived
class. The derived class *inherits* features from the base class, such
as receiving the members of the derived class. Typically, inheritance is
justified when establishing an *is a* relationship, i.e., the derived
class *is a* base class.

``` r

library(oopr)
```

## Usage

The second argument of
[`oopr()`](https://bochotti.github.io/oopr/reference/oopr.md) is used
for inheritance. It expects another class via its symbol. Multiple
inheritance is available by using a brace `{`.

``` r

# create some classes
oopr("Base1",, { public:shout   <- \( ) print("HELLO!"); })
oopr("Base2",, { public:whisper <- \( ) print("world");  })
```

``` r

# single inheritance
oopr("InheritanceExample", Base1,
{
public:
  speak <- \( ) { this$shout(); }
})
```

``` r

obj <- InheritanceExample();
obj$speak();
#> [1] "HELLO!"
```

``` r

# multiple inheritance
oopr("InheritanceExample", { Base1; Base2; },
{
public:
  speak <- \( )
  {
    this$shout();
    this$whisper();
  }
})
```

``` r

obj <- InheritanceExample();
obj$speak();
#> [1] "HELLO!"
#> [1] "world"
```

## Access

The access of base classes can be amended using `public:`, `protected:`,
and `private:` specifiers, this influences the access of all the base
classes members. By default, base classes are inherited privately.

If `public:`, then the base S3 class is included in the derived instance
S3 class list.

``` r

# Inherits privately by default
print(obj)
#> <InheritanceExample: 0x563fea7329e8>
#>  └─$speak:\()
```

``` r

# Inherit as public
oopr("InheritanceExample", public:Base1,
{
public:
  speak <- \( ) { this$shout(); }
})
```

``` r

obj <- InheritanceExample();

# Base public members are visible
print(obj);
#> <InheritanceExample: 0x563feb2650b8>
#>  ├─$speak:\()  
#>  └─$shout:\()

# it also carries the S3 class
print(class(obj));
#> [1] "InheritanceExample" "Base1"              "oopr"
```

The interaction between member access and class inheritance access is:

| Member \\ Class | public    | protected | private   |
|:----------------|:----------|:----------|:----------|
| public          | public    | protected | private   |
| protected       | protected | protected | private   |
| private         | no access | no access | no access |

Inheritance Access Interactions {.table}

This has some implications:

1.  Public inheritance allows members of a derived class to copy the
    base class.
2.  Protected members can be accessed by a derived class, but private
    members cannot.
3.  Inheriting a class privately will allow use of its members to the
    derived class, but hide them from further derived classes.

Note that constructor and destructor methods are never inherited.

## Redefining Base Members

A derived class may create its own implementation of the base class
method. The base class implementation can still be used inside a derived
class methods via `base$` instead of `this$`:

``` r

# base class with a method
oopr("Base",,
{
public:
  method <- \( ) { print("Base"); }
})
```

``` r

# derived class, which redefines the method
oopr("Derived", public:Base,
{
public:
  method <- \( )
  {
    Base$method();
    print("Method");
  }
})
```

``` r

# only one method
obj <- Derived();
print(obj);
#> <Derived: 0x563fec6e30d8>
#>  └─$method:\()

# which calls the base class
obj$method();
#> [1] "Base"
#> [1] "Method"
```

### Final

The `final:` specifier is available to stop derived classes redefining
methods:

``` r

oopr("Base",,
{
public:
  final:method <- \( ) { print("Base"); } #<- note final:
})
```

``` r

# not allowed
oopr("Derived", public:Base,
{
public:
  method <- \( ) { print("Method"); }
})
#>   Method `method` is overridding inherited class `Base` final method.
#> Error in `oopr()`:
#> ! Compilation errors
```

## Virtual Methods

The base class may allow derived classes to implement their own versions
of a method, but require them to have the same function signature[^1],
which is useful for looping over multiple instances of different classes
that all share a base class.

``` r

# base class with virtual method
oopr("Base",,
{
public:
  virtual:method <- \(x, y = 1L) { }
})
```

``` r

# does not match arguments
oopr("Derived", public:Base,
{
public:
  method <- \( ) { }
})
#>   Method `method` signature () does not match inherited class `Base`
#>   virtual method signature (x = , y = 1L): "Not the same amount of
#>   arguments".
#> Error in `oopr()`:
#> ! Compilation errors
```

``` r

# does not have a default argument
oopr("Derived", public:Base,
{
public:
  method <- \(x = 1L, y) { }
})
#>   Method `method` signature (x = 1L, y = ) does not match inherited
#>   class `Base` virtual method signature (x = , y = 1L): "Argument "y"
#>   must have a default value".
#> Error in `oopr()`:
#> ! Compilation errors
```

Virtual methods inside the base class instance are *overridden* if the
derived class creates its own implementation of the method.

``` r

# method that relies on a virtual method
oopr("Base",,
{
public:
  speak  <- \( ) { this$method(); }
protected:
  virtual:method <- \( ) { print("Base"); }
})
```

``` r

# overridden for the derived class
oopr("Derived", public:Base,
{
protected:
  method <- \( ) { print("Derived"); }
})
```

``` r

obj <- Derived();
obj$speak()
#> [1] "Derived"
```

Although popular in other languages, virtual methods with `oopr` cannot
be specified as `private:`.

### Abstract

Virtual methods can be marked as abstract. An abstract method requires
derived classes to create their own implementation. Any class with an
abstract method cannot have an instance constructed.

To mark a virtual method as abstract, append `= 0` after its body.

``` r

# mark an abstract method
oopr("AbstractClass",,
{
public:
  doWork <- \(x, y, z)
  {
    o <- this$doStep1(x, y);
    o <- this$doStep2(o, z);
    return(o);
  }
protected:
  virtual:doStep1 <- \(x, y) { } = 0 #<- = 0 here
  virtual:doStep2 <- \(o, z) { } = 0 #<- = 0 here
})
```

``` r

# Cannot create an instance
AbstractClass()
#> Error in `AbstractClass()`:
#> ! AbstractClass constructor is abstract

# Cannot be a class member
oopr("AbstractClassMem",,
{
public:
  abs <- AbstractClass
})
#>   Member `abs` cannot be class `AbstractClass` as it has abstract
#>   methods c("doStep1", "doStep2").
#> Error in `oopr()`:
#> ! Compilation errors

# Derived class cannot be final
oopr("AbstractClassInhr", public:AbstractClass,
{
public:
  final:AbstractClassInhr <- \( ) { }
})
#>   Constructor method `AbstractClassInhr` cannot be final as methods
#>   c("doStep1", "doStep2") are abstract.
#> Error in `oopr()`:
#> ! Compilation errors
```

``` r

# correct
oopr("DerivedClass", public:AbstractClass,
{
protected:
  doStep1 <- \(x, y) { return(x + y); }
  doStep2 <- \(o, z) { return(o * z);   }
})

obj <- DerivedClass();
obj$doWork(1, 2, 3);
#> [1] 9
```

## Constructing

When the base class has a constructor method, then it must be explicitly
initialized in the derived class[^2].

``` r

# base class with constructor method
oopr("Base",,
{
public:
  Base <- \(x) { this$x <- x; }
  x    <- integer(1L);
})
```

``` r

# unable to create the derived class
oopr("Derived", Base, {})
#>   Class `Base` must be initialized in the constructor method via
#>   `Base(...)`.
#> Error in `oopr()`:
#> ! Compilation errors
```

To initialize, simply call the base class constructor within the derived
customer method:

``` r

# call Base(...)
oopr("Derived", public:Base,
{
public:
  Derived <- \(x) { Base(x); }
})
```

``` r

# success
obj <- Derived(1L);
print(obj);
#> <Derived: 0x563feb273cd0>
#>  └─$x: int 1
```

Note that base class members cannot be accessed until they are
constructed:

``` r

# cannot access `x` before Base(...)
oopr("Derived", public:Base,
{
public:
  Derived <- \(x)
  {
    print(Base$x); # <- neither Base$
    print(this$x); # <- nor this$
    Base(x);
  }
})
#>   Constructor method `Derived` is using an inherited member `Base$x`
#>   prior to initializing the inherited class `Base`.
#> 
#>   Constructor method `Derived` is using an inherited member `this$x`
#>   prior to initializing the inherited class `Base`.
#> Error in `oopr()`:
#> ! Compilation errors
```

### Protected Constructor

To ensure that a class can only be constructed as a base class, mark its
constructor method as `protected:`:

``` r

# protected constructor
oopr("Base",,
{
protected:
  Base <- \( ) { }
})
```

``` r

# not allowed
Base();
#> Error in `Base()`:
#> ! Base constructor is protected
```

``` r

# allowed
oopr("Derived", Base, { })
Derived();
#> <Derived: 0x563feac5b648>
```

### Final Constructor

To prevent a class from being inherited by other classes, mark its
constructor as `final:`:

``` r

# final constructor
oopr("Base",,
{
public:
  final:Base <- \( ) { this$x <- 10L; }
  x <- integer(1L);
})
```

``` r

# not allowed
oopr("Derived", Base, { })
#>   Cannot inherit class `Base` as its constructor is final.
#> Error in `oopr()`:
#> ! Compilation errors
```

``` r

# allowed
Base();
#> <Base: 0x563fe7eb4cf8>
#>  └─$x: int 10
```

Note: private and protected constructors cannot be marked as final, as
this would not be intuitive.

[^1]: As R is not type safe, just the argument names apply, not the
    return type.

[^2]: Unless the base class’ constructor method has no arguments, or all
    arguments have a default value.
