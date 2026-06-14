# Containers for oopr Instances

Create a vector or key-value pair of `oopr` instances.

## Usage

``` r
OoprVec(ooprC)

OoprMap(ooprC)

# S3 method for class 'OoprVec'
x[i, j, ..., drop]

# S3 method for class 'OoprVec'
x[i, j, ...] <- value

# S3 method for class 'OoprMap'
x[i, j, ..., drop]

# S3 method for class 'OoprMap'
x[i, j, ...] <- value
```

## Arguments

- ooprC:

  `ooprC`  
  An `oopr` class.

## 

------------------------------------------------------------------------

------------------------------------------------------------------------

OoprVec

### Fields

- `class`:

  `character(1L)`  
  The name of the underlying `oopr` class.

- `empty`:

  `logical(1L)`  
  Whether there are no instanced classes in the container.

- `size`:

  `integer(1L)`  
  The amount of instanced classes in the container.

- `data`:

  [`list()`](https://rdrr.io/r/base/list.html)  
  The container.

### Methods

- `insert`:

  Insert an already instanced class.

- `emplace`:

  Construct a new class into the container.

- `resize`:

  Pre-allocate or destroy the container.

- `erase`:

  Remove a class from the container.

- `swap`:

  Swap two elements of the container.

- `apply`:

  Apply a function over every element in the container.

- `[`:

  Access an element of the container.

- `[<-`:

  Assign to an element of the container.

------------------------------------------------------------------------

insert

#### Description

Insert an already instanced class.

#### Usage

``` R

insert(pos = this$size, x)
```

#### Arguments

### 

[TABLE]

#### Returns

`this` invisibly.

------------------------------------------------------------------------

emplace

#### Description

Construct a new class into the container.

#### Usage

``` R

emplace(. = this$size, ...)
```

#### Arguments

### 

[TABLE]

#### Returns

`this` invisibly.

------------------------------------------------------------------------

resize

#### Description

Pre-allocate or destroy the container.

#### Usage

``` R

resize(n)
```

#### Arguments

### 

[TABLE]

#### Returns

`this` invisibly.

------------------------------------------------------------------------

erase

#### Description

Remove a class from the container.

#### Usage

``` R

erase(pos = this$size)
```

#### Arguments

### 

[TABLE]

#### Returns

`this` invisibly.

------------------------------------------------------------------------

swap

#### Description

Swap two elements of the container.

#### Usage

``` R

swap(pos1, pos2)
```

#### Arguments

### 

[TABLE]

#### Returns

`this` invisibly.

------------------------------------------------------------------------

apply

#### Description

Apply a function over every element in the container.

#### Usage

``` R

apply(fun, ...)
```

#### Arguments

### 

[TABLE]

#### Returns

[`list()`](https://rdrr.io/r/base/list.html) of the outputs from `fun`.

------------------------------------------------------------------------

\[

#### Description

Access an element of the container.

#### Usage

``` R

`[`(i, j, ..., drop)
```

#### Arguments

### 

[TABLE]

#### Returns

An `oopr` object of class `$class`.

------------------------------------------------------------------------

\[\<-

#### Description

Assign to an element of the container.

#### Usage

``` R

`[<-`(i, j, ..., value)
```

#### Arguments

### 

[TABLE]

#### Details

Can be used to set members of classes within the container, e.g.
`x[i]$mem <- ...`.

#### Returns

`this` invisibly.

------------------------------------------------------------------------

------------------------------------------------------------------------

OoprMap

### Fields

- `class`:

  `character(1L)`  
  The name of the underlying `oopr` class.

- `empty`:

  `logical(1L)`  
  Whether there are no instanced classes in the container.

- `size`:

  `integer(1L)`  
  The amount of instanced classes in the container.

- `keys`:

  [`character()`](https://rdrr.io/r/base/character.html)  
  The keys within the container

- `data`:

  [`list()`](https://rdrr.io/r/base/list.html)  
  The container.

### Methods

- `exists`:

  Check whether a key exists.

- `insert`:

  Insert an already instanced class.

- `emplace`:

  Construct a new class into the container.

- `erase`:

  Remove a class from the container.

- `resize`:

  Pre-allocate or destroy the container.

- `apply`:

  Apply a function over every element in the container.

- `[`:

  Access an element of the container.

- `[<-`:

  Assign to an element of the container.

------------------------------------------------------------------------

exists

#### Description

Check whether a key exists.

#### Usage

``` R

exists(key)
```

#### Arguments

### 

[TABLE]

#### Returns

`logical(1L)`.

------------------------------------------------------------------------

insert

#### Description

Insert an already instanced class.

#### Usage

``` R

insert(key, x)
```

#### Arguments

### 

[TABLE]

#### Returns

`this` invisibly.

------------------------------------------------------------------------

emplace

#### Description

Construct a new class into the container.

#### Usage

``` R

emplace(., ...)
```

#### Arguments

### 

[TABLE]

#### Returns

`this` invisibly.

------------------------------------------------------------------------

erase

#### Description

Remove a class from the container.

#### Usage

``` R

erase(key)
```

#### Arguments

### 

[TABLE]

#### Returns

`this` invisibly.

------------------------------------------------------------------------

resize

#### Description

Pre-allocate or destroy the container.

#### Usage

``` R

resize(keys = character(0L))
```

#### Arguments

### 

[TABLE]

#### Returns

`this` invisibly.

------------------------------------------------------------------------

apply

#### Description

Apply a function over every element in the container.

#### Usage

``` R

apply(fun, ...)
```

#### Arguments

### 

[TABLE]

#### Returns

[`list()`](https://rdrr.io/r/base/list.html) of the outputs from `fun`.

------------------------------------------------------------------------

\[

#### Description

Access an element of the container.

#### Usage

``` R

`[`(i, j, ..., drop)
```

#### Arguments

### 

[TABLE]

#### Returns

An `oopr` object of class `$class`.

------------------------------------------------------------------------

\[\<-

#### Description

Assign to an element of the container.

#### Usage

``` R

`[<-`(i, j, ..., value)
```

#### Arguments

### 

[TABLE]

#### Details

Can be used to set members of classes within the container, e.g.
`x[i]$mem <- ...`.

#### Returns

`this` invisibly.

## Examples

``` r
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("test",,
{
test <- \(x) { this$x <- x; }
public:x <- 0L;
})
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
# create a vector
vec <- OoprVec(test);

# $emplace create a new instance
vec$emplace(, 1L);

# $insert passes existing instance
vec$insert(0L, test(2L));

print(vec);
#> <OoprVec: 0x55edd7bde070>
#>  ├─$class  : chr "test"
#>  ├─$empty  : logi FALSE
#>  ├─$size   : int 2
#>  ├─$data   :List of 2
#>  │           $:<test: 0x55edd7fcffd0>
#>  │            ..└─$x: int 2
#>  │           $:<test: 0x55edd7f8b178>
#>  │            ..└─$x: int 1
#>  ├─$insert :\(pos = this$size, x)  
#>  ├─$emplace:\(. = this$size, x)  
#>  ├─$resize :\(n)  
#>  ├─$erase  :\(pos = this$size)  
#>  ├─$swap   :\(pos1, pos2)  
#>  ├─$apply  :\(fun, ...)  
#>  ├─$[      :\(i, j, ..., drop)  
#>  └─$[<-    :\(i, j, ..., value)  

# $swap will swap elements
vec$swap(2L, 1L);
print(vec);
#> <OoprVec: 0x55edd7bde070>
#>  ├─$class  : chr "test"
#>  ├─$empty  : logi FALSE
#>  ├─$size   : int 2
#>  ├─$data   :List of 2
#>  │           $:<test: 0x55edd7f8b178>
#>  │            ..└─$x: int 1
#>  │           $:<test: 0x55edd7fcffd0>
#>  │            ..└─$x: int 2
#>  ├─$insert :\(pos = this$size, x)  
#>  ├─$emplace:\(. = this$size, x)  
#>  ├─$resize :\(n)  
#>  ├─$erase  :\(pos = this$size)  
#>  ├─$swap   :\(pos1, pos2)  
#>  ├─$apply  :\(fun, ...)  
#>  ├─$[      :\(i, j, ..., drop)  
#>  └─$[<-    :\(i, j, ..., value)  

# $apply to loop over instances
vec$apply(\(x) { x$x <- x$x + 1L; })
#> [[1]]
#> [1] 2
#> 
#> [[2]]
#> [1] 3
#> 
vec$apply(\(x) { x$x; })
#> [[1]]
#> [1] 2
#> 
#> [[2]]
#> [1] 3
#> 
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
# create a key-value pair
map <- OoprMap(test);
map$emplace("a", "a")$emplace("b", "b");
print(map);
#> <OoprMap: 0x55edd887fe40>
#>  ├─$class  : chr "test"
#>  ├─$empty  : logi FALSE
#>  ├─$size   : int 2
#>  ├─$keys   : chr [1:2] "a" "b"
#>  ├─$data   :List of 2
#>  │           $a:<test: 0x55edd88ab750>
#>  │            ..└─$x: chr "a"
#>  │           $b:<test: 0x55edd88aebf0>
#>  │            ..└─$x: chr "b"
#>  ├─$exists :\(key)  
#>  ├─$insert :\(key, x)  
#>  ├─$emplace:\(., x)  
#>  ├─$erase  :\(key)  
#>  ├─$resize :\(keys = character(0L))  
#>  ├─$apply  :\(fun, ...)  
#>  ├─$[      :\(i, j, ..., drop)  
#>  └─$[<-    :\(i, j, ..., value)  

# apply is a two argument function
map$apply(\(k, o) { o$x <- toupper(k); });
#> $a
#> [1] "A"
#> 
#> $b
#> [1] "B"
#> 
map$apply(\(k, o) { o$x == k; });
#> $a
#> [1] FALSE
#> 
#> $b
#> [1] FALSE
#> 
```
