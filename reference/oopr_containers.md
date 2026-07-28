# Containers for oopr Instances

Create a container of `oopr` instances.

## Usage

``` r
OoprVec(ooprC)

OoprMap(ooprC)
```

## Arguments

- ooprC:

  `ooprC`  
  An `oopr` class.

## Value

- [`OoprVec`](#oopr_containers-OoprVec):

  *`[4 fields]`* *`[8 methods]`*  
  A vector/array, where the container is indexed by an integer position.

- [`OoprMap`](#oopr_containers-OoprMap):

  *`[5 fields]`* *`[8 methods]`*  
  A map, where the container is indexed by a string, i.e. key-value
  pair.

------------------------------------------------------------------------

------------------------------------------------------------------------

OoprVec

### Description

A vector/array, where the container is indexed by an integer position.

### Fields

- `class`:

  *`[read-only]`*  
  `character(1L)`  
  The name of the underlying `oopr` class.

- `empty`:

  *`[read-only]`*  
  `logical(1L)`  
  Whether there are no instanced classes in the container.

- `size`:

  *`[read-only]`*  
  `integer(1L)`  
  The amount of instanced classes in the container.

- `data`:

  [`list()`](https://rdrr.io/r/base/list.html)  
  The container.

### Methods

- [`insert`](#oopr_containers-OoprVec-insert):

  Insert an already instanced class.

- [`emplace`](#oopr_containers-OoprVec-emplace):

  Construct a new class into the container.

- [`resize`](#oopr_containers-OoprVec-resize):

  Pre-allocate or destroy the container.

- [`erase`](#oopr_containers-OoprVec-erase):

  Remove a class from the container.

- [`swap`](#oopr_containers-OoprVec-swap):

  Swap two elements of the container.

- [`apply`](#oopr_containers-OoprVec-apply):

  Apply a function over every element in the container.

- [`` `[` ``](#oopr_containers-OoprVec-%60%5B%60):

  *`[S3]`*  
  Access an element of the container.

- [`` `[<-` ``](#oopr_containers-OoprVec-%60%5B%3C-%60):

  *`[S3]`*  
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

‘\[’

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

‘\[\<-’

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

### Description

A map, where the container is indexed by a string, i.e. key-value pair.

### Fields

- `class`:

  *`[read-only]`*  
  `character(1L)`  
  The name of the underlying `oopr` class.

- `empty`:

  *`[read-only]`*  
  `logical(1L)`  
  Whether there are no instanced classes in the container.

- `size`:

  *`[read-only]`*  
  `integer(1L)`  
  The amount of instanced classes in the container.

- `keys`:

  *`[read-only]`*  
  [`character()`](https://rdrr.io/r/base/character.html)  
  The keys within the container

- `data`:

  [`list()`](https://rdrr.io/r/base/list.html)  
  The container.

### Methods

- [`exists`](#oopr_containers-OoprMap-exists):

  Check whether a key exists.

- [`insert`](#oopr_containers-OoprMap-insert):

  Insert an already instanced class.

- [`emplace`](#oopr_containers-OoprMap-emplace):

  Construct a new class into the container.

- [`erase`](#oopr_containers-OoprMap-erase):

  Remove a class from the container.

- [`resize`](#oopr_containers-OoprMap-resize):

  Pre-allocate or destroy the container.

- [`apply`](#oopr_containers-OoprMap-apply):

  Apply a function over every element in the container.

- [`` `[` ``](#oopr_containers-OoprMap-%60%5B%60):

  *`[S3]`*  
  Access an element of the container.

- [`` `[<-` ``](#oopr_containers-OoprMap-%60%5B%3C-%60):

  *`[S3]`*  
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

‘\[’

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

‘\[\<-’

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
#> <OoprVec: 0x55aac38938f8>
#>  ├─$class  : chr "test"
#>  ├─$empty  : logi FALSE
#>  ├─$size   : int 2
#>  ├─$data   :List of 2
#>  │           $:<test: 0x55aac37c1a88>
#>  │            ..└─$x: int 2
#>  │           $:<test: 0x55aac381ba00>
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
#> <OoprVec: 0x55aac38938f8>
#>  ├─$class  : chr "test"
#>  ├─$empty  : logi FALSE
#>  ├─$size   : int 2
#>  ├─$data   :List of 2
#>  │           $:<test: 0x55aac381ba00>
#>  │            ..└─$x: int 1
#>  │           $:<test: 0x55aac37c1a88>
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
#> <OoprMap: 0x55aac8de3138>
#>  ├─$class  : chr "test"
#>  ├─$empty  : logi FALSE
#>  ├─$size   : int 2
#>  ├─$keys   : chr [1:2] "a" "b"
#>  ├─$data   :List of 2
#>  │           $a:<test: 0x55aac887ea58>
#>  │            ..└─$x: chr "a"
#>  │           $b:<test: 0x55aac8881ef8>
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
