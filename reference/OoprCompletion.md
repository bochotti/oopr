# Completion for oopr internals

Completion for oopr internals

## Usage

``` r
OoprCompletionSource()

OoprCompletionRStudio()

OoprCompletion()
```

## 

------------------------------------------------------------------------

------------------------------------------------------------------------

OoprCompletionSource

### Description

A virtual class that can be inherited to use as a completion identifier.

### Fields

- `env`:

  `environment`  
  The environment provided to `$load`.

- `call`:

  `call`  
  The evaluation string/call at the position `row` & `col` in the file,
  e.g. `this$a$b`.

- `file`:

  `character(1L)`  
  An R source file.

- `text`:

  [`character()`](https://rdrr.io/r/base/character.html)  
  A vector of lines of an R source file.

- `row`:

  `integer(1L)`  
  Line of a text cursor position.

- `col`:

  `integer(1L)`  
  Line of a text cursor position.

- `expr`:

  `expression`  
  A parsed expressed of `$file` or `$text` from `$parse()`.

- `defs`:

  [`list()`](https://rdrr.io/r/base/list.html)  
  A named list of `oopr` definitions from `$parse()`.

- `objs`:

  [`list()`](https://rdrr.io/r/base/list.html)  
  A named list of `oopr` object from `$eval()`.

- `obj`:

  `ooprC` If `$row` and `$col` are set, then the `oopr` object that is
  at that location.

### Methods

- `isAvailable`:

  Check if the call is currently a completion.

- `load`:

  Load the completion context into the class.

- `source`:

  Source the file being completed.

- `parse`:

  Try to parse the file.

- `eval`:

  Evaluate the `oopr`s in `$defs`.

------------------------------------------------------------------------

isAvailable

#### Description

Check if the call is currently a completion.

#### Usage

``` R

isAvailable()
```

#### Details

This method should check the call stack to ensure the appropriate
completion call is being made above. It should also use the `$load`
method.

#### Returns

`logical(1L)`.

------------------------------------------------------------------------

load

#### Description

Load the completion context into the class.

#### Usage

``` R

load(
  env  = globalenv()
 ,file
 ,text = NULL
 ,row
 ,col
)
```

#### Arguments

### 

[TABLE]

#### Details

Cannot be over-ridden. This is used inside `OoprCompletion` class.

#### Returns

`this` invisibly.

------------------------------------------------------------------------

source

#### Description

Source the file being completed.

#### Usage

``` R

source()
```

#### Details

Cannot be over-ridden. This is used inside `OoprCompletion` class.

#### Returns

`this` invisibly.

------------------------------------------------------------------------

parse

#### Description

Try to parse the file.

#### Usage

``` R

parse()
```

#### Details

When completing, the contents of a file may fail to parse:

1.  `$` with no rhs, e.g. `this$`

2.  Incomplete control-flow, e.g. `if(i in this$)`

Additionally, evaluation may fail due to member names or method
signatures:

1.  `this$mem` instead of `this$member`

2.  `this$method()` instead of `this$method(x = 1L)`

As such, `this` (or an inherited class) is replaced with `list` so the
class members are not reference checked.

#### Returns

See `OoprSource$parse()`.

------------------------------------------------------------------------

eval

#### Description

Evaluate the `oopr`s in `$defs`.

#### Usage

``` R

eval(top = globalenv())
```

#### Arguments

### 

[TABLE]

#### Details

If `$row` & `$col` are set, then evaluation stops at that `oopr` call.

#### Returns

Saves the `oopr` objects to `$objs`.

------------------------------------------------------------------------

------------------------------------------------------------------------

OoprCompletionRStudio

### Description

Use completion in RStudio.

### Fields

- `env`:

  `environment`  
  The environment provided to `$load`.

- `call`:

  `call`  
  The evaluation string/call at the position `row` & `col` in the file,
  e.g. `this$a$b`.

- `file`:

  `character(1L)`  
  An R source file.

- `text`:

  [`character()`](https://rdrr.io/r/base/character.html)  
  A vector of lines of an R source file.

- `row`:

  `integer(1L)`  
  Line of a text cursor position.

- `col`:

  `integer(1L)`  
  Line of a text cursor position.

- `expr`:

  `expression`  
  A parsed expressed of `$file` or `$text` from `$parse()`.

- `defs`:

  [`list()`](https://rdrr.io/r/base/list.html)  
  A named list of `oopr` definitions from `$parse()`.

- `objs`:

  [`list()`](https://rdrr.io/r/base/list.html)  
  A named list of `oopr` object from `$eval()`.

- `obj`:

  `ooprC` If `$row` and `$col` are set, then the `oopr` object that is
  at that location.

### Methods

- `isAvailable`:

  Pull information from the `.rs.rpc_get_completions` call.

- `load`:

  Load the completion context into the class.

- `source`:

  Source the file being completed.

- `parse`:

  Try to parse the file.

- `eval`:

  Evaluate the `oopr`s in `$defs`.

------------------------------------------------------------------------

isAvailable

#### Description

Pull information from the `.rs.rpc_get_completions` call.

#### Usage

``` R

isAvailable()
```

#### Details

Specifically pulls the rstudio `id` for the completion which contains
the file path, row and column.

#### Returns

`logical(1L)`.

------------------------------------------------------------------------

load

#### Description

Load the completion context into the class.

#### Usage

``` R

load(
  env  = globalenv()
 ,file
 ,text = NULL
 ,row
 ,col
)
```

#### Arguments

### 

[TABLE]

#### Details

Cannot be over-ridden. This is used inside `OoprCompletion` class.

#### Returns

`this` invisibly.

------------------------------------------------------------------------

source

#### Description

Source the file being completed.

#### Usage

``` R

source()
```

#### Details

Cannot be over-ridden. This is used inside `OoprCompletion` class.

#### Returns

`this` invisibly.

------------------------------------------------------------------------

parse

#### Description

Try to parse the file.

#### Usage

``` R

parse()
```

#### Details

When completing, the contents of a file may fail to parse:

1.  `$` with no rhs, e.g. `this$`

2.  Incomplete control-flow, e.g. `if(i in this$)`

Additionally, evaluation may fail due to member names or method
signatures:

1.  `this$mem` instead of `this$member`

2.  `this$method()` instead of `this$method(x = 1L)`

As such, `this` (or an inherited class) is replaced with `list` so the
class members are not reference checked.

#### Returns

See `OoprSource$parse()`.

------------------------------------------------------------------------

eval

#### Description

Evaluate the `oopr`s in `$defs`.

#### Usage

``` R

eval(top = globalenv())
```

#### Arguments

### 

[TABLE]

#### Details

If `$row` & `$col` are set, then evaluation stops at that `oopr` call.

#### Returns

Saves the `oopr` objects to `$objs`.

------------------------------------------------------------------------

------------------------------------------------------------------------

OoprCompletion

### Description

Use completion.

### Details

Completion call on an object is in the form `this$a$b$`. The trick here
is to check whether the text cursor is inside a class in the source
file. If so, collect that information and skip over everything before
the last dollar.

When the last dollar is reached, `.DollarNames` is called on `oopr_this`
object, which then fires the `names` method below. The evaluation
context `this$a$b` is found, and `.DollarNames` provided.

### Fields

- `isCompleting`:

  `logical(1L)`  
  Whether completion is currently in place. This will skip over each `$`
  call in the evaluation context.

- `isGettingNames`:

  `logical(1L)`  
  Whether names of an `ooprC` are being saught. This is used from
  `.DollarNames.ooprC` to provide more than just public static members.

- `source`:

  `OoprCompletionSource`  
  A instanced class advising whether completion is being sought.

### Methods

- `isCompletion`:

  Is completion being sought?

- `names`:

  Get the dollar names of the completion context.

- `obj`:

  Get the object of the completion context.

------------------------------------------------------------------------

isCompletion

#### Description

Is completion being sought?

#### Usage

``` R

isCompletion()
```

#### Details

Checks the `source` member as to whether the `$` or `.DollarNames` calls
are within a completion context. If so, then the file is parsed, `oopr`
class constructor collected, and `$isCompleting` set to `TRUE`.

#### Returns

`logical(1L)`.

------------------------------------------------------------------------

names

#### Description

Get the dollar names of the completion context.

#### Usage

``` R

names()
```

#### Returns

[`character()`](https://rdrr.io/r/base/character.html) of names.

------------------------------------------------------------------------

obj

#### Description

Get the object of the completion context.

#### Usage

``` R

obj()
```

#### Details

Used within `.rs.rpc.get_custom_parameter_help`.

#### Returns

`varies`.
