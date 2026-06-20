# Source files for oopr

Source files for oopr

## Usage

``` r
OoprSource()

OoprSourceTry(file = NULL, text = NULL, row = NULL, col = NULL)
```

------------------------------------------------------------------------

------------------------------------------------------------------------

OoprSource

### Description

Takes a file and parses the oopr definitions inside it.

### Fields

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

- `parse`:

  Parse `$file` or `$text`.

- `eval`:

  Evaluate the `oopr`s in `$defs`.

------------------------------------------------------------------------

parse

#### Description

Parse `$file` or `$text`.

#### Usage

``` R

parse()
```

#### Details

If `$file` is set, then it acts as the `srcfile` of the parsed object.

#### Returns

Saves the entire parse to `$expr` and the `oopr` calls to `$defs`.

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

OoprSourceTry

### Description

Tries to parse and evaluate a file containing ooprs, intended for
completion.

### Fields

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

- `parse`:

  Try to parse the file.

- `eval`:

  Evaluate the `oopr`s in `$defs`.

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
