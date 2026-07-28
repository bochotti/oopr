# OoprRoxy Internals

`OoprRoxy` is called by `roxygen2` methods. It creates `OoprRoxyClass`
which creates a roxy block containing sections for each class.

**TODO**:

1.  Try to constrain horizontal width for nested sections.

2.  Tidy up pdf

## Usage

``` r
OoprRoxy()

OoprRoxySection(title = "", content = character(0L), hr = FALSE, pfx = "")

OoprRoxyDescribe(title = "Fields", hr = FALSE)

OoprRoxyUsage(content = character(0L), name = "")

OoprRoxyArguments(args = list())

OoprRoxyMethod(title, tags, fun, warn = TRUE, hr = TRUE, pfx = "")

OoprRoxyClass(block)
```

## Arguments

- title:

  `character(1L)`  
  The title of the section.

- content:

  [`character()`](https://rdrr.io/r/base/character.html)  
  Lines of the content for the section.

- hr:

  `logical(1L)`  
  Whether to add horizontal line to section heading.

- pfx:

  `character(1L)`  
  To add a hyperref.

- args:

  [`list()`](https://rdrr.io/r/base/list.html)  
  A list of `roxy_tag_param`s.

- tags:

  [`list()`](https://rdrr.io/r/base/list.html)  
  A list of (potentially optional) tags: `@description`, `@usage`,
  `@param`, `@details` & `@returns`.

- fun:

  `function`  
  The function object of the method.

- warn:

  `logical(1L)`  
  Whether warnings should display when missing tags.

- block:

  `roxy_block`  
  A roxy block containing an `oopr` class.

## Value

- [`OoprRoxySection`](#OoprRoxy-OoprRoxySection):

  *`[3 fields]`* *`[4 methods]`*  
  Create a subsection.

- [`OoprRoxyDescribe`](#OoprRoxy-OoprRoxyDescribe):

  *`[4 fields]`* *`[3 methods]`*  
  Represents a describe subsection.

- [`OoprRoxyUsage`](#OoprRoxy-OoprRoxyUsage):

  *`[3 fields]`* *`[3 methods]`*  
  Represents a usage subsection.

- [`OoprRoxyArguments`](#OoprRoxy-OoprRoxyArguments):

  *`[4 fields]`* *`[3 methods]`*  
  Create an arguments subsection.

- [`OoprRoxyMethod`](#OoprRoxy-OoprRoxyMethod):

  *`[6 fields]`* *`[3 methods]`*  
  Represents a method subsection.

- [`OoprRoxyClass`](#OoprRoxy-OoprRoxyClass):

  *`[5 fields]`* *`[4 methods]`*  
  Represents a class section.

------------------------------------------------------------------------

------------------------------------------------------------------------

OoprRoxySection

### Description

Create a subsection.

### Fields

- `title`:

  *`[read-only]`*  
  `character(1L)`  
  The title of the section.

- `content`:

  *`[read-only]`*  
  [`character()`](https://rdrr.io/r/base/character.html)  
  Lines of the content for the section.

- `size`:

  *`[read-only]`*  
  `integer(1L)`  
  The number of lines.

### Methods

- [`insert`](#OoprRoxy-OoprRoxySection-insert):

  *`[virtual]`*  
  Insert a line into the section.

- [`erase`](#OoprRoxy-OoprRoxySection-erase):

  *`[final]`*  
  Remove all lines from the section.

- [`toRd`](#OoprRoxy-OoprRoxySection-toRd):

  *`[final]`*  
  Convert the section into Rd formatted text.

- [`format`](#OoprRoxy-OoprRoxySection-format):

  *`[protected]`* *`[virtual]`*  
  Convert `$content` into a format for use in `$toRd()`.

------------------------------------------------------------------------

insert

#### Description

Insert a line into the section.

#### Usage

``` R
insert(x, i = this$size + 1L)
```

#### Arguments

### 

[TABLE]

#### Returns

`this` invisibly.

------------------------------------------------------------------------

erase

#### Description

Remove all lines from the section.

#### Usage

``` R
erase()
```

#### Returns

`this` invisibly.

------------------------------------------------------------------------

toRd

#### Description

Convert the section into Rd formatted text.

#### Usage

``` R
toRd()
```

#### Details

Uses the output from virtual method `$format()`.

#### Returns

`character(1L)`, wrapped in `\subsection`.

------------------------------------------------------------------------

format

#### Description

Convert `$content` into a format for use in `$toRd()`.

#### Usage

``` R
format()
```

#### Returns

Must return [`character()`](https://rdrr.io/r/base/character.html).

------------------------------------------------------------------------

------------------------------------------------------------------------

OoprRoxyDescribe

### Description

Represents a describe subsection.

### Fields

- `names`:

  *`[read-only]`*  
  `character(0L)`  
  Name of each item.

- `title`:

  *`[read-only]`*  
  `character(1L)`  
  The title of the section.

- `content`:

  *`[read-only]`*  
  [`character()`](https://rdrr.io/r/base/character.html)  
  Lines of the content for the section.

- `size`:

  *`[read-only]`*  
  `integer(1L)`  
  The number of lines.

### Methods

- [`insert`](#OoprRoxy-OoprRoxyDescribe-insert):

  *`[virtual]`*  
  Insert a line into the section.

- [`erase`](#OoprRoxy-OoprRoxyDescribe-erase):

  *`[final]`*  
  Remove all lines from the section.

- [`toRd`](#OoprRoxy-OoprRoxyDescribe-toRd):

  *`[final]`*  
  Convert the section into Rd formatted text.

------------------------------------------------------------------------

insert

#### Description

Insert a line into the section.

#### Usage

``` R
insert(x, i = character(1L))
```

#### Arguments

### 

[TABLE]

#### Returns

`this` invisibly.

------------------------------------------------------------------------

erase

#### Description

Remove all lines from the section.

#### Usage

``` R
erase()
```

#### Returns

`this` invisibly.

------------------------------------------------------------------------

toRd

#### Description

Convert the section into Rd formatted text.

#### Usage

``` R
toRd()
```

#### Details

Uses the output from virtual method `$format()`.

#### Returns

`character(1L)`, wrapped in `\subsection`.

------------------------------------------------------------------------

------------------------------------------------------------------------

OoprRoxyUsage

### Description

Represents a usage subsection.

### Fields

- `title`:

  *`[read-only]`*  
  `character(1L)`  
  The title of the section.

- `content`:

  *`[read-only]`*  
  [`character()`](https://rdrr.io/r/base/character.html)  
  Lines of the content for the section.

- `size`:

  *`[read-only]`*  
  `integer(1L)`  
  The number of lines.

### Methods

- [`insert`](#OoprRoxy-OoprRoxyUsage-insert):

  *`[virtual]`*  
  Insert a line into the section.

- [`erase`](#OoprRoxy-OoprRoxyUsage-erase):

  *`[final]`*  
  Remove all lines from the section.

- [`toRd`](#OoprRoxy-OoprRoxyUsage-toRd):

  *`[final]`*  
  Convert the section into Rd formatted text.

------------------------------------------------------------------------

insert

#### Description

Insert a line into the section.

#### Usage

``` R
insert(x, i = this$size + 1L)
```

#### Arguments

### 

[TABLE]

#### Returns

`this` invisibly.

------------------------------------------------------------------------

erase

#### Description

Remove all lines from the section.

#### Usage

``` R
erase()
```

#### Returns

`this` invisibly.

------------------------------------------------------------------------

toRd

#### Description

Convert the section into Rd formatted text.

#### Usage

``` R
toRd()
```

#### Details

Uses the output from virtual method `$format()`.

#### Returns

`character(1L)`, wrapped in `\subsection`.

------------------------------------------------------------------------

------------------------------------------------------------------------

OoprRoxyArguments

### Description

Create an arguments subsection.

### Fields

- `names`:

  *`[read-only]`*  
  `character(0L)`  
  Name of each item.

- `title`:

  *`[read-only]`*  
  `character(1L)`  
  The title of the section.

- `content`:

  *`[read-only]`*  
  [`character()`](https://rdrr.io/r/base/character.html)  
  Lines of the content for the section.

- `size`:

  *`[read-only]`*  
  `integer(1L)`  
  The number of lines.

### Methods

- [`insert`](#OoprRoxy-OoprRoxyArguments-insert):

  *`[virtual]`*  
  Insert a line into the section.

- [`erase`](#OoprRoxy-OoprRoxyArguments-erase):

  *`[final]`*  
  Remove all lines from the section.

- [`toRd`](#OoprRoxy-OoprRoxyArguments-toRd):

  *`[final]`*  
  Convert the section into Rd formatted text.

------------------------------------------------------------------------

insert

#### Description

Insert a line into the section.

#### Usage

``` R
insert(x, i = character(1L))
```

#### Arguments

### 

[TABLE]

#### Returns

`this` invisibly.

------------------------------------------------------------------------

erase

#### Description

Remove all lines from the section.

#### Usage

``` R
erase()
```

#### Returns

`this` invisibly.

------------------------------------------------------------------------

toRd

#### Description

Convert the section into Rd formatted text.

#### Usage

``` R
toRd()
```

#### Details

Uses the output from virtual method `$format()`.

#### Returns

`character(1L)`, wrapped in `\subsection`.

------------------------------------------------------------------------

------------------------------------------------------------------------

OoprRoxyMethod

### Description

Represents a method subsection.

### Details

Combines many sections into a single subsection. If the tags
`@description`, `@returns` and `@param` are not provided, a warning will
display.

### Fields

- `title`:

  *`[read-only]`*  
  `character(1L)`  
  The title of the section.

- `fun`:

  *`[read-only]`*  
  `function`  
  The function object of the method.

- `warn`:

  *`[read-only]`*  
  `logical(1L)`  
  Whether warnings should display when missing tags.

- `sections`:

  *`[container]`*  
  `OoprRoxySection`  
  A container of the subsections.

- `content`:

  *`[read-only]`*  
  [`character()`](https://rdrr.io/r/base/character.html)  
  Lines of the content for the section.

- `size`:

  *`[read-only]`*  
  `integer(1L)`  
  The number of lines.

### Methods

- [`insert`](#OoprRoxy-OoprRoxyMethod-insert):

  *`[virtual]`*  
  Insert a line into the section.

- [`erase`](#OoprRoxy-OoprRoxyMethod-erase):

  *`[final]`*  
  Remove all lines from the section.

- [`toRd`](#OoprRoxy-OoprRoxyMethod-toRd):

  *`[final]`*  
  Convert the section into Rd formatted text.

------------------------------------------------------------------------

insert

#### Description

Insert a line into the section.

#### Usage

``` R
insert(x, i = this$size + 1L)
```

#### Arguments

### 

[TABLE]

#### Returns

`this` invisibly.

------------------------------------------------------------------------

erase

#### Description

Remove all lines from the section.

#### Usage

``` R
erase()
```

#### Returns

`this` invisibly.

------------------------------------------------------------------------

toRd

#### Description

Convert the section into Rd formatted text.

#### Usage

``` R
toRd()
```

#### Details

Uses the output from virtual method `$format()`.

#### Returns

`character(1L)`, wrapped in `\subsection`.

------------------------------------------------------------------------

------------------------------------------------------------------------

OoprRoxyClass

### Description

Represents a class section.

### Details

Combines many sections into a single top-level section for a class.

Methods need to be called in order.

### Fields

- `title`:

  *`[read-only]`*  
  `character(1L)`  
  The title of the section.

- `block`:

  *`[read-only]`*  
  `roxy_block`  
  A roxy block containing an `oopr` class.

- `members`:

  *`[read-only]`*  
  [`list()`](https://rdrr.io/r/base/list.html)  
  A (nested) list of tags for each member.

- `rdname`:

  *`[read-only]`*  
  `character(1L)`  
  The name of the .Rd.

- `sections`:

  *`[container]`*  
  `OoprRoxySection`  
  The subsections inside the class section.

### Methods

- [`makeSections`](#OoprRoxy-OoprRoxyClass-makeSections):

  Creates subsections for `@description` and `@details` tags.

- [`makeFields`](#OoprRoxy-OoprRoxyClass-makeFields):

  Creates a subsection for a list of fields/properties.

- [`makeMethods`](#OoprRoxy-OoprRoxyClass-makeMethods):

  Creates a subsection for a list of methods and inserts method
  sections.

- [`makeTag`](#OoprRoxy-OoprRoxyClass-makeTag):

  Places the class section under the blocks tags list

------------------------------------------------------------------------

makeSections

#### Description

Creates subsections for `@description` and `@details` tags.

#### Usage

``` R
makeSections()
```

------------------------------------------------------------------------

makeFields

#### Description

Creates a subsection for a list of fields/properties.

#### Usage

``` R
makeFields()
```

------------------------------------------------------------------------

makeMethods

#### Description

Creates a subsection for a list of methods and inserts method sections.

#### Usage

``` R
makeMethods()
```

------------------------------------------------------------------------

makeTag

#### Description

Places the class section under the blocks tags list

#### Usage

``` R
makeTag()
```

------------------------------------------------------------------------

------------------------------------------------------------------------
