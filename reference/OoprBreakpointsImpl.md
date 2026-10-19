# Breakpoint Internals

Breakpoint Internals

## Usage

``` r
OoprBreakpointsMap(old, new)

OoprBreakpointsBreak(body, line, cond)

OoprBreakpointsCond()

OoprBreakpointsFunction(fun)

OoprBreakpointsClass(ooprC)

OoprBreakpointsFile(file)

OoprBreakpointsImpl()

OoprBreakpointsRStudio
```

## Arguments

- old:

  `function`  
  Old function.

- new:

  `function`  
  New function.

- body:

  `language`  
  The body of the function to set the breakpoint.

- line:

  `integer(1L)`  
  The line number.

- cond:

  `langauge`  
  The condition that triggers the breakpoint.

- fun:

  `function`  
  The function to set breakpoints for.

- ooprC:

  `ooprC`  
  The `ooprC` object.

- file:

  `character(1L)`  
  The name of the source file. Will be normalized

## Value

- [`OoprBreakpointsMap`](#OoprBreakpointsImpl-OoprBreakpointsMap):

  *`[public]`*  
  *`[1 field]`* *`[3 methods]`*  
  Map srcref line numbers.

- [`OoprBreakpointsBreak`](#OoprBreakpointsImpl-OoprBreakpointsBreak):

  *`[public]`*  
  *`[3 fields]`* *`[0 methods]`*

- [`OoprBreakpointsFunction`](#OoprBreakpointsImpl-OoprBreakpointsFunction):

  *`[public]`*  
  *`[4 fields]`* *`[5 methods]`*  
  Represents a function object inside a class.

- [`OoprBreakpointsClass`](#OoprBreakpointsImpl-OoprBreakpointsClass):

  *`[public]`*  
  *`[3 fields]`* *`[6 methods]`*  
  Represents a class inside a file.

- [`OoprBreakpointsFile`](#OoprBreakpointsImpl-OoprBreakpointsFile):

  *`[public]`*  
  *`[7 fields]`* *`[7 methods]`*  
  Represents a source file that contains `oopr` classes.

- [`OoprBreakpointsImpl`](#OoprBreakpointsImpl-OoprBreakpointsImpl):

  *`[protected]`*  
  *`[3 fields]`* *`[2 methods]`*  
  Implementation of `oopr` breakpoints.

- [`OoprBreakpointsRStudio`](#OoprBreakpointsImpl-OoprBreakpointsRStudio):

  *`[private]`*  
  *`[0 fields]`* *`[4 methods]`*  
  The controller for RStudio breakpoints.

------------------------------------------------------------------------

------------------------------------------------------------------------

OoprBreakpointsMap

### Description

Map srcref line numbers.

### Details

Utilises a longest common subsequence algorithm.

### Fields

- `map`:

  *`[read-only]`*  
  [`integer()`](https://rdrr.io/r/base/integer.html)  
  The lines which can be set.

### Methods

- [`load`](#OoprBreakpointsImpl-OoprBreakpointsMap-load):

  Add a new map.

- [`toOld`](#OoprBreakpointsImpl-OoprBreakpointsMap-toOld):

  Map the new lines to the old lines.

- [`toNew`](#OoprBreakpointsImpl-OoprBreakpointsMap-toNew):

  Map the old lines to the new lines.

------------------------------------------------------------------------

load

#### Description

Add a new map.

#### Usage

``` R
load(old, new)
```

#### Returns

`this` invisibly.

------------------------------------------------------------------------

toOld

#### Description

Map the new lines to the old lines.

#### Usage

``` R
toOld(lines)
```

#### Arguments

### 

[TABLE]

#### Returns

`integer(length(lines))`. If a new line does not exist in the old lines,
then `NA`.

------------------------------------------------------------------------

toNew

#### Description

Map the old lines to the new lines.

#### Usage

``` R
toNew(lines)
```

#### Arguments

### 

[TABLE]

#### Returns

`integer(length(lines))`. If an old line does not exist in the new
lines, then `NA`.

------------------------------------------------------------------------

------------------------------------------------------------------------

OoprBreakpointsBreak

### Fields

- `line`:

  *`[read-only]`*  
  `integer(1L)`  
  The line number.

- `cond`:

  *`[read-only]`*  
  `langauge`  
  The condition that triggers the breakpoint.

- `steps`:

  *`[read-only]`*  
  [`integer()`](https://rdrr.io/r/base/integer.html)  
  The steps required to set breakpoint at `$line`.

------------------------------------------------------------------------

------------------------------------------------------------------------

------------------------------------------------------------------------

------------------------------------------------------------------------

------------------------------------------------------------------------

OoprBreakpointsCond

### Description

Used to track changes to breakpoints among files/classes/functions.

### Details

Holds vector of value, and/or container of the same class.

### Methods

- [`push`](#OoprBreakpointsImpl-OoprBreakpointsCond-push):

  Add a value.

- [`clear`](#OoprBreakpointsImpl-OoprBreakpointsCond-clear):

  Clear all values & children.

- [`child`](#OoprBreakpointsImpl-OoprBreakpointsCond-child):

  Create a child class.

- [`list`](#OoprBreakpointsImpl-OoprBreakpointsCond-list):

  Convert contents to a last

------------------------------------------------------------------------

push

#### Description

Add a value.

#### Usage

``` R
push(x)
```

------------------------------------------------------------------------

clear

#### Description

Clear all values & children.

#### Usage

``` R
clear()
```

------------------------------------------------------------------------

child

#### Description

Create a child class.

#### Usage

``` R
child(...)
```

#### Arguments

### 

[TABLE]

#### Returns

The new child class.

------------------------------------------------------------------------

list

#### Description

Convert contents to a last

#### Usage

``` R
list()
```

#### Returns

[`list()`](https://rdrr.io/r/base/list.html)

------------------------------------------------------------------------

------------------------------------------------------------------------

OoprBreakpointsFunction

### Description

Represents a function object inside a class.

### Fields

- `fun`:

  *`[read-only]`*  
  `function`  
  The method or active binding.

- `body`:

  *`[read-only]`*  
  `expression`  
  Returns the body of `$fun` without any breakpoints.

- `srcref`:

  *`[read-only]`*  
  `srcref`  
  The srcref for `$fun`.

- `breaks`:

  *`[read-only]`*  
  `data.frame`  
  The `line`, `steps` and `cond` of each set breakpoint.

### Methods

- [`synced`](#OoprBreakpointsImpl-OoprBreakpointsFunction-synced):

  Check whether `$srcref` matches the source file.

- [`has`](#OoprBreakpointsImpl-OoprBreakpointsFunction-has):

  Check whether a line number is inside the function.

- [`add`](#OoprBreakpointsImpl-OoprBreakpointsFunction-add):

  Add breakpoints to the function

- [`rm`](#OoprBreakpointsImpl-OoprBreakpointsFunction-rm):

  Remove breakpoints.

- [`load`](#OoprBreakpointsImpl-OoprBreakpointsFunction-load):

  Update the srcref mapping or load a new function.

------------------------------------------------------------------------

synced

#### Description

Check whether `$srcref` matches the source file.

#### Usage

``` R
synced()
```

#### Details

The source file is read, and cut by the positions of `$srcref`.

#### Returns

`logical(1L)`.

------------------------------------------------------------------------

has

#### Description

Check whether a line number is inside the function.

#### Usage

``` R
has(lines, old = FALSE)
```

#### Arguments

### 

[TABLE]

#### Returns

`logical(length(lines))`.

------------------------------------------------------------------------

add

#### Description

Add breakpoints to the function

#### Usage

``` R
add(lines, conds = list(), old = FALSE)
```

#### Arguments

### 

[TABLE]

#### Details

Existing breakpoints are removed and reapplied.

#### Returns

`this` invisibly.

------------------------------------------------------------------------

rm

#### Description

Remove breakpoints.

#### Usage

``` R
rm(lines = integer(), old = FALSE)
```

#### Arguments

### 

[TABLE]

#### Returns

`this` invisibly.

------------------------------------------------------------------------

load

#### Description

Update the srcref mapping or load a new function.

#### Usage

``` R
load(
  fun
 ,new  = FALSE
 ,cond = OoprBreakpointsCond()
)
```

#### Arguments

### 

[TABLE]

#### Returns

`this` invisibly.

------------------------------------------------------------------------

------------------------------------------------------------------------

OoprBreakpointsClass

### Description

Represents a class inside a file.

### Fields

- `ooprC`:

  *`[read-only]`*  
  `ooprC`  
  The `ooprC` object.

- `name`:

  *`[read-only]`*  
  `character(1L)`  
  The name of the class.

- `breaks`:

  *`[read-only]`*  
  `data.frame`  
  The `line`, `steps` and `cond` of each set breakpoint.

### Methods

- [`synced`](#OoprBreakpointsImpl-OoprBreakpointsClass-synced):

  Check if functions are in sync.

- [`has`](#OoprBreakpointsImpl-OoprBreakpointsClass-has):

  Check if a any class' functions contains a line.

- [`where`](#OoprBreakpointsImpl-OoprBreakpointsClass-where):

  See which function contains the line.

- [`add`](#OoprBreakpointsImpl-OoprBreakpointsClass-add):

  Add breakpoints to the function

- [`rm`](#OoprBreakpointsImpl-OoprBreakpointsClass-rm):

  Remove breakpoints.

- [`load`](#OoprBreakpointsImpl-OoprBreakpointsClass-load):

  Update the srcref mapping or load a new class.

------------------------------------------------------------------------

synced

#### Description

Check if functions are in sync.

#### Usage

``` R
synced(names = character(0L))
```

#### Arguments

### 

[TABLE]

#### Returns

`logical(1L)`

------------------------------------------------------------------------

has

#### Description

Check if a any class' functions contains a line.

#### Usage

``` R
has(lines)
```

#### Arguments

### 

[TABLE]

#### Returns

`logical(length(lines))`.

------------------------------------------------------------------------

where

#### Description

See which function contains the line.

#### Usage

``` R
where(lines)
```

#### Arguments

### 

[TABLE]

------------------------------------------------------------------------

add

#### Description

Add breakpoints to the function

#### Usage

``` R
add(lines, conds)
```

#### Arguments

### 

[TABLE]

#### Details

Existing breakpoints are removed and reapplied.

#### Returns

`this` invisibly.

------------------------------------------------------------------------

rm

#### Description

Remove breakpoints.

#### Usage

``` R
rm(lines = integer(0L))
```

#### Arguments

### 

[TABLE]

#### Returns

`this` invisibly.

------------------------------------------------------------------------

load

#### Description

Update the srcref mapping or load a new class.

#### Usage

``` R
load(
  ooprC
 ,new   = FALSE
 ,cond  = OoprBreakpointsCond()
)
```

#### Arguments

### 

[TABLE]

#### Returns

`this` invisibly.

------------------------------------------------------------------------

------------------------------------------------------------------------

OoprBreakpointsFile

### Description

Represents a source file that contains `oopr` classes.

### Fields

- `file`:

  *`[read-only]`*  
  `character(1L)`  
  The file path relative to the working directory.

- `pkg`:

  *`[read-only]`*  
  `character(1L)`  
  The package name of `$file`, if applicable.

- `top`:

  *`[read-only]`*  
  `environment`  
  If `nzchar(pkg)` then the namespace, otherwise the global environment.
  `getNamespace` is called with each access, as to not carry references
  to (older) namespaces if re-loaded.

- `reloaded`:

  *`[read-only]`*  
  `logical(1L)`  
  Whether `$top` namespace differs.

- `timestamp`:

  `POSIXct(1L)`  
  The last time the source file was read.

- `modified`:

  *`[read-only]`*  
  `logical(1L)`  
  Whether `$file` has been modified since the last load.

- `breaks`:

  *`[read-only]`*  
  `data.frame`  
  The `line`, `steps` and `cond` of each set breakpoint.

### Methods

- [`` `==` ``](#OoprBreakpointsImpl-OoprBreakpointsFile-%60==%60):

  *`[S3]`*  
  Check file equivalence.

- [`has`](#OoprBreakpointsImpl-OoprBreakpointsFile-has):

  Check if any class inside the file contains a function and/or a line.

- [`where`](#OoprBreakpointsImpl-OoprBreakpointsFile-where):

  See which class contains a line.

- [`synced`](#OoprBreakpointsImpl-OoprBreakpointsFile-synced):

  Check if functions are in sync.

- [`add`](#OoprBreakpointsImpl-OoprBreakpointsFile-add):

  Add breakpoints to the function

- [`rm`](#OoprBreakpointsImpl-OoprBreakpointsFile-rm):

  Remove breakpoints.

- [`load`](#OoprBreakpointsImpl-OoprBreakpointsFile-load):

------------------------------------------------------------------------

‘==’

#### Description

Check file equivalence.

#### Usage

``` R
`==`(e2)
```

#### Arguments

### 

[TABLE]

#### Returns

`logical(1L)`

------------------------------------------------------------------------

has

#### Description

Check if any class inside the file contains a function and/or a line.

#### Usage

``` R
has(lines)
```

#### Arguments

### 

[TABLE]

#### Returns

Named [`logical()`](https://rdrr.io/r/base/logical.html) the same length
as `lines`, indicating whether any class holds the line.

------------------------------------------------------------------------

where

#### Description

See which class contains a line.

#### Usage

``` R
where(lines)
```

#### Arguments

### 

[TABLE]

#### Returns

`character(length(lines))`, being the class name/s. If a line is not
covered, then a zero character string.

------------------------------------------------------------------------

synced

#### Description

Check if functions are in sync.

#### Usage

``` R
synced(names = character(0L))
```

#### Arguments

### 

[TABLE]

#### Returns

`logical(1L)`

------------------------------------------------------------------------

add

#### Description

Add breakpoints to the function

#### Usage

``` R
add(lines, conds)
```

#### Arguments

### 

[TABLE]

#### Details

Existing breakpoints are removed and reapplied.

#### Returns

`this` invisibly.

------------------------------------------------------------------------

rm

#### Description

Remove breakpoints.

#### Usage

``` R
rm(lines = integer(0L))
```

#### Arguments

### 

[TABLE]

#### Returns

`this` invisibly.

------------------------------------------------------------------------

load

#### Usage

``` R
load(
  new  = this$reloaded
 ,cond = OoprBreakpointsCond()
)
```

#### Arguments

### 

[TABLE]

------------------------------------------------------------------------

------------------------------------------------------------------------

OoprBreakpointsImpl

### Description

Implementation of `oopr` breakpoints.

### Fields

- `volatile`:

  *`[static]`*  
  `logical(1L)`  
  Whether to automatically update breakpoints when changes to source
  files or environments occur. Updates only occur when this class is
  interacted with.

- `quiet`:

  *`[static]`*  
  `logical(1L)`  
  Whether to silence messages when breakpoints auto update.

- `breaks`:

  *`[read-only]`* *`[static]`*  
  `data.frame(6L)`  
  Currently set breakpoints; giving the file, class, name, line, steps,
  and condition. File paths are always relative to the current working
  directory. Lines displayed are referred to the last modified state of
  the file, not necessarily to a functions `srcref`.

### Methods

- [`add`](#OoprBreakpointsImpl-OoprBreakpointsImpl-add):

  *`[static]`*  
  Add breakpoints.

- [`rm`](#OoprBreakpointsImpl-OoprBreakpointsImpl-rm):

  *`[static]`*  
  Remove breakpoints.

------------------------------------------------------------------------

add

#### Description

Add breakpoints.

#### Usage

``` R
add(file, lines, conds = list())
```

#### Arguments

### 

[TABLE]

#### Details

If `$volatile == TRUE`, then `lines` must align with the most recent
modified version of `file`.

#### Returns

`this` invisibly.

------------------------------------------------------------------------

rm

#### Description

Remove breakpoints.

#### Usage

``` R
rm(file, lines = integer(0L))
```

#### Arguments

### 

[TABLE]

#### Details

If `$volatile == TRUE`, then `lines` must align with the most recent
modified version of `file`.

#### Returns

`this` invisibly.

------------------------------------------------------------------------

------------------------------------------------------------------------

## RStudio

Setting breakpoints inside `oopr` classes is the same process as setting
them for normal functions - use the gutter or `SHIFT+F9`.

RStudio calls its R functions inside `tools:rstudio` from the global
environment. By assigning functions of the same name, these calls can be
intercepted.

This feature is disabled by default as it is potentially destructive for
an R session. Environmental variable `R_OOPR_BREAKPOINTS=true` can be
used to enable this functionality when `oopr` namespace is loaded.

Breakpoints cannot be set on single line functions with a specifier,

    get:x <- \( ) { return("x"); }

as RStudio thinks the line is at the top-level.

Tested with RStudio version 2026.1.1.403, there are no guarantees for
earlier or later versions.

------------------------------------------------------------------------

------------------------------------------------------------------------

OoprBreakpointsRStudio

### Description

The controller for RStudio breakpoints.

### Details

RStudio's process of setting a breakpoint is:

1.  Check the function is in sync.

2.  Get the steps of the functions body to arrive at a line.

3.  Get the environment that the function is assigned to.

4.  Set the breakpoints into the function. Only steps 3 & 4 apply when
    unsetting a breakpoint, or reloading a file.

RStudio maintains its own record of breakpoints and is recorded by file
and function name. However, multiple classes in a file could share a
function name.

TODO: newer versions of RStudio use `.rs.setBreakpoint`.

### Methods

- [`isFunctionInSync`](#OoprBreakpointsImpl-OoprBreakpointsRStudio-isFunctionInSync):

  *`[static]`*  
  Checks whether the srcref matches the file.

- [`getFunctionSteps`](#OoprBreakpointsImpl-OoprBreakpointsRStudio-getFunctionSteps):

  *`[static]`*  
  Gets the "steps" required for a line.

- [`getEnvironmentOfFunction`](#OoprBreakpointsImpl-OoprBreakpointsRStudio-getEnvironmentOfFunction):

  *`[static]`*  
  Gets the environment of a function.

- [`setFunctionBreakpoints`](#OoprBreakpointsImpl-OoprBreakpointsRStudio-setFunctionBreakpoints):

  *`[static]`*  
  Sets the breakpoints for a function.

------------------------------------------------------------------------

isFunctionInSync

#### Description

Checks whether the srcref matches the file.

#### Usage

``` R
isFunctionInSync(name, file, pkg)
```

#### Arguments

### 

[TABLE]

#### Details

The first function to be called when setting a breakpoint.

#### Returns

`logical(1L)`

------------------------------------------------------------------------

getFunctionSteps

#### Description

Gets the "steps" required for a line.

#### Usage

``` R
getFunctionSteps(name, file, pkg, lines)
```

#### Arguments

### 

[TABLE]

#### Details

The trick here is to prefix the steps with `"oopr:"` as RStudio does
*not* validate the `steps` field. The function setting the breakpoints
will know which breakpoints are for `oopr` classes.

#### Returns

A `list` of
`list(name = character(1L), line = integer(1L), at = character(1L))`,
where `at` is in the form `"oopr:line"` if relating to an `oopr` class.

------------------------------------------------------------------------

getEnvironmentOfFunction

#### Description

Gets the environment of a function.

#### Usage

``` R
getEnvironmentOfFunction(
  name
 ,file
 ,pkg
)
```

#### Details

This is called first when unsetting a breakpoint.

Returning `emptenv()` ensures that the next step is called. This can be
called multiple times, so I only want to do this once.

#### Returns

`environment`

------------------------------------------------------------------------

setFunctionBreakpoints

#### Description

Sets the breakpoints for a function.

#### Usage

``` R
setFunctionBreakpoints(name, env, steps)
```

#### Arguments

### 

[TABLE]

#### Details

`steps` contains all active breakpoints (except those being removed)
plus any that are being added. If `length(steps) == 0L`, then all
breakpoints are being removed.

#### Returns

`name`

------------------------------------------------------------------------

------------------------------------------------------------------------
