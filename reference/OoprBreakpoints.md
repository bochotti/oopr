# Breakpoint Internals

Breakpoint Internals

## Usage

``` r
OoprBreakpointsFunction(name, ooprC)

OoprBreakpointsClass(ooprC)

OoprBreakpointsFile(file, env)

OoprBreakpoints()
```

## Arguments

- name:

  `character(1L)`  
  The name of the function.

- ooprC:

  `ooprC`  
  The `ooprC` object.

- file:

  `character(1L)`  
  The name of the source file.

- env:

  `environment`  
  The environment that holds the class definitions. Either a package
  namespace or the global environment.

## Value

- [`OoprBreakpointsFunction`](#OoprBreakpoints-OoprBreakpointsFunction):

  *`[7 fields]`* *`[4 methods]`*  
  Represents a function object inside a class.

- [`OoprBreakpointsClass`](#OoprBreakpoints-OoprBreakpointsClass):

  *`[3 fields]`* *`[4 methods]`*  
  Represents a class inside a file.

- [`OoprBreakpointsFile`](#OoprBreakpoints-OoprBreakpointsFile):

  *`[3 fields]`* *`[5 methods]`*  
  Represents a source file that contains `oopr` classes.

- [`OoprBreakpoints`](#OoprBreakpoints-OoprBreakpoints):

  *`[1 field]`* *`[5 methods]`*  
  The controller of the breakpoints. Process of setting a breakpoint is:

  1.  Check the function is in sync.

  2.  Get the steps of the functions body to arrive at a line.

  3.  Get the environment that the function is assigned to.

  4.  Set the breakpoints into the function. Only steps 3 & 4 are
      conducted when unsetting a breakpoint.

  RStudio maintains its own database for breakpoints and is recorded by
  file and function name. However, multiple classes in a file could
  share a function name.

------------------------------------------------------------------------

------------------------------------------------------------------------

OoprBreakpointsFunction

### Description

Represents a function object inside a class.

### Fields

- `name`:

  `character(1L)`  
  The name of the function.

- `ooprC`:

  `ooprC`  
  The `ooprC` class that the function resides.

- `encl`:

  `environment`  
  The enclosure inside `ooprC`. Can change to set breakpoints for class
  instances.

- `property`:

  `logical(1L)`  
  Whether the function is a property. If so, then breakpoints need to be
  set on the active binding function.

- `fun`:

  `function`  
  Returns the un-traced version of the function from `$encl$this`. If
  set, it assigns into `$encl$this` and `$encl$.this` if applicable.

- `srcref`:

  *`[read-only]`*  
  `srcref`  
  The srcref for `this$fun`.

- `breaks`:

  *`[read-only]`*  
  [`integer()`](https://rdrr.io/r/base/integer.html)  
  Line numbers of each call to `$getSteps`.

### Methods

- [`isInSync`](#OoprBreakpoints-OoprBreakpointsFunction-isInSync):

  Check whether `$srcref` matches the source file.

- [`hasLine`](#OoprBreakpoints-OoprBreakpointsFunction-hasLine):

  Check whether a line number is inside the function.

- [`getSteps`](#OoprBreakpoints-OoprBreakpointsFunction-getSteps):

  Get the steps of the function required to reach a line number.

- [`setBreakpoints`](#OoprBreakpoints-OoprBreakpointsFunction-setBreakpoints):

  Set breakpoints inside the class, and any class instances.

------------------------------------------------------------------------

isInSync

#### Description

Check whether `$srcref` matches the source file.

#### Usage

``` R
isInSync()
```

#### Details

The source file is read, and cut by the positions of `$srcref`.

#### Returns

`logical(1L)`.

------------------------------------------------------------------------

hasLine

#### Description

Check whether a line number is inside the function.

#### Usage

``` R
hasLine(line)
```

#### Arguments

### 

[TABLE]

#### Returns

`logical(1L)`.

------------------------------------------------------------------------

getSteps

#### Description

Get the steps of the function required to reach a line number.

#### Usage

``` R
getSteps(line)
```

#### Arguments

### 

[TABLE]

#### Details

The srcrefs inside the body of `$fun` is searched to find where the line
can be found. Steps is the integer position to enter the body of the
function, e.g. `body(fun)[[steps]]`.

#### Returns

`character(1L)` of the steps separated by `,`.

------------------------------------------------------------------------

setBreakpoints

#### Description

Set breakpoints inside the class, and any class instances.

#### Usage

``` R
setBreakpoints(steps = character())
```

#### Arguments

### 

[TABLE]

#### Details

Uses [`base::trace`](https://rdrr.io/r/base/trace.html) to create a
traced function, steps are ordered by their depth of nesting so `trace`
doesn't overwrite its own breakpoints.

#### Returns

`this` invisibly. `steps` is also saved inside a private member for
`$breaks` to return line numbers set.

------------------------------------------------------------------------

------------------------------------------------------------------------

OoprBreakpointsClass

### Description

Represents a class inside a file.

### Fields

- `ooprC`:

  `ooprC`  
  The `ooprC` object.

- `name`:

  *`[read-only]`*  
  `character(1L)`  
  The name of the class.

- `functions`:

  *`[container]`*  
  `OoprBreakpointsFunction`  
  An array of each function inside the class.

### Methods

- [`has`](#OoprBreakpoints-OoprBreakpointsClass-has):

  Check if the class contains a function and/or a line.

- [`isInSync`](#OoprBreakpoints-OoprBreakpointsClass-isInSync):

  Check if a function is in sync.

- [`getSteps`](#OoprBreakpoints-OoprBreakpointsClass-getSteps):

  Get the steps required to get to a line in a function.

- [`setBreakpoints`](#OoprBreakpoints-OoprBreakpointsClass-setBreakpoints):

  Set breakpoints inside the class, and any class instances.

------------------------------------------------------------------------

has

#### Description

Check if the class contains a function and/or a line.

#### Usage

``` R
has(name, line = integer(0L))
```

#### Arguments

### 

[TABLE]

#### Returns

`logical(1L)`.

------------------------------------------------------------------------

isInSync

#### Description

Check if a function is in sync.

#### Usage

``` R
isInSync(name)
```

#### Arguments

### 

[TABLE]

#### Details

If the function is not in the class `TRUE` is returned.

#### Returns

`logical(1L)`.

------------------------------------------------------------------------

getSteps

#### Description

Get the steps required to get to a line in a function.

#### Usage

``` R
getSteps(name, line)
```

#### Arguments

### 

[TABLE]

#### Details

The trick here is to append the steps with the name of the class. This
way when receiving them for setting a breakpoints, the class holding the
function is known.

#### Returns

`list(name = character(1L), line = integer(1L), at = character(1L))`.
`at` is in the form `"class:steps"`, where `steps` are the integer
position of the function body, separated by `,`. If no steps can be
found, then [`list()`](https://rdrr.io/r/base/list.html) is returned.

------------------------------------------------------------------------

setBreakpoints

#### Description

Set breakpoints inside the class, and any class instances.

#### Usage

``` R
setBreakpoints(
  name
 ,steps = character(0L)
)
```

#### Arguments

### 

[TABLE]

#### Details

Uses [`base::trace`](https://rdrr.io/r/base/trace.html) to create a
traced function, steps are ordered by their depth of nesting so `trace`
doesn't overwrite its own breakpoints.

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
  The name of the source file.

- `timestamp`:

  `POSIXct(1L)`  
  The last time the source file was read.

- `classes`:

  *`[container]`*  
  `OoprBreakpointsClass`  
  The classes defined inside the source file.

### Methods

- [`syncClassesWithFile`](#OoprBreakpoints-OoprBreakpointsFile-syncClassesWithFile):

  Check if the source file has been modified, if so reload the classes.

- [`has`](#OoprBreakpoints-OoprBreakpointsFile-has):

  Check if any class inside the file contains a function and/or a line.

- [`isInSync`](#OoprBreakpoints-OoprBreakpointsFile-isInSync):

  Check if a function is in sync.

- [`getSteps`](#OoprBreakpoints-OoprBreakpointsFile-getSteps):

  Get the steps of multiple lines across the file.

- [`setBreakpoints`](#OoprBreakpoints-OoprBreakpointsFile-setBreakpoints):

  Set breakpoints for a function across classes.

------------------------------------------------------------------------

syncClassesWithFile

#### Description

Check if the source file has been modified, if so reload the classes.

#### Usage

``` R
syncClassesWithFile(env)
```

#### Arguments

### 

[TABLE]

#### Returns

`this` invisibly.

------------------------------------------------------------------------

has

#### Description

Check if any class inside the file contains a function and/or a line.

#### Usage

``` R
has(name, line = integer(0L))
```

#### Arguments

### 

[TABLE]

#### Returns

Named [`logical()`](https://rdrr.io/r/base/logical.html) the same length
as `$classes`, indicating which class holds the function and/or line. If
only `name` is provided, can have more than one `TRUE`.

------------------------------------------------------------------------

isInSync

#### Description

Check if a function is in sync.

#### Usage

``` R
isInSync(name)
```

#### Arguments

### 

[TABLE]

#### Details

If the function is not in the class `TRUE` is returned.

#### Returns

`logical(1L)`.

------------------------------------------------------------------------

getSteps

#### Description

Get the steps of multiple lines across the file.

#### Usage

``` R
getSteps(name, lines)
```

#### Arguments

### 

[TABLE]

#### Details

The trick here is to append the steps with the name of the class. This
way when receiving them for setting a breakpoints, the class holding the
function is known.

#### Returns

A list the same length of `lines`, containing further lists in the form:
`list(name = character(1L), line = integer(1L), at = character(1L))`.
Lines that cannot be found are returned as
[`list()`](https://rdrr.io/r/base/list.html).

------------------------------------------------------------------------

setBreakpoints

#### Description

Set breakpoints for a function across classes.

#### Usage

``` R
setBreakpoints(
  name
 ,classes = character(0L)
 ,steps   = character(0L)
)
```

#### Arguments

### 

[TABLE]

#### Details

Uses [`base::trace`](https://rdrr.io/r/base/trace.html) to create a
traced function, steps are ordered by their depth of nesting so `trace`
doesn't overwrite its own breakpoints.

#### Returns

`this` invisibly.

------------------------------------------------------------------------

------------------------------------------------------------------------

OoprBreakpoints

### Description

The controller of the breakpoints. Process of setting a breakpoint is:

1.  Check the function is in sync.

2.  Get the steps of the functions body to arrive at a line.

3.  Get the environment that the function is assigned to.

4.  Set the breakpoints into the function. Only steps 3 & 4 are
    conducted when unsetting a breakpoint.

RStudio maintains its own database for breakpoints and is recorded by
file and function name. However, multiple classes in a file could share
a function name.

### Fields

- `files`:

  *`[static]`* *`[container]`*  
  `OoprBreakpointsFile`  
  The source files that have/had breakpoints.

### Methods

- [`isFunctionInSync`](#OoprBreakpoints-OoprBreakpoints-isFunctionInSync):

  *`[static]`*  
  Checks whether the srcref matches the file.

- [`getFunctionSteps`](#OoprBreakpoints-OoprBreakpoints-getFunctionSteps):

  *`[static]`*  
  Gets the steps required for a line.

- [`getEnvironmentOfFunction`](#OoprBreakpoints-OoprBreakpoints-getEnvironmentOfFunction):

  *`[static]`*  
  Gets the environment of a function.

- [`setFunctionBreakpoints`](#OoprBreakpoints-OoprBreakpoints-setFunctionBreakpoints):

  *`[static]`*  
  Sets the breakpoints for a function.

- [`printBreaks`](#OoprBreakpoints-OoprBreakpoints-printBreaks):

  *`[static]`*  
  Print currently set breakpoints.

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

Gets the steps required for a line.

#### Usage

``` R
getFunctionSteps(name, file, pkg, lines)
```

#### Arguments

### 

[TABLE]

#### Details

`lines` include only the lines to be added.

#### Returns

`list(name =, line =, at = )` - `at` should be separated by `,`.

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

#### Arguments

### 

[TABLE]

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

The function is fully untraced first, meaning that `steps` always
contains all active breakpoints (except those being removed) plus any
that are being added. If `length(steps) == 0L`, then all breakpoints are
being removed.

#### Returns

`name`

------------------------------------------------------------------------

printBreaks

#### Description

Print currently set breakpoints.

#### Usage

``` R
printBreaks()
```

------------------------------------------------------------------------

------------------------------------------------------------------------
