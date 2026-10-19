# Breakpoints for oopr

Set breakpoints for `oopr` classes.

## Usage

``` r
OoprBreakpoints
```

## Value

- [`OoprBreakpoints`](#oopr_breakpoints-OoprBreakpoints):

  *`[private]`*  
  *`[4 fields]`* *`[2 methods]`*

## Details

Breakpoints for `oopr` classes are set within the class object and all
instances that are spawned from it.

When interacting with `oopr` breakpoints, changes to source files and
their associated environment (either global or package) are checked for
changes. This allows eligible breakpoints to be applied to classes that
have been reloaded, and line numbers to be synced with the file.

------------------------------------------------------------------------

------------------------------------------------------------------------

OoprBreakpoints

### Fields

- `rstudio`:

  *`[static]`*  
  `logical(1L)`  
  Tap into RStudios breakpoint mechanism. See section **RStudio** below.
  Cannot be used with `$volatile`.

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

- [`add`](#oopr_breakpoints-OoprBreakpoints-add):

  *`[static]`*  
  Add breakpoints.

- [`rm`](#oopr_breakpoints-OoprBreakpoints-rm):

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

## Examples

``` r
tmp <- tempfile();
on.exit(unlink(tmp));
cat(file = tmp, r"{
oopr("test",,
{
public:
  method <- \( )
  {
    "break me!";
  }
})
}");
source(tmp, keep.source = TRUE);
obj <- test();

OoprBreakpoints$add(tmp, 7L);
print(OoprBreakpoints);
#> <OoprBreakpoints ooprC: 0x55e3142d18e8>
#> Static Members:
#>  ├─$rstudio : logi FALSE
#>  ├─$volatile: logi TRUE
#>  ├─$quiet   : logi FALSE
#>  ├─$breaks  : ┬
#>  │            └─./../../../../../../../tmp/RtmpVHLJSp/file198377d3febf
#>  │              └─test
#>  │                └─method
#>  │                  └─7 [TRUE]
#>  │             
#>  ├─$add     :\(file, lines, conds = list())  
#>  └─$rm      :\(file, lines = integer(0L))  
if (FALSE) obj$method(); # \dontrun{}
```
