# Breakpoints for oopr

Use RStudio breakpoints with `oopr` classes.

## Usage

``` r
oopr_breakpoints(on = NA, force = FALSE)
```

## Arguments

- on:

  `logical(1L)`  
  To enable/disable breakpoints. If the default, `NA`, checks whether
  they are currently enabled.

- force:

  `logical(1L)`  
  Force assigning or removing the required functions to/from the global
  environment.

## Value

`logical(1L)` indicating whether this functionality is enabled. Any
breakpoints set are printed to the console.

## Details

Setting breakpoints inside `oopr` classes is the same process as setting
them for normal functions - use the gutter or `SHIFT+F9`.

This feature requires intercepting RStudio calls to functions that
normally reside within `tools:rstudio`, by assigning functions of the
same name inside the global environment. It is disabled by default as it
is potentially destructive for an R session. Environmental variable
`R_OOPR_BREAKPOINTS=true` can be used to enable this functionality when
`oopr` namespace is loaded.

Breakpoints can be set for both methods and properties. One limitation
is that breakpoints cannot be set on single line functions with a
specifier,

    get:x <- \( ) { return("x"); }

as RStudio thinks the line is at the top-level.

When a breakpoint is set class instances are searched in the global
environment and package namespace (if applicable). If the function
inside an instance does not match the source file, then all breakpoints
are removed for that function in that instance.

Tested with RStudio version 2026.1.1.403, there are no guarantees for
earlier or later versions.
