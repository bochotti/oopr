# covr for oopr

Include `oopr` classes in package coverage, or test the coverage of a
single class.

## Usage

``` r
OoprCovr()
```

## Details

For package coverage, write code to initialize this class inside
`./tests/testthat/setup.R`

## 

------------------------------------------------------------------------

------------------------------------------------------------------------

OoprCovr

### Methods

- `class`:

  Test a single class.

------------------------------------------------------------------------

class

#### Description

Test a single class.

#### Usage

``` R

class(ooprC, file = NULL, report = TRUE)
```

#### Arguments

### 

[TABLE]

#### Details

`file` guessing assumes `testhat` is being used in a "local" package.

#### Returns

An object of class `coverage`.

## Examples

``` r
if (FALSE) { # \dontrun{
# ./tests/testthat/setup.R
OoprCovr();} # }
```
