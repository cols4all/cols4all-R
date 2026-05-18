# Import and export system data

Import and export system data. `c4a_sysdata_import` will import system
data and overwrite the current system data, `c4a_sysdata_export` will
export the current system data, and `c4a_sysdata_remove` (partly)
removes system data.

## Usage

``` r
c4a_sysdata_import(data)

c4a_sysdata_export()

c4a_sysdata_remove(fullnames = NULL, series = NULL, are.you.sure = NA)
```

## Arguments

- data:

  cols4all data (see `c4a_data`)

- fullnames:

  full palette names (so in the format `series.palette_name`)

- series:

  a character vector of series names that should be removed (use `"all"`
  to remove all).

- are.you.sure:

  are you sure you want to remove series?

## Value

`c4a_sysdata_export` returns the system data (a list)

## Examples

``` r
x = c4a_sysdata_export()
c4a_sysdata_import(x)
#> cols4all system data imported successfully
y = c4a_sysdata_export()
identical(x, y)
#> [1] TRUE
```
