# Edit cols4all palettes (in development)

Edit cols4all palettes. c4a_duplicate duplicates an existing cols4all
palette, and c4a_modify is used to change the colors. Use c4a_data to
craete palettes from scratch.

## Usage

``` r
c4a_modify(palette, x = NULL, xNA = NULL)

c4a_duplicate(palette, name = NA)
```

## Arguments

- palette:

  name of the palette

- x:

  vector of the new colors. It should either the same length, or a named
  vector, where the names correspond to the index numbers. E.g.
  `c("3" = "#AABBCC")` will replace the third color with the color
  `"#AABBCC"`.

- xNA:

  the new color for missing values.

- name:

  name of new palette

## See also

[`c4a_data()`](https://cols4all.github.io/reference/c4a_data.md)

## Examples

``` r
c4a_duplicate("brewer.set2", "set2_mod")
c4a_modify("set2_mod", c("4" = "#EA8AB8"))
```
