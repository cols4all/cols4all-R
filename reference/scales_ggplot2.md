# col4all scales for ggplot2

col4all scales for ggplot2. The scale functions are organized as
`scale_<aesthetic>_<mapping>_c4a_<type>`, where the `<aesthetic>` should
be either `colo(u)r` or `fill`, `<mapping>` refers to the mapping that
is applied (`discrete`, `continuous` or `binned`), and `<type>` is the
palette type: `cat`, `seq`, or `div`.

## Usage

``` r
scale_color_discrete_c4a_cat(
  palette = NULL,
  reverse = FALSE,
  order = NULL,
  ...
)

scale_colour_discrete_c4a_cat(
  palette = NULL,
  reverse = FALSE,
  order = NULL,
  ...
)

scale_fill_discrete_c4a_cat(palette = NULL, reverse = FALSE, order = NULL, ...)

scale_color_discrete_c4a_seq(
  palette = NULL,
  reverse = FALSE,
  range = NULL,
  ...
)

scale_colour_discrete_c4a_seq(
  palette = NULL,
  reverse = FALSE,
  range = NULL,
  ...
)

scale_fill_discrete_c4a_seq(palette = NULL, reverse = FALSE, range = NULL, ...)

scale_color_discrete_c4a_div(
  palette = NULL,
  reverse = FALSE,
  range = NULL,
  ...
)

scale_colour_discrete_c4a_div(
  palette = NULL,
  reverse = FALSE,
  range = NULL,
  ...
)

scale_fill_discrete_c4a_div(palette = NULL, reverse = FALSE, range = NULL, ...)

scale_color_continuous_c4a_seq(
  palette = NULL,
  reverse = FALSE,
  range = NULL,
  mid = 0,
  n_interp = 11,
  ...
)

scale_colour_continuous_c4a_seq(
  palette = NULL,
  reverse = FALSE,
  range = NULL,
  mid = 0,
  n_interp = 11,
  ...
)

scale_fill_continuous_c4a_seq(
  palette = NULL,
  reverse = FALSE,
  range = NULL,
  mid = 0,
  n_interp = 11,
  ...
)

scale_color_continuous_c4a_div(
  palette = NULL,
  reverse = FALSE,
  range = NULL,
  mid = 0,
  n_interp = 11,
  ...
)

scale_colour_continuous_c4a_div(
  palette = NULL,
  reverse = FALSE,
  range = NULL,
  mid = 0,
  n_interp = 11,
  ...
)

scale_fill_continuous_c4a_div(
  palette = NULL,
  reverse = FALSE,
  range = NULL,
  mid = 0,
  n_interp = 11,
  ...
)

scale_color_binned_c4a_seq(
  palette = NULL,
  reverse = FALSE,
  range = NULL,
  mid = 0,
  n_interp = 11,
  ...
)

scale_colour_binned_c4a_seq(
  palette = NULL,
  reverse = FALSE,
  range = NULL,
  mid = 0,
  n_interp = 11,
  ...
)

scale_fill_binned_c4a_seq(
  palette = NULL,
  reverse = FALSE,
  range = NULL,
  mid = 0,
  n_interp = 11,
  ...
)

scale_color_binned_c4a_div(
  palette = NULL,
  reverse = FALSE,
  range = NULL,
  mid = 0,
  n_interp = 11,
  ...
)

scale_colour_binned_c4a_div(
  palette = NULL,
  reverse = FALSE,
  range = NULL,
  mid = 0,
  n_interp = 11,
  ...
)

scale_fill_binned_c4a_div(
  palette = NULL,
  reverse = FALSE,
  range = NULL,
  mid = 0,
  n_interp = 11,
  ...
)
```

## Arguments

- palette, reverse, order, range:

  See [`c4a`](https://cols4all.github.io/reference/c4a.md).

- ...:

  parameters passed on to the underlying scale functions:
  [`discrete_scale`](https://ggplot2.tidyverse.org/reference/discrete_scale.html),
  [`continuous_scale`](https://ggplot2.tidyverse.org/reference/continuous_scale.html),
  and
  [`binned_scale`](https://ggplot2.tidyverse.org/reference/binned_scale.html).

- mid:

  data value that should be mapped to the mid-point of the diverging
  color scale. By default 0, which is useful for many use cases.
  However, if the data has only positive (or only negative) values, it
  may be worthwhile to specify this. Use `NA` to set it to the middle of
  the value range.

- n_interp:

  number of discrete colors that should be used to interpolate the
  continuous color scale. Recommended to use an odd number to include
  the midpoint

## Value

A ggplot2 component that defines the scale

## Examples

``` r
if (require("ggplot2")) {
  data("diamonds")
  diam_exp = diamonds[diamonds$price >= 15000, ]
  diam_exp$clarity[1:500] = NA

  # discrete categorical scale
  ggplot(diam_exp, aes(x = carat, y = price, color = color)) +
    geom_point(size = 2) +
    scale_color_discrete_c4a_cat("carto.safe") +
    theme_light()

  # missing values
  c4a_plot("tol.muted", 8)
  ggplot(diam_exp, aes(x = carat, y = price, fill = clarity)) +
    geom_point(size = 2, shape = 21) +
    scale_fill_discrete_c4a_cat("tol.muted") +
    theme_light()

  # discrete sequential scale
  ggplot(diam_exp, aes(x = carat, y = price, color = cut)) +
    geom_point(size = 2) +
    scale_color_discrete_c4a_seq("hcl.blues2") +
    theme_light()

  # continuous sequential scale
  ggplot(diam_exp, aes(x = carat, y = price, color = depth)) +
    geom_point(size = 2) +
    scale_color_continuous_c4a_seq("hcl.blues2", range = c(0.4, 1)) +
    theme_light()

  # continuous diverging scale
  ggplot(diam_exp, aes(x = carat, y = depth, color = price)) +
    geom_point(size = 2) +
    scale_color_continuous_c4a_div("wes.zissou1", mid = NA) +
    theme_light()

  # binned sequential scale
  ggplot(diam_exp, aes(x = carat, y = price, color = depth)) +
    geom_point(size = 2) +
    scale_color_binned_c4a_seq("scico.batlow", range = c(0.4, 1)) +
    theme_light()
}
#> Loading required package: ggplot2

```
