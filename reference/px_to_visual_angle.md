# Convert a line thickness from pixels to degrees of visual angle

Inverse of
[`visual_angle_to_px()`](https://cols4all.github.io/reference/visual_angle_to_px.md).
See its documentation for the default viewing conditions, the
`convention` argument, and why the conditions should be overridden for a
real setup.

## Usage

``` r
px_to_visual_angle(
  px,
  viewing_distance_in = 30,
  dpi = 96,
  convention = .C4A$angle_convention
)
```

## Arguments

- px:

  line thickness in pixels

- viewing_distance_in:

  viewing distance, in inches

- dpi:

  display resolution, in dots (pixels) per inch

- convention:

  `"szafir"` or `"standard"` – see
  [`visual_angle_to_px()`](https://cols4all.github.io/reference/visual_angle_to_px.md).
  Defaults to the `angle_convention` package option (see
  [`c4a_options()`](https://cols4all.github.io/reference/c4a_options.md)).

## Value

line thickness in degrees of visual angle

## See also

[`visual_angle_to_px()`](https://cols4all.github.io/reference/visual_angle_to_px.md),
[`line_discrim_prob()`](https://cols4all.github.io/reference/line_discrim_prob.md),
[`c4a_options()`](https://cols4all.github.io/reference/c4a_options.md)
