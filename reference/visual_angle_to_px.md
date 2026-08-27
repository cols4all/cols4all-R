# Convert a line thickness from degrees of visual angle to pixels

The defaults (30 inch viewing distance, 96 dpi) match Szafir (2018)'s
own assumed viewing conditions. Override both to match an actual lab
setup (measured pixel pitch and viewing distance), rather than trusting
a reported dpi, which is frequently wrong for actual displays.

## Usage

``` r
visual_angle_to_px(
  deg,
  viewing_distance_in = 30,
  dpi = 96,
  convention = .C4A$angle_convention
)
```

## Arguments

- deg:

  line thickness in degrees of visual angle

- viewing_distance_in:

  viewing distance, in inches

- dpi:

  display resolution, in dots (pixels) per inch

- convention:

  `"szafir"` or `"standard"` – see Details. Defaults to the
  `angle_convention` package option (see
  [`c4a_options()`](https://cols4all.github.io/reference/c4a_options.md)).

## Value

line thickness in pixels

## Details

The standard chord formula for the linear size subtending a visual angle
`deg` at distance `d` is `2 * d * tan(deg/2)` – this is what
`convention = "standard"` computes. Szafir (2018)'s own in-text
px-equivalents (e.g. "0.25 deg (6px)", "2.0 deg (50px)" for points;
"0.05 deg (1px)", "0.35 deg (9px)" for lines) are consistently exactly
*half* of that formula's result at their own stated 30in/96dpi
assumptions – across both experiments, at four independently checked
(angle, px) pairs, which is why this is treated as their own conversion
convention rather than a one-off rounding difference.
`convention = "szafir"` (the default, and the package default via
`c4a_options("angle_convention")`) reproduces that, so preview sizes in
[`palette_prob_bg()`](https://cols4all.github.io/reference/palette_prob_bg.md)
/ [`c4a_gui()`](https://cols4all.github.io/reference/c4a_gui.md)
visually match the paper's own figures; `convention = "standard"` gives
the textbook-correct value, which is what you want once calibrating
against a real measured display.

## See also

[`px_to_visual_angle()`](https://cols4all.github.io/reference/px_to_visual_angle.md),
[`line_discrim_prob()`](https://cols4all.github.io/reference/line_discrim_prob.md),
[`c4a_options()`](https://cols4all.github.io/reference/c4a_options.md)
