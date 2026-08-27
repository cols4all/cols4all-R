# Line-mark discrimination probability for a pair of colors

Predicts the proportion of viewers who would detect the color difference
between two line marks, as a function of line thickness, using the model
fit by Szafir (2018) for line graphs (see Details).

## Usage

``` r
line_discrim_prob(hex1, hex2, thickness = seq(0.05, 0.5, by = 0.01))
```

## Arguments

- hex1, hex2:

  two hex color strings, e.g. `"#3B4CC0"` and `"#B40426"`

- thickness:

  numeric vector of line thicknesses in degrees of visual angle.
  Szafir's tested range was 0.05 to 0.35 degrees; the default spans a
  bit wider for a smooth curve, but see the validated-range caveat above
  for values outside that range.

## Value

data.frame with columns `thickness_deg` and `p_discriminable`

## Details

Model: `p = m_x(s) * dx`, `m_x(s) = c_x + k_x / s`, per CIELAB axis
(L\*, a\*, b\*), combined across axes as `sqrt(sum((dx * m_x(s))^2))`
and clipped to `[0, 1]`. `s` is line thickness in degrees of visual
angle, `dx` is the absolute difference between the two colors along that
axis.

**Caveats:**

- **White background only.** Szafir's stimuli were rendered on plain
  white; this is a white-background baseline and says nothing about gray
  or black backgrounds.

- **Validated range.** The regressions were fit using six
  color-difference steps per axis, all below the detection asymptote,
  and thickness was tested from 0.05 to 0.35 degrees. Outside that range
  the model is extrapolating: it is linear in `dx` per axis, so nothing
  stops a raw prediction from exceeding 1 (clipped here, which produces
  a kink rather than the smooth saturation a real psychometric function
  would have).

- **Cross-axis combination is untested.** Szafir's experiment held
  "which axis differs" as a between-participants factor, so no
  participant judged a pair differing on more than one axis at once. The
  Euclidean combination above is the paper's own proposed generalization
  for real (multi-axis) color pairs, but it was not directly tested that
  way.

## References

Szafir, D.A. (2018). Modeling Color Difference for Visualization Design.
IEEE Transactions on Visualization and Computer Graphics, 24(1),
Experiment 3.

## See also

[`point_discrim_prob()`](https://cols4all.github.io/reference/point_discrim_prob.md),
[`palette_prob_bg()`](https://cols4all.github.io/reference/palette_prob_bg.md),
[`visual_angle_to_px()`](https://cols4all.github.io/reference/visual_angle_to_px.md)
