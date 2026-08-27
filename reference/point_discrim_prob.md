# Point-mark discrimination probability for a pair of colors

The point-mark analogue of
[`line_discrim_prob()`](https://cols4all.github.io/reference/line_discrim_prob.md):
predicts the proportion of viewers who would detect the color difference
between two circular scatterplot-style point marks, as a function of
point diameter, using the model fit by Szafir (2018) for scatterplots
(Experiment One; see Details).

## Usage

``` r
point_discrim_prob(hex1, hex2, size = seq(0.25, 2, by = 0.05))
```

## Arguments

- hex1, hex2:

  two hex color strings, e.g. `"#3B4CC0"` and `"#B40426"`

- size:

  numeric vector of point diameters in degrees of visual angle. Szafir's
  tested range was 0.25 to 2.0 degrees; see the validated-range caveat
  above for values outside that range.

## Value

data.frame with columns `size_deg` and `p_discriminable`

## Details

Model: `p = m_x(s) * dx`, `m_x(s) = c_x + k_x / s`, per CIELAB axis
(L\*, a\*, b\*), combined across axes as `sqrt(sum((dx * m_x(s))^2))`
and clipped to `[0, 1]` – identical form to
[`line_discrim_prob()`](https://cols4all.github.io/reference/line_discrim_prob.md),
but with its own coefficients fit on point marks, and `s` is point
*diameter* rather than line thickness.

**Caveats** (see
[`line_discrim_prob()`](https://cols4all.github.io/reference/line_discrim_prob.md)
for the shared ones – white background only, cross-axis combination
untested):

- **Validated range.** Fit using six diameter steps from 0.25 to 2.0
  degrees. This is a much higher floor than the line model's 0.05-0.35
  degrees – **do not** feed sizes below roughly 0.25 degrees into this
  function. Below about 0.1-0.2 degrees (depending on axis),
  `c_x + k_x/s` goes negative, which is not a meaningful extrapolation,
  just a sign flip from being far outside the fitted domain.

- **Points are less discriminable than lines of the same size.**
  Szafir's own comparison found colors generally more discriminable on
  elongated marks (lines, bars) than on points – don't expect a point
  and a line of the same visual-angle size to give similar probabilities
  for the same color pair.

## References

Szafir, D.A. (2018). Modeling Color Difference for Visualization Design.
IEEE Transactions on Visualization and Computer Graphics, 24(1),
Experiment One.

## See also

[`line_discrim_prob()`](https://cols4all.github.io/reference/line_discrim_prob.md),
[`palette_prob_bg()`](https://cols4all.github.io/reference/palette_prob_bg.md)
