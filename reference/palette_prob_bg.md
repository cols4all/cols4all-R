# Pairwise mark-discrimination probability matrix for a palette

The probability analogue of `palette_dist_bg()`'s `"min-lum"` method:
for every pair of colors in `x`, the probability that two `size`-sized
marks (line marks, or point/scatterplot marks) drawn in those colors
would be told apart, using the Szafir (2018) mark-discrimination models
(see
[`line_discrim_prob()`](https://cols4all.github.io/reference/line_discrim_prob.md)
/
[`point_discrim_prob()`](https://cols4all.github.io/reference/point_discrim_prob.md))
instead of a
[`colorblindcheck::palette_dist()`](https://jakubnowosad.com/colorblindcheck/reference/palette_dist.html)
distance.

## Usage

``` r
palette_prob_bg(
  x,
  bgcol = "#FFFFFF",
  cvd = NULL,
  severity = 1,
  thickness = 0.05,
  mark = c("line", "point"),
  metric = 2000,
  lum_k = 100,
  lum_floor = 0.05,
  lum_adjust = FALSE
)
```

## Arguments

- x:

  vector of hex colors (the palette)

- bgcol:

  background color as a hex string. Only used when `lum_adjust = TRUE`.

- cvd:

  type of color vision deficiency to simulate before computing
  probabilities, or `NULL` for none. See
  [`colorblindcheck::palette_dist()`](https://jakubnowosad.com/colorblindcheck/reference/palette_dist.html)
  for supported values.

- severity:

  severity of the CVD simulation, between 0 and 1

- thickness:

  mark size in degrees of visual angle – line thickness when
  `mark = "line"`, point diameter when `mark = "point"` – passed to the
  underlying
  [`line_discrim_prob()`](https://cols4all.github.io/reference/line_discrim_prob.md)
  /
  [`point_discrim_prob()`](https://cols4all.github.io/reference/point_discrim_prob.md)
  model. See their documentation for the (different) validated ranges.

- mark:

  which Szafir (2018) model to use: `"line"` (line graphs) or `"point"`
  (scatterplots). See
  [`line_discrim_prob()`](https://cols4all.github.io/reference/line_discrim_prob.md)
  and
  [`point_discrim_prob()`](https://cols4all.github.io/reference/point_discrim_prob.md).

- metric:

  accepted for signature parity with `palette_dist_bg()`, but unused:
  this model works directly on CIELAB L\*/a\*/b\*, not on a
  [`colorblindcheck::palette_dist()`](https://jakubnowosad.com/colorblindcheck/reference/palette_dist.html)
  distance.

- lum_k, lum_floor:

  passed to `lum_factor()` when `lum_adjust = TRUE`; see that function's
  documentation for their meaning.

- lum_adjust:

  should the probability be multiplied by `lum_factor()`? Exploratory
  and unvalidated – see Details.

## Value

n x n matrix of discrimination probabilities as percentages (0-100),
with `NA` on the diagonal

## Details

Only `"min-lum"`'s weakest-link-plus-lightness-penalty idea is mirrored
here, since it is the only `palette_dist_bg()` method with a coherent
probability interpretation: `"bg-norm"` and `"bg-norm-cr-spread"` are
distance-space transforms (squared-distance ratios, log-contrast spread)
that don't map onto a `[0, 1]`-bounded probability, so there is no
`method` argument here.

Unlike `palette_dist_bg()`, this function does **not** compute a
mark-vs-background discriminability term (the `bdiff` piece of
`"min-lum"`). Szafir's models were fit on two thin/small marks compared
against each other on a field of gray distractors, not a mark against a
large uniform background field, so reusing that ΔE2000-based `bdiff`
logic here is not well founded. `lum_adjust = TRUE` is offered as an
exploratory alternative instead (see below) – if a background-visibility
floor is wanted for this model, it needs its own justification, not a
straight port from the distance version.

**Caveats**, in addition to those on
[`line_discrim_prob()`](https://cols4all.github.io/reference/line_discrim_prob.md)
/
[`point_discrim_prob()`](https://cols4all.github.io/reference/point_discrim_prob.md)
(note the two marks have different validated size ranges – 0.05-0.35
degrees for `"line"`, 0.25-2.0 degrees for `"point"`):

- **`lum_adjust = TRUE` is exploratory, not validated.** It multiplies
  the Szafir-derived probability by `lum_factor()`, which was fit on
  ΔE-based *distance* data, not probabilities. There is no empirical
  basis yet for combining the two this way – treat it as a hypothesis to
  test, not a trustworthy correction. It defaults to `FALSE`.

## See also

[`line_discrim_prob()`](https://cols4all.github.io/reference/line_discrim_prob.md),
[`point_discrim_prob()`](https://cols4all.github.io/reference/point_discrim_prob.md)
