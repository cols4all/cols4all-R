# Graphical user interface to analyse palettes

Graphical user interface to analyse palettes. `c4a_table` shows a table
that can be opened in the browser. `c4a_gui` is a graphical user
interface (shiny app) around this table.

## Usage

``` r
c4a_gui(type = "cat", n = NA, series = "all")

c4a_table(
  type = c("cat", "seq", "div", "cyc", "bivs", "bivc", "bivd", "bivg"),
  n = NULL,
  m = NULL,
  continuous = FALSE,
  filters = character(0),
  cvd.sim = c("none", "bw", "deutan", "protan", "tritan"),
  col.temp = 6500,
  sort = "name",
  text.format = "hex",
  text.col = "same",
  series = "all",
  range = NA,
  colorsort = "orig",
  include.na = FALSE,
  include_tritan = FALSE,
  show.scores = FALSE,
  columns = NA,
  verbose = TRUE
)
```

## Arguments

- type:

  type of palette. Run
  [`c4a_types`](https://cols4all.github.io/reference/c4a_palettes.md) to
  see the implemented types and their description. For `c4a_gui` it only
  determines which type is shown initially.

- n, m:

  `n` is the number of displayed colors. For bivariate palettes `"biv"`,
  `n` and `m` are the number of columns and rows respectively. If
  omitted: for `"cat"` the full palette is displayed, for `"seq"`,
  `"div"` and `"cyc"`, 7, 9, and 9 colors respectively, and for
  `"bivs"`/`"bivc"`/`"bivd"`/`"bivg"` 4 columns and rows. For `c4a_gui`
  it only determines which number of colors initially.

- series:

  Series of palettes to show. See
  [`c4a_series`](https://cols4all.github.io/reference/c4a_palettes.md)
  for options. By default, `"all"`, which means all series. For
  `c4a_gui` it only determines which series are shown initially.

- continuous:

  should the palettes as continuous instead of discrete. Only applicable
  for `"seq"`, `"div"`, and `"cyc"`.

- filters:

  filters to be applied. A character vector with a subset from:`"nmax"`
  (only palettes where `n = nmax`, which is only applicable for
  categorical palettes), `"cbf"` (colorblind-friendly), `"fair"`
  (fairness),`"naming"` (nameability), `"crW"` (sufficient contrast
  ratio with white), and `"crB"` (sufficient contrast ratio with black).
  By default an empty vector, so no filters are applied.

- cvd.sim:

  color vision deficiency simulation: one of `"none"`, `"bw"`,
  `"deutan"`, `"protan"`, `"tritan"`

- col.temp:

  display color temperature in Kelvin: one of `6500` (default), `4000`,
  `2700` and `2200`. Lower values simulate a display with a warmer white
  point, such as night mode. Viewers partly adapt to a warmer white, so
  the perceived shift is much smaller than the physical shift of the
  emitted light. To approximate the perceived colors, white is mapped to
  a fixed perceived white per temperature (the page background used in
  `c4a_gui()`: `#F4F1EA`, `#F3E6D3` and `#ECD6B3` respectively), and all
  other colors are transformed accordingly with a von Kries chromatic
  adaptation transform in Bradford cone space. The amount of adaptation
  in reality depends on viewing conditions and time.

- sort:

  column name to sort the data. The available column names depend on the
  arguments `type` and `show.scores`. They are listed in the warning
  message. Use a `"-"` prefix to reverse the order.

- text.format:

  The format of the text of the colors. One of `"hex"`, `"RGB"` or
  `"HCL"`.

- text.col:

  The text color of the colors. By default `"same"`, which means that
  they are the same as the colors themselves (so invisible, but
  available for selection). `"auto"` means automatic: black for light
  colors and white for dark colors.

- range:

  vector of two numbers that determine the range that is used for
  sequential and diverging palettes. Both numbers should be between 0
  and 1. The first number determines where the palette begins, and the
  second number where it ends. For sequential palettes, 0 means the
  leftmost (normally lightest) color, and 1 the rightmost (often
  darkest) color. For diverging palettes, 0 means the middle color, and
  1 both extremes. If only one number is provided, this number is
  interpreted as the endpoint (with 0 taken as the start). By default,
  it is set automatically, based on `n`.

- colorsort:

  Sort the colors (`"cat"` only). Options: `"orig"` (original order),
  `"Hx"` (hue, where x is a starting number from 0 to 360), `"C"`
  (chroma), `"L"` (luminance)

- include.na:

  should color for missing values be shown? `FALSE` by default

- include_tritan:

  should tritan (blue-yellow color vision deficiency, about 1 in 10,000
  people) be taken into account by the colorblind-friendly filter
  (`filters = "cbf"`) and badge, besides deutan and protan (red-green,
  together about 4% of the population)? `FALSE` by default, as in
  `c4a_gui()`.

- show.scores:

  should scores of the quality indicators be printed? See details for a
  description of those indicators.

- columns:

  number of columns. By default equal to `n` or, if not specified, 12.
  Cannot be higher than the palette lengths.

- verbose:

  should messages and warnings be printed?

## Value

An HMTL table (`kableExtra` object)

## Details

See vignette how the properties are calculated. Parameters, such as
threshold values which determined when palettes are classified as
"colorblind-friendly", can be specified via
[`c4a_options`](https://cols4all.github.io/reference/c4a_options.md).
Also the nameability score function (which is in development) can be
specified there. See the examples of
[`c4a_options`](https://cols4all.github.io/reference/c4a_options.md) for
both use cases.

## See also

References of the palettes:
[`cols4all-package`](https://cols4all.github.io/reference/cols4all-package.md).

## Examples

``` r
if (requireNamespace("shiny") &&
  requireNamespace("shinyjs") &&
  requireNamespace("kableExtra") &&
  requireNamespace("colorblindcheck") &&
  interactive()) {

c4a_gui()

# categorical palettes with maximum number of colors
c4a_table(type = "cat")

# sort sequential palettes by hue
c4a_table(type = "seq", n = 7, sort = "H")

# sort sequential palettes by hue type (how many hues are used)
c4a_table(type = "seq", n = 5, sort = "hues")
}
```
