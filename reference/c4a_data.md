# Build and load palette data

Build palette data. Both `c4a_data` and `c4a_data_as_is` build data
palette. The difference is that the former may restructure the palette
colors (see details) whereas the latter takes the palette colors as they
are. Data can subsequently be loaded into cols4all via `c4a_load`. The
`c4a_data` function can also be used to read `c4a_info` objects, which
contain data for a single palette.

## Usage

``` r
c4a_data(
  x,
  xNA = NA,
  types = "cat",
  series = "x",
  nmin = NA,
  nmax = NA,
  ndef = NA,
  mmin = NA,
  mmax = NA,
  mdef = NA,
  format.palette.name = TRUE,
  remove.blacks = NA,
  remove.whites = NA,
  take.gray.for.NA = FALSE,
  remove.other.grays = FALSE,
  light.to.dark = FALSE,
  remove.names = TRUE,
  biv.method = "byrow",
  space = "rgb",
  range_matrix_args = list(NULL),
  bib = NA,
  description = NA
)

c4a_load(data, overwrite = FALSE)

c4a_data_as_is(
  ...,
  format.palette.name = FALSE,
  remove.blacks = FALSE,
  remove.whites = FALSE,
  take.gray.for.NA = FALSE,
  remove.other.grays = FALSE,
  light.to.dark = FALSE,
  remove.names = FALSE
)
```

## Arguments

- x:

  either a named list of color palettes or a
  [`c4a_info`](https://cols4all.github.io/reference/c4a_info.md) object.
  For the first case: see details for indexing. The second case will
  bypass the other arguments.

- xNA:

  colors for missing values. Vector of the same length as x (or length
  1). For `NA` values, the color for missing values is automatically
  determined (preferable a light grayscale color, but if it is
  indistinguishable by color blind people, a light color with a low
  chroma value is selected)

- types:

  character vector of the same length as x (or length 1), which
  determines the type of palette: `"cat"`, `"seq"`, `"div"`, `"cyc"`,
  `"bivs"`, `"bivc"`, `"bivd"`, or `"bivg"`. See details.

- series:

  a character vector of the same length as x (or length 1), which
  determines the series.

- nmin, nmax, ndef:

  minimum / maximum / default number of colors for the palette. By
  default: `nmin = 1`, for `"cat"` `nmax` and `ndef` the number of
  supplied colors. For the other types, `nmax` is `Inf`. `ndef` is 7 for
  `"seq"`, 9. For diverging palettes, these numbers refer to the number
  of columns. (See `mmin`, `mmax`, `mdef` for the rows)

- mmin, mmax, mdef:

  minimum / maximum / default number of rows for bivariate palettes.

- format.palette.name:

  should palette names be formatted to lowercase/underscore format?

- remove.blacks, remove.whites, take.gray.for.NA, remove.other.grays:

  These arguments determine the processing of grayscale colors for
  categorical `"cat"` palettes: if `remove.blacks` and there are (near)
  blacks, these are removed first. Next, if `take.gray.for.NA`, `xNA` is
  `NA`, and a palette contains at least one grayscale color (which can
  also be white), this is used as color for missing values. In case
  there are more than one grayscale color, the lightest is taken.
  `remove.other.grays` determines what happens with the other grays.

- light.to.dark:

  should sequential `"seq"` palettes be automatically ordered from light
  to dark?

- remove.names:

  should individual color names be removed?

- biv.method:

  method to a create bivariate palette. Options are `"byrow"` means that
  the colors are wrapped row-wise to a color matrix where the number of
  rows and columns is automatically determined, `"byrowX"` the same but
  with X (integer between 2 and 9) columns, `"bycol"` and `"bycolX`
  similar but wrapped column-wise. `"div2seqseq"` and `"div2catseq`
  means that colors are extracted from a divering palette. The former
  translates colors into a matrix with the neutral color in the
  diagonal, while the latter places the neutral color in the middle
  column. `"seq2uncseq"`

- space:

  color space in which interpolated colors are determined. Options:
  `"rgb"` (RGB) and `"Lab"` (CIE Lab).

- range_matrix_args:

  list of lists, one for each palette. Each such list specifies the
  range of sequential and diverging palettes, in case they are not
  indexed. See details.

- bib:

  bibtex reference in the form of a
  [`utils::bibentry`](https://rdrr.io/r/utils/bibentry.html) object.

- description:

  description of the series. If `series` contains multiple series
  (rather than one value), please specify a vector of the same length as
  `series`. See
  [`c4a_series`](https://cols4all.github.io/reference/c4a_palettes.md)
  for the descriptions of the currently loaded series.

- data:

  cols4all data created with `c4a_data`

- overwrite:

  in case the palettes already exist (i.e. the full names), should the
  old names be overwritten?

- ...:

  passed on to `c4a_data`

## Value

`c4a_data` object, which is a list of four items: `data`, `s`,
`citation`, and `description`

## Details

In cols4all, palettes are organized by series and by type. The
**series** or 'family' specifies where the palettes belong to. For
instance `"brewer"` stands for the color palettes from ColorBrewer. Run
[`c4a_series`](https://cols4all.github.io/reference/c4a_palettes.md) to
get an overview of loaded series. The **type** specifies what kind of
palette it is; see
[`c4a_types`](https://cols4all.github.io/reference/c4a_palettes.md) for
a description of the implemented ones.

This function structures the palette data, such that it is consistent
with the other palette data. This includes:

- Palette names are made consistent. We use the convention
  `"my_series.my_palette"`, so all lower case, a period to separate the
  series name from the palette name, and underscores to separate words.

- (Only for `c4a_data`, bypassed for `c4a_data_as_is`) Categorical
  palettes: black is removed from categorical palettes, and a grayscale
  color is assigned to be used for missing values (other grayscale
  colors are removed). Sequential palettes are sorted from light to
  dark.

Indexing: for a categorical `"cat"` palette, an optional `"index"`
attribute determines which colors to use for which lengths: if the
palette consists of k colors, index should be a list of k, where the
i-th element is an integer vector of length i with values 1,2,...,k. See
`c4a_info("rainbow")` and for an example.

Range: sequential and diverging palettes are usually defined for 9+
colors. The optional `"range_matrix"` attribute determines that range is
used for less colors. It is a n x 2 matrix where row i defines the
applied range of a palette of length i. For sequential palettes a range
`c(0,1)` means that the palette is generated (via a color ramp) between
the two outermost colors. For diverging palettes, a range `c(x, y)`
means that both sides of the palette are generated (via a color ramp)
from `x`, which is the distance to the center color, to `y` which
represents both outermost colors.

The range is automatically set for sequential and diverging palettes
that have no `"index"` or `"range_matrix"` attribute via the parameter
`range_matrix_args`, which is a list per palette. The arguments for a
sequential palette are: `nmin` the minimum number of colors for which
the range is reduced, `nmax`, the number of colors for which the range
is set to `c(0,1)`, `slope_min` and `slope_max` determine the slopes of
range reduction from a palette of length `nmax` to `nmin`, and `space`
sets the color space for which the color ramp is applied (`"rgb"` or
`"Lab"`). The arguments for a diverging palette are the same, but only
one `slope` is used (namely for the outermost colors).

It may take some time to process, especially large categorical palettes,
because of calculations of the color blind checks.

## Examples

``` r
# palettes extracted Pink Floyd albums
pf = list(piper = c("#391C1C", "#C6C6AA", "#713939", "#C6391C",
    "#C6E3C6", "#AA7155", "#AA8E71", "#C68E71"),
      saucerful = c("#000000", "#1C1C1C", "#393939", "#FFFFFF",
    "#555555", "#8E8E71", "#E3C6AA", "#715539"),
      atom = c("#C6E3FF", "#397139", "#557139", "#E3E3C6",
    "#1C1C1C", "#1C551C", "#AAAA8E", "#8EC6E3"),
      meddle = c("#715539", "#553939", "#8E7155", "#71AAAA",
    "#8E8E71", "#1CAAE3", "#55C6E3", "#AA7155"),
      obscured = c("#000000", "#1C1C1C", "#393939", "#717155",
    "#8E8E71", "#715539", "#C6AA8E", "#E3C6AA"),
      moon = c("#000000", "#FF0000", "#FF9224", "#FFFF00",
    "#71C600", "#00C6FF", "#8E398E", "#FFFFFF"),
      wish = c("#FFFFFF", "#AAC6E3", "#8E8E8E", "#717155",
    "#555539", "#8E8E71", "#555555", "#8E7155"),
      animals = c("#391C39", "#393955", "#E3C671", "#718E8E",
    "#AAAA8E", "#C67139", "#AA5539", "#E3AA39"),
      wall = c("#FFFFFF", "#E3E3E3", "#C6C6C6", "#AAAAC6",
    "#1C1C1C", "#000000", "#8E8E8E", "#E3C6E3"),
      cut = c("#000000", "#E30000", "#AA0000", "#391C55",
    "#FFE3E3", "#1C1C00", "#FFAA55", "#8E8E55"),
      lapse = c("#000000", "#8E8EC6", "#8E8E71", "#7171AA",
    "#39391C", "#717171", "#AAAAAA", "#E3E3E3"),
      division = c("#000000", "#FFFFC6", "#00398E", "#AA8E55",
    "#39558E", "#C6AA71", "#39391C", "#555571"),
      more = c("#0055AA", "#FFAA1C", "#1C71AA", "#003971",
    "#E38E55", "#E3AAAA", "#718EAA", "#71718E"),
      umma = c("#AA8E71", "#555539", "#39391C", "#1C1C1C",
    "#E3E3C6", "#715539", "#391C1C", "#8E7155"),
      relics = c("#3955AA", "#1C3971", "#5571C6", "#715555",
    "#8E7155", "#E3AA71", "#8E8EAA", "#E3FFFF"),
      river = c("#393939", "#555555", "#39558E", "#C6C6C6",
    "#718EAA", "#1C1C1C", "#717171", "#E3C68E"))

if (requireNamespace("colorblindcheck", quietly = TRUE)) {
  pfdata = c4a_data_as_is(pf, series = "pinkfloyd",
    description = "Palettes extracted from Pink Floyd album covers")
  c4a_load(pfdata)

  c4a_series()
  c4a_overview()

  if (requireNamespace("shiny") &&
    requireNamespace("shinyjs") &&
    requireNamespace("kableExtra") &&
    requireNamespace("colorblindcheck") &&
    requireNamespace("plotly") &&
    interactive()) {
    c4a_gui(series = "pinkfloyd", n = 8)
  }
}
#> Loading required namespace: shiny
#> Loading required namespace: shinyjs
#> Loading required namespace: kableExtra
#> Loading required namespace: plotly
```
