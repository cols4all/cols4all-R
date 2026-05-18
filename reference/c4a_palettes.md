# Get available palette names and series

`c4a_palettes` lists all available cols4all color palettes. Palettes are
organized by series. The available series are listed with `c4a_series`.
Palettes are also organized per functional type, where we currently
support: categorical `"cat"`, sequential `"seq"`, diverging `"div"`",
cyclic `"cyc"`, and bivariate (seq x seq `"bivs"`, seq x cat `"bivc"`,
seq x div `"bivd"`, seq x desaturated `"bivg"`) palette types. The
function `c4a_types` lists all available types. The function
`c4a_overview` gives an overview table of the number of palette per
series and type. In an IDE with auto-completion (such as RStudio) it is
possible to browse through the palette names with `.P` (using `$` like
in lists).

## Usage

``` r
c4a_palettes(
  type = c("all", "cat", "seq", "div", "cyc", "bivs", "bivc", "bivd", "bivg"),
  series = NULL,
  full.names = TRUE
)

c4a_series(type = c("all", "cat", "seq", "div", "cyc"), as.data.frame = TRUE)

c4a_types(series = NULL, as.data.frame = TRUE)

c4a_overview(return.matrix = FALSE, zero.count.as.NA = FALSE)

.P
```

## Format

An object of class `environment` of length 21.

## Arguments

- type:

  type of color palette: one of `"all"` (all palettes), `"cat"`,
  `"seq"`, `"div"`, `"cyc"`, `"bivs"`, `"bivc"`, `"bivd"`, or `"bivg"`.
  See `c4a_types` for descriptions.

- series:

  series to list the palettes from. Run `c4a_series` to see the options.

- full.names:

  should full names, i.e. with the prefix "series."? By default `TRUE`.

- as.data.frame:

  should `c4a_series` and `c4a_types` return the result as a data.frame,
  with description included as a column?

- return.matrix:

  should only a matrix be returned with numbers per palette and type? If
  `FALSE` a data.frame is returned with addional information

- zero.count.as.NA:

  should zeros counted in the table be returned as 0 (`FALSE`, default)
  or as `NA` (`TRUE`)?

## Value

names of the loaded color palettes

## See also

References of the palettes:
[`cols4all-package`](https://cols4all.github.io/reference/cols4all-package.md).

## Examples

``` r
c4a_series()
#>        series                                         description
#> 1     bivario            Palettes from the Python library bivario
#> 2      brewer                                ColorBrewer palettes
#> 3       carto                          Palettes designed by CARTO
#> 4    cols4all                  cols4all palettes (in development)
#> 5         gmt                 Palettes from General Mapping Tools
#> 6         hcl  Palettes from the Hue Chroma Luminance color space
#> 7      kovesi                   Palettes designed by Peter Kovesi
#> 8  matplotlib         Palettes from the Python library matplotlib
#> 9         met Palettes inspired by The Metropolitan Museum of Art
#> 10      meteo           Palettes by Oliver Fuhrer from MeteoSwiss
#> 11       misc                              Miscellaneous palettes
#> 12      ocean          Colormaps for Oceanography by Thyng et al.
#> 13      parks                 Palettes inspired by National Parks
#> 14  pinkfloyd     Palettes extracted from Pink Floyd album covers
#> 15       poly               Qualitative palettes with many colors
#> 16    powerbi                    Palettes from Microsoft Power BI
#> 17      scico             Scientific colour maps by Fabio Crameri
#> 18    seaborn            Palettes from the Python library Seaborn
#> 19    stevens                Bivariate palettes by Joshua Stevens
#> 20    tableau                        Palettes designed by Tableau
#> 21        tol                       Palettes designed by Paul Tol
#> 22        wes                   Palettes from Wes Anderson movies

c4a_types()
#>   type                          description
#> 1  cat                          categorical
#> 2  seq                           sequential
#> 3  div                            diverging
#> 4  cyc                               cyclic
#> 5 bivs  bivariate (sequential x sequential)
#> 6 bivc bivariate (sequential x categorical)
#> 7 bivd   bivariate (sequential x diverging)
#> 8 bivg bivariate (sequential x desaturated)

c4a_overview()
#>        series                                         description cat seq div
#> 1     bivario            Palettes from the Python library bivario   0   0   0
#> 2      brewer                                ColorBrewer palettes   9  18   9
#> 3       carto                          Palettes designed by CARTO   6  21   7
#> 4    cols4all                  cols4all palettes (in development)  14   0   2
#> 5         gmt                 Palettes from General Mapping Tools   0  19   4
#> 6         hcl  Palettes from the Hue Chroma Luminance color space   9  23  11
#> 7      kovesi                   Palettes designed by Peter Kovesi   0  28  14
#> 8  matplotlib         Palettes from the Python library matplotlib   0  51  12
#> 9         met Palettes inspired by The Metropolitan Museum of Art  33   8  14
#> 10      meteo           Palettes by Oliver Fuhrer from MeteoSwiss   2  14   7
#> 11       misc                              Miscellaneous palettes   5   0   0
#> 12      ocean          Colormaps for Oceanography by Thyng et al.   0  15   3
#> 13      parks                 Palettes inspired by National Parks  22   5   3
#> 14  pinkfloyd     Palettes extracted from Pink Floyd album covers  16   0   0
#> 15       poly               Qualitative palettes with many colors   9   0   0
#> 16    powerbi                    Palettes from Microsoft Power BI  19   1   4
#> 17      scico             Scientific colour maps by Fabio Crameri  21  21  10
#> 18    seaborn            Palettes from the Python library Seaborn   6   4   2
#> 19    stevens                Bivariate palettes by Joshua Stevens   0   5   0
#> 20    tableau                        Palettes designed by Tableau  29  23  28
#> 21        tol                       Palettes designed by Paul Tol   8   8   4
#> 22        wes                   Palettes from Wes Anderson movies  23   0   1
#>    cyc bivs bivc bivd bivg
#> 1    0   25    0    0    0
#> 2    0    2    2    1    0
#> 3    0    0    0    0    0
#> 4    0    2    0    2    5
#> 5    0    0    0    0    0
#> 6    6    0    0    0    0
#> 7    8    0    0    0    0
#> 8    3    0    0    0    0
#> 9    0    0    1    0    0
#> 10   0    0    0    0    0
#> 11   0    0    3    0    0
#> 12   0    0    0    0    0
#> 13   0    0    0    0    0
#> 14   0    0    0    0    0
#> 15   0    0    0    0    0
#> 16   0    0    0    0    0
#> 17   5    0    2    0    1
#> 18   0    0    0    0    0
#> 19   0    5    0    0    0
#> 20   0    0    3    0    0
#> 21   0    0    0    0    0
#> 22   0    0    0    0    0

c4a_palettes(type = "cat", series = "tol")
#> [1] "tol.bright"   "tol.contrast" "tol.vibrant"  "tol.muted"    "tol.medium"  
#> [6] "tol.light"    "tol.dark"     "tol.rainbow" 

c4a_palettes(type = "seq", series = "kovesi")
#>  [1] "kovesi.cy_or"                   "kovesi.isoluminant_cgo_80_c38" 
#>  [3] "kovesi.isoluminant_cm_70_c39"   "kovesi.bu_gn_yl_wh"            
#>  [5] "kovesi.bu_gn_yl"                "kovesi.linear_bgyw_15_100_c68" 
#>  [7] "kovesi.blue_cyan"               "kovesi.blue"                   
#>  [9] "kovesi.bu_wh_mg"                "kovesi.linear_bmw_5_95_c89"    
#> [11] "kovesi.bu_yl_mg"                "kovesi.linear_bmy_10_95_c78"   
#> [13] "kovesi.linear_gow_60_85_c27"    "kovesi.linear_gow_65_90_c35"   
#> [15] "kovesi.green"                   "kovesi.linear_grey_0_100_c0"   
#> [17] "kovesi.grey"                    "kovesi.bk_rd_yl"               
#> [19] "kovesi.linear_kry_5_98_c75"     "kovesi.bk_rd_wh"               
#> [21] "kovesi.linear_kryw_5_100_c67"   "kovesi.ternary_blue"           
#> [23] "kovesi.ternary_green"           "kovesi.ternary_red"            
#> [25] "kovesi.rainbow_bu_gn_yl_rd"     "kovesi.rainbow_bgyr_35_85_c73" 
#> [27] "kovesi.rainbow_bu_gn_yl_rd_mg"  "kovesi.rainbow_bgyrm_35_85_c71"

# handy when auto-completion is available:
.P$kovesi$seq$linear_terrain
#> NULL
```
