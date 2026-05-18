# Get information from a cols4all palette

Get information from a cols4all palette

## Usage

``` r
c4a_scores(
  palette = NULL,
  type = NULL,
  series = NULL,
  n = NA,
  no.match = c("message", "error", "null"),
  verbose = TRUE
)
```

## Arguments

- palette:

  name of the palette

- type:

  type of palettes (in case palette is not specified)

- series:

  series name (in case palette is not specified)

- n:

  number of colors

- no.match:

  what happens is no match is found? Options: `"message"`: a message is
  thrown with suggestions, `"error"`: an error is thrown, `"null"`:
  `NULL` is returned

- verbose:

  should messages be printed?

## Value

list with the following items: name, series, fullname, type, palette
(colors), na (color), nmax, and reverse. The latter is `TRUE` when there
is a `"-"` prefix before the palette name.

## Examples

``` r
c4a_scores("blues3")
#>     name series   fullname type n min_dist nameability min_step max_step
#> 1 blues3    hcl hcl.blues3  seq 7     7.17          NA     7.17    12.64
#>   inter_wing_dist tri_ineq Cmax   H  HL  HR Lmid Hwidth HwidthL HwidthR Lrange
#> 1              NA     6.79   70 246 248 244   68     11       7       6     67
#>   Crange fairness CRmin CRwt CRbk Blues cbfriendly chroma Hspread fair hues
#> 1     60       81  1.18 1.12 2.15  1.83    1.00717      M       4    H   SH
#>   equiluminance contrastWT contrastBK float nameable
#> 1          TRUE       TRUE       TRUE FALSE       NA

pals = c4a_palettes(type = "cat")
scores_cat7 = t(sapply(pals, c4a_scores, n = 7))

head(scores_cat7)
#>               name      series   fullname        type  n min_dist nameability
#> misc.r3       "r3"      "misc"   "misc.r3"       "cat" 7 8.63     0          
#> misc.r4       "r4"      "misc"   "misc.r4"       "cat" 7 4.29     0          
#> misc.ggplot2  "ggplot2" "misc"   "misc.ggplot2"  "cat" 7 3.93     0          
#> misc.okabe    "okabe"   "misc"   "misc.okabe"    "cat" 7 11.13    0          
#> brewer.accent "accent"  "brewer" "brewer.accent" "cat" 7 2.06     0          
#> brewer.dark2  "dark2"   "brewer" "brewer.dark2"  "cat" 7 2.29     0          
#>               min_step max_step inter_wing_dist tri_ineq Cmax H   HL  HR  Lmid
#> misc.r3       NA       NA       NA              NA       179  360 17  17  91  
#> misc.r4       NA       NA       NA              NA       105  360 245 10  82  
#> misc.ggplot2  NA       NA       NA              NA       100  360 12  14  70  
#> misc.okabe    NA       NA       NA              NA       110  360 142 316 89  
#> brewer.accent NA       NA       NA              NA       132  360 26  320 98  
#> brewer.dark2  NA       NA       NA              NA       115  360 270 84  52  
#>               Hwidth HwidthL HwidthR Lrange Crange fairness CRmin CRwt CRbk
#> misc.r3       287    223     139     65     179    0        1.16  1.07 2.44
#> misc.r4       291    241     111     34     105    12       1     1.6  4.37
#> misc.ggplot2  296    238     118     6      100    20       1     2.27 7.71
#> misc.okabe    271    186     142     43     57     56       1.02  1.32 4.05
#> brewer.accent 237    206     137     53     102    8        1.04  1.05 3.94
#> brewer.dark2  248    232     64      23     63     73       1.03  2.06 4.72
#>               Blues cbfriendly chroma Hspread fair hues equiluminance
#> misc.r3       Inf   0.00863    "H"    93      "L"  "RH" TRUE         
#> misc.r4       1.52  0.00429    "H"    94      "L"  "RH" TRUE         
#> misc.ggplot2  1.63  0.00393    "H"    96      "L"  "RH" TRUE         
#> misc.okabe    1.56  1.01113    "H"    88      "M"  "NA" TRUE         
#> brewer.accent 1.63  0.00206    "H"    77      "L"  "NA" TRUE         
#> brewer.dark2  1.53  0.00229    "H"    80      "M"  "NA" TRUE         
#>               contrastWT contrastBK float nameable
#> misc.r3       TRUE       TRUE       TRUE  FALSE   
#> misc.r4       TRUE       FALSE      FALSE FALSE   
#> misc.ggplot2  TRUE       FALSE      FALSE FALSE   
#> misc.okabe    TRUE       FALSE      FALSE FALSE   
#> brewer.accent TRUE       FALSE      FALSE FALSE   
#> brewer.dark2  TRUE       FALSE      FALSE FALSE   
```
