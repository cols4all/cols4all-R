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
#>   inter_wing_dist tri_ineq min_dist_dp min_step_dp max_step_dp
#> 1              NA     6.79        7.17        7.17       13.35
#>   inter_wing_dist_dp tri_ineq_dp min_dist_none min_dist_deutan min_dist_protan
#> 1                 NA        6.79            NA              NA              NA
#>   min_dist_tritan Cmax   H  HL  HR Lmid Hwidth HwidthL HwidthR Lrange Crange
#> 1              NA   70 246 248 244   68     11       7       6     67     60
#>   fairness CRmin CRwt CRbk Blues cbfriendly cbf_none cbf_deutan cbf_protan
#> 1       81  1.18 1.12 2.15  1.83    1.00717       NA         NA         NA
#>   cbf_tritan min_dist_overall min_dist_overall_dp chroma Hspread fair hues
#> 1         NA               NA                  NA      M       4    H   SH
#>   equiluminance contrastWT contrastBK float nameable
#> 1          TRUE       TRUE       TRUE FALSE       NA

pals = c4a_palettes(type = "cat")
scores_cat7 = t(sapply(pals, c4a_scores, n = 7))

head(scores_cat7)
#>               name      series   fullname        type  n min_dist nameability
#> misc.r3       "r3"      "misc"   "misc.r3"       "cat" 7 NA       0          
#> misc.r4       "r4"      "misc"   "misc.r4"       "cat" 7 NA       0          
#> misc.ggplot2  "ggplot2" "misc"   "misc.ggplot2"  "cat" 7 NA       0          
#> misc.okabe    "okabe"   "misc"   "misc.okabe"    "cat" 7 NA       0          
#> brewer.accent "accent"  "brewer" "brewer.accent" "cat" 7 NA       0          
#> brewer.dark2  "dark2"   "brewer" "brewer.dark2"  "cat" 7 NA       0          
#>               min_step max_step inter_wing_dist tri_ineq min_dist_dp
#> misc.r3       NA       NA       NA              NA       NA         
#> misc.r4       NA       NA       NA              NA       NA         
#> misc.ggplot2  NA       NA       NA              NA       NA         
#> misc.okabe    NA       NA       NA              NA       NA         
#> brewer.accent NA       NA       NA              NA       NA         
#> brewer.dark2  NA       NA       NA              NA       NA         
#>               min_step_dp max_step_dp inter_wing_dist_dp tri_ineq_dp
#> misc.r3       NA          NA          NA                 NA         
#> misc.r4       NA          NA          NA                 NA         
#> misc.ggplot2  NA          NA          NA                 NA         
#> misc.okabe    NA          NA          NA                 NA         
#> brewer.accent NA          NA          NA                 NA         
#> brewer.dark2  NA          NA          NA                 NA         
#>               min_dist_none min_dist_deutan min_dist_protan min_dist_tritan
#> misc.r3       100           30              55              59             
#> misc.r4       100           46              42              47             
#> misc.ggplot2  100           34              15              28             
#> misc.okabe    75            46              55              43             
#> brewer.accent 100           41              8               70             
#> brewer.dark2  82            15              7               49             
#>               Cmax H   HL  HR  Lmid Hwidth HwidthL HwidthR Lrange Crange
#> misc.r3       179  360 17  17  91   287    223     139     65     179   
#> misc.r4       105  360 245 10  82   291    241     111     34     105   
#> misc.ggplot2  100  360 12  14  70   296    238     118     6      100   
#> misc.okabe    110  360 142 316 89   271    186     142     43     57    
#> brewer.accent 132  360 26  320 98   237    206     137     53     102   
#> brewer.dark2  115  360 270 84  52   248    232     64      23     63    
#>               fairness CRmin CRwt CRbk Blues cbfriendly cbf_none cbf_deutan
#> misc.r3       0        1.16  1.07 2.44 Inf   -0.97      2        -1        
#> misc.r4       12       1     1.6  4.37 1.52  -0.958     2        -1        
#> misc.ggplot2  20       1     2.27 7.71 1.63  -0.985     2        -1        
#> misc.okabe    56       1.02  1.32 4.05 1.56  -0.957     0        -1        
#> brewer.accent 8        1.04  1.05 3.94 1.63  -0.992     2        -1        
#> brewer.dark2  73       1.03  2.06 4.72 1.53  -0.993     0        -1        
#>               cbf_protan cbf_tritan min_dist_overall min_dist_overall_dp chroma
#> misc.r3       0          0          30               30                  "H"   
#> misc.r4       -1         -1         42               42                  "H"   
#> misc.ggplot2  -1         -1         15               15                  "H"   
#> misc.okabe    0          -1         43               46                  "H"   
#> brewer.accent -1         0          8                8                   "H"   
#> brewer.dark2  -1         -1         7                7                   "H"   
#>               Hspread fair hues equiluminance contrastWT contrastBK float
#> misc.r3       93      "L"  "RH" TRUE          TRUE       TRUE       TRUE 
#> misc.r4       94      "L"  "RH" TRUE          TRUE       FALSE      FALSE
#> misc.ggplot2  96      "L"  "RH" TRUE          TRUE       FALSE      FALSE
#> misc.okabe    88      "M"  "NA" TRUE          TRUE       FALSE      FALSE
#> brewer.accent 77      "L"  "NA" TRUE          TRUE       FALSE      FALSE
#> brewer.dark2  80      "M"  "NA" TRUE          TRUE       FALSE      FALSE
#>               nameable
#> misc.r3       FALSE   
#> misc.r4       FALSE   
#> misc.ggplot2  FALSE   
#> misc.okabe    FALSE   
#> brewer.accent FALSE   
#> brewer.dark2  FALSE   
```
