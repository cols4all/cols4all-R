# Set cols4all options

Get or set global options for c4a. Works similar as the base function
`options`

## Usage

``` r
c4a_options(...)
```

## Arguments

- ...:

  Use character values to retrieve options. To set options, either use
  named arguments (where the names refer to the options), a list that
  consists of those options.

## Value

A list of options

## Details

|  |  |
|----|----|
| **Option** | **Description** |
| defaults | Default palettes per type |
| CBF_th | Parameters that label a palette as color blind friendly |
| CBVF_th | Parameters that label a palette as very color blind friendly |
| CBU_th | Parameters that label a palette as color blind unfriendly |
| CrangeFair | Maximum chroma range for which a palette is considered harmonic |
| CrangeUnfair | Minimum chroma range for which a palette is considered disharmonic |
| LrangeFair | Maximum luminance range for which a palette is considered harmonic |
| LrangeUnfair | Minimum luminance range for which a palette is considered disharmonic |
| Cintense | Chroma of colors that are considered intense |
| Cpastel | Chroma of colors that are considered 'pastel' |
| HwidthDivRainbow | A diverging palette is labeled as 'rainbow hue' if HwidthL or HwidthR are at least `HwidthDivRainbow` |
| HwidthDivSingle | A diverging palette is labeled as 'single hue' if HwidthL and HwidthR are at most `HwidthDivSingle` |
| HwidthSeqRainbow | A sequential palette is labeled as 'rainbow hue' if Hwidth is at least `HwidthSeqRainbow` |
| HwidthSeqSingle | A sequential palette is labeled as 'single hue' if Hwidth is at most `HwidthSeqSingle` |
| naming_fun | Function that returns a distance matrix with the `naming_colors` (see examples) |
| naming_fun_args | List of arguments for `naming_fun` |
| naming_colors | Vector of prototype colors for the color names (see examples) |
| naming_softmax | List of parameters for the softmax function applied to the distance matrix |
| angle_convention | Degrees-of-visual-angle \<-\> pixel conversion used by [`visual_angle_to_px()`](https://cols4all.github.io/reference/visual_angle_to_px.md) / [`px_to_visual_angle()`](https://cols4all.github.io/reference/px_to_visual_angle.md): `"szafir"` (default) reproduces Szafir (2018)'s own in-text px-equivalents; `"standard"` is the textbook chord formula |

## Examples

``` r
# Example how to lower the color-blind friendly threshold
# for categorical palettes (so more smileys in the GUI!)
# CBF_th: one smiley
# CBVF_th: two smileys

# current table
if (FALSE) { # \dontrun{
c4a_table(n = 9, sort = "cbfriendly")

opts = c4a_options("CBF_th", "CBVF_th")
opts$CBF_th$cat["min_dist"] = 7
opts$CBVF_th$cat["min_dist"] = 10


old = c4a_options(opts)

# more smileys :-) :-)
c4a_table(n = 9, sort = "cbfriendly")

# set the old settings back
c4a_options(old)
} # }

# Example how to use own nameability function
#
# This function should:
# - have an argument "pal" (vector of colors)
# - optionally have other arguments
# - return a distance matrix of n rows (length of pal) and k columns (classes).
#   It shoud have columns names that correspond to the naming colors (see below).
naming_RGB = function(pal) {
  cols = colorspace::hex2RGB(pal)
  coords = cols@coords

  cls = apply(coords, MARGIN = 1, which.max)
  mx = apply(coords, MARGIN = 1, max)
  dominance = ((mx + 0.001) / (rowSums(coords) + 0.001))
  cls[dominance < 0.4] = 4L

  m = matrix(0, nrow = length(pal), ncol = 4,
        dimnames = list(NULL, c("Red", "Green", "Blue", "Other")))
  for (i in 1:nrow(m)) {
    m[i, cls[i]] = 1
  }

  -m
}

# testing this function...
naming_RGB(c4a("brewer.set1")) #fair enough
#>       Red Green Blue Other
#>  [1,]  -1     0    0     0
#>  [2,]   0     0   -1     0
#>  [3,]   0    -1    0     0
#>  [4,]   0     0   -1     0
#>  [5,]  -1     0    0     0
#>  [6,]  -1     0    0     0
#>  [7,]  -1     0    0     0
#>  [8,]  -1     0    0     0
#>  [9,]   0     0    0    -1

# This vector should contain the 'prototype' colors, and have names that correspond
# to the column names of the returned matrices by the function above.
names_RGB =
    c("Red" = "#FF0000",
      "Green" = "#00FF00",
      "Blue" = "#0000FF",
      "Other" = "#AAAAAA")

# Set the options (may take a while because if calculated the nameability scores)
if (FALSE) { # \dontrun{
c4a_options(naming_fun = naming_RGB,
      naming_fun_args = list(),
      naming_colors = names_RGB)
} # }
```
