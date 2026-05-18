# Show how to cite palettes

Show how to cite palettes

## Usage

``` r
c4a_citation(name, verbose = TRUE)
```

## Arguments

- name:

  name of a palette or series

- verbose:

  should text be printed (if FALSE only a
  [`utils::bibentry`](https://rdrr.io/r/utils/bibentry.html) object is
  returned)

## Value

[`utils::bibentry`](https://rdrr.io/r/utils/bibentry.html) object

## Examples

``` r
c4a_citation("hcl")
#> 
#> To cite palettes from series "hcl" in publications use:
#> 
#> Zeileis A, Hornik K, Murrell P (2009). “Escaping RGBland: Selecting
#> Colors for Statistical Graphics.” _Computational Statistics & Data
#> Analysis_, *53*(9), 3259-3270. doi:10.1016/j.csda.2008.11.033
#> <https://doi.org/10.1016/j.csda.2008.11.033>.
#> 
#> @Article{hcl,
#>   title = {Escaping {RGB}land: Selecting Colors for Statistical Graphics},
#>   author = {Achim Zeileis and Kurt Hornik and Paul Murrell},
#>   journal = {Computational Statistics \& Data Analysis},
#>   year = {2009},
#>   volume = {53},
#>   number = {9},
#>   pages = {3259--3270},
#>   doi = {10.1016/j.csda.2008.11.033},
#> }

c4a_citation("poly.glasbey")
#> 
#> To cite palette glasbey from series "poly" in publications use:
#> 
#> Glasbey C, van der Heijden G, Toh VFK, Gray A (2007). “Colour displays
#> for categorical images.” _Color Research & Application_, *32*(4),
#> 304-309. doi:10.1002/col.20327 <https://doi.org/10.1002/col.20327>.
#> <https://onlinelibrary.wiley.com/doi/abs/10.1002/col.20327>.
#> 
#> @Article{poly.glasbey,
#>   author = {Chris Glasbey and Gerie {van der Heijden} and Vivian F. K. Toh and Alision Gray},
#>   title = {Colour displays for categorical images},
#>   journal = {Color Research \& Application},
#>   volume = {32},
#>   number = {4},
#>   pages = {304-309},
#>   keywords = {pseudo-colour, look-up table, perceptually uniform colour space, RGB, CIELAB, colour quantization},
#>   doi = {https://doi.org/10.1002/col.20327},
#>   url = {https://onlinelibrary.wiley.com/doi/abs/10.1002/col.20327},
#>   year = {2007},
#> }
```
