# Exploring the 2759 palettes from paletteer

## Introduction

The most extensive collection of color palettes in R is the package
**paletteer** by Emil <Hvitfeldt@paletteer>. Let’s load these palettes
into .

## Loading the paletteer palettes

The prepared system data can be loaded as follows:

``` r

paletteer = readRDS(gzcon(url("https://cols4all.github.io/paletteer.rds")))
# from paletteer version 1.6.0

c4a_sysdata_import(paletteer)
```

Note that this system data import will replace the default cols4all
palettes. This is convenient because almost all cols4all palettes are
also contained in **paletteer**. The only exceptions are the cols4all
palette series `"powerbi"` and `"cols4all"`.

An overview of the number of available palettes:

``` r

(m <- c4a_overview(return.matrix = TRUE))
```

There are in total `sum(m)` palettes, and by palette type:

``` r

colSums(m)
```

## Exploring the palettes

Now we can start the interactive tool:

``` r

c4a_gui()
```

First we will see all categorical palettes of length 7, sorted by name.
When we filter on color blind friendliness and sort by
[fairness](https://cols4all.github.io/articles/01_paper.html#fairness)
we get this table:

![Fair categorical palettes of 7 colors, filtered by
colorblind-friendliness](../reference/figures/paletteer_cbf.png)

Figure 1: Colorblind friendly categorical palettes of 7 colors, sorted
by fairness

As you can see, the top three palettes are identical, namely the palette
by Okabe and <Ito@okabe>, but from difference R package sources and
variations regarding the the eighth color (black or gray).

The other way round, when we filter on fair and sort by
colorblind-friendliness we get this table:

![Fair categorical palettes of 7 colors, filtered by
colorblind-friendliness](../reference/figures/paletteer_fair.png)

Figure 2: Fair categorical palettes of 7 colors, sorted by
colorblind-friendliness

Observe that there are no color palettes of length 7 that are both
color-blind friendly and fair. The good news is that cols4all contains a
few new preliminary palettes that meet both criteria. See [strategies
for palette
design](https://cols4all.github.io/articles/01_paper.html#strategies-for-palette-design).
