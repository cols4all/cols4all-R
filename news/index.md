# Changelog

## cols4all (development version)

- c4a_gui() opens the cols4all website, <https://cols4all.org>, which
  also shows and scores own palettes loaded with c4a_load(); see
  ?c4a_gui.
- the shiny app is still available with c4a_gui(local = TRUE), but will
  be removed in a future version.

## cols4all 0.11

- Scientific colour maps (series “scico”) updated to version 9.0
- palette “tableau.red_blue_brown” renamed to “tableau.blue_red_brown”
  (following ggthemes); the old name still works
- separability of colors is now expressed as the percentage of people
  that can tell two lines or points apart, based on Szafir (2018); see
  palette_prob_bg()
- second tab of c4a_gui() renamed to “Color Discrimination”
- c4a_gui() and c4a_table() can simulate a warmer display color
  temperature, e.g. night mode (approximation)
- c4a_table() and c4a_scores() have an argument include_tritan
- c4a_gui() and c4a_table() explain why no palettes are found and what
  to change
- c4a_gui() is faster and keeps the selected palette in sync across tabs

## cols4all 0.10

CRAN release: 2025-10-27

- palettes from bivario added
- black and white option added

## cols4all 0.9-1

- several “meteo” palettes now classified as “div” (instead of “seq”)

## cols4all 0.9

CRAN release: 2025-08-28

- palettes added: “stevens” sequential
- palette series added: “gmt”, “meteo”, and “ocean”
- added “\|”, “+”, “//”, and “\\ prefixes to reverse bivariate palettes

## cols4all 0.8

CRAN release: 2024-10-16

- main table much faster
- sorting and filtering options added
- powerbi and matplotlib palettes added
- \[!\] added new “cols4all” categorical palettes
- analysis tabs updated

## cols4all 0.7-1

CRAN release: 2024-03-12

- fixed critical bug in c4a_gui

## cols4all 0.7

CRAN release: 2024-03-06

- improved similarity matrix plot
- improved applications tab
- added naming column (in development)
- added new ‘tol’ palettes
- scores shown with more precision
- compatibility with tmap 4.0

## cols4all 0.6-1

- fixed bug confusion lines

## cols4all 0.6

CRAN release: 2023-02-02

- first CRAN release
