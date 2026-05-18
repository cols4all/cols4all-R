# cols4all overview

cols4all stands for: color palettes for all people, including those with
color vision deficiency. Popular color palette series, such as
ColorBrewer, have been organized by type and have been scored on several
properties such as color-blind-friendliness and fairness (i.e. do colors
stand out equally?). Own palettes can also be loaded and analysed.
Besides the common palette types (categorical, sequential, and
diverging) it also includes bivariate color palettes. ggplot2 scales are
included.

## Details

This page provides a brief overview of all package functions.

## Main functions

|  |  |
|----|----|
| [`c4a_gui`](https://cols4all.github.io/reference/c4a_gui.md) | Dashboard for analyzing the palettes |
| [`c4a`](https://cols4all.github.io/reference/c4a.md) | Get the colors from a palette ([`c4a_na`](https://cols4all.github.io/reference/c4a.md) for the associated color for missing values) |
| [`c4a_plot`](https://cols4all.github.io/reference/c4a_plot.md) | Plot a color palette |

## Palette names and properties

|  |  |
|----|----|
| [`c4a_palettes`](https://cols4all.github.io/reference/c4a_palettes.md) | Get available palette names |
| [`c4a_series`](https://cols4all.github.io/reference/c4a_palettes.md) | Get available series names |
| [`c4a_overview`](https://cols4all.github.io/reference/c4a_palettes.md) | Get an overview of palettes per series x type |
| [`c4a_citation`](https://cols4all.github.io/reference/c4a_citation.md) | Show how to cites palettes (with bibtex code) |
| [`c4a_info`](https://cols4all.github.io/reference/c4a_info.md) | Get information from a palette, such as type and maximum number of colors) |
| [`.P`](https://cols4all.github.io/reference/c4a_palettes.md) | Environment via which palette names can be browsed with auto-completion (using `$`) |

## Importing and exporting palettes

|  |  |
|----|----|
| [`c4a_data`](https://cols4all.github.io/reference/c4a_data.md) | Build color palette data |
| [`c4a_load`](https://cols4all.github.io/reference/c4a_data.md) | Load color palette data |
| [`c4a_sysdata_import`](https://cols4all.github.io/reference/c4a_sysdata_import.md) | Import system data |
| [`c4a_sysdata_export`](https://cols4all.github.io/reference/c4a_sysdata_import.md) | Export system data |

## See also

Useful links:

- <https://cols4all.github.io/cols4all-R/>

- <https://github.com/cols4all/cols4all-R>

- Report bugs at <https://github.com/cols4all/cols4all-R/issues>

## Author

**Maintainer**: Martijn Tennekes <mtennekes@gmail.com>

Other contributors:

- Marco Puts <mputs@acm.org> \[contributor\]

- Achim Zeileis <Achim.Zeileis@R-project.org> \[contributor\]

- Jakub Nowosad <nowosad.jakub@gmail.com> \[contributor\]

- Robin Lovelace <rob00x@gmail.com> \[contributor\]

- Helgasoft <contact@helgasoft.com> \[contributor\]

- Matthew Petroff <matthew@mpetroff.net> \[contributor\]

- Olivier Roy \[contributor\]
