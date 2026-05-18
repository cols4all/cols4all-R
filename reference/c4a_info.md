# Get information from a cols4all palette

Get information from a cols4all palette

## Usage

``` r
c4a_info(palette, no.match = c("message", "error", "null"), verbose = TRUE)
```

## Arguments

- palette:

  name of the palette

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
