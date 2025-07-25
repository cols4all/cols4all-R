% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/c4a_palettes.R
\docType{data}
\name{c4a_palettes}
\alias{c4a_palettes}
\alias{c4a_series}
\alias{c4a_types}
\alias{c4a_overview}
\alias{.P}
\title{Get available palette names and series}
\format{
An object of class \code{environment} of length 20.
}
\usage{
c4a_palettes(
  type = c("all", "cat", "seq", "div", "cyc", "bivs", "bivc", "bivd", "bivg"),
  series = NULL,
  full.names = TRUE
)

c4a_series(type = c("all", "cat", "seq", "div", "cyc"), as.data.frame = TRUE)

c4a_types(series = NULL, as.data.frame = TRUE)

c4a_overview(return.matrix = FALSE, zero.count.as.NA = FALSE)

.P
}
\arguments{
\item{type}{type of color palette: one of \code{"all"} (all palettes), \code{"cat"}, \code{"seq"}, \code{"div"}, \code{"cyc"}, \code{"bivs"}, \code{"bivc"}, \code{"bivd"}, or \code{"bivg"}. See \code{\link{c4a_types}} for descriptions.}

\item{series}{series to list the palettes from. Run \code{c4a_series} to see the options.}

\item{full.names}{should full names, i.e. with the prefix "series."? By default \code{TRUE}.}

\item{as.data.frame}{should \code{c4a_series} and \code{c4a_types} return the result as a data.frame, with description included as a column?}

\item{return.matrix}{should only a matrix be returned with numbers per palette and type? If \code{FALSE} a data.frame is returned with addional information}

\item{zero.count.as.NA}{should zeros counted in the table be returned as 0 (\code{FALSE}, default) or as \code{NA} (\code{TRUE})?}
}
\value{
names of the loaded color palettes
}
\description{
\code{c4a_palettes} lists all available cols4all color palettes. Palettes are organized by series. The available series are listed with \code{c4a_series}. Palettes are also organized per functional type, where we currently support: categorical \code{"cat"}, sequential \code{"seq"}, diverging \code{"div"}", cyclic \code{"cyc"}, and bivariate (seq x seq \code{"bivs"}, seq x cat \code{"bivc"}, seq x div \code{"bivd"}, seq x desaturated \code{"bivg"}) palette types. The function \code{c4a_types} lists all available types. The function \code{c4a_overview} gives an overview table of the number of palette per series and type. In an IDE with auto-completion (such as RStudio) it is possible to browse through the palette names with \code{.P} (using \code{$} like in lists).
}
\examples{
c4a_series()

c4a_types()

c4a_overview()

c4a_palettes(type = "cat", series = "tol")

c4a_palettes(type = "seq", series = "kovesi")

# handy when auto-completion is available:
.P$kovesi$seq$linear_terrain
}
\seealso{
References of the palettes: \code{\link{cols4all-package}}.
}
\keyword{datasets}
