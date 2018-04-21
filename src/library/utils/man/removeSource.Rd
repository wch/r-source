% File src/library/utils/man/removeSource.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2014-2018 R Core Team
% Distributed under GPL 2 or later

\name{removeSource}
\alias{removeSource}
\title{Remove Stored Source from a Function}
\description{
  When \code{options("keep.source")} is \code{TRUE}, a copy of the
  original source code to a function is stored with it.  This function
  removes that copy.
}
\usage{
removeSource(fn)
}
\arguments{
  \item{fn}{
    A \code{\link{function}} or another language object
    (fulfilling \code{\link{is.language}} or \code{\link{is.expression}})
    from which to remove the source.
  }
}
\details{
  This removes the \code{"srcref"} and related attributes.
}
\value{
  A copy of the \code{fn} object with the source removed.
}
\seealso{
  \code{\link{srcref}} for a description of source reference records,
  \code{\link{deparse}} for a description of how functions are deparsed.
}
\examples{
## to make this act independently of the global 'options()' setting:
op <- options(keep.source = TRUE)
fn <- function(x) {
  x + 1 # A comment, kept as part of the source
}
fn
names(attributes(fn))       # "srcref" (only)
names(attributes(body(fn))) # "srcref" "srcfile" "wholeSrcref"
f2 <- removeSource(fn)
f2
stopifnot(length(attributes(fn)) > 0,
          is.null(attributes(f2)),
          is.null(attributes(body(f2))))

## Source attribute of parse()d expressions: have {"srcref", "srcfile", "wholeSrcref"}
E  <- parse(text ="a <- x^y  # power")  ; names(attributes(E ))
E. <- removeSource(E)                   ; names(attributes(E.))
stopifnot(length(attributes(E ))  > 0,
          is.null(attributes(E.)))
options(op) # reset to previous state
}
\keyword{ utility }
