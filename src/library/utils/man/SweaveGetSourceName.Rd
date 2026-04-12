% File src/library/utils/man/SweaveGetSourceName.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2026 R Core Team
% Distributed under GPL 2 or later

\name{SweaveGetSourceName}
\alias{SweaveGetSourceName}
\title{File Processed by Sweave or Stangle}
\description{
  Extract the name of the file currently processed by
  \code{\link[utils]{Sweave}} or \code{\link[utils]{Stangle}} in a
  non-interactive session.
}
\usage{
SweaveGetSourceName()
}
\details{
  The function returns the name of the source file being processed by
  \code{Sweave} or \code{Stangle} when the tool is invoked from the
  command line through either:
  \enumerate{
    \item an expression passed to \code{R -e} or \code{Rscript -e};
    \item \code{R CMD Sweave} or \code{R CMD Stangle}.
  }

  In the first scenario, the filename must be the first, unnamed
  argument to \code{Sweave} or \code{Stangle}.

  This function cannot be used from an interactive R session.
}
\value{
  Filename as a character string.
}
\author{
  Vincent Goulet
}
\examples{
nmRnw <- "example-4.Rnw"
exfile <- system.file("Sweave", nmRnw, package = "utils")

## Check the contents of the file
if (interactive()) file.show(exfile)

## Sweave the file in a separate process
expr <- paste0("Sweave('", exfile, "', ",
               "encoding = 'utf-8')")
system2("R", c("-e", paste0("\"", expr, "\"")))

## View the LaTeX file
nmtex <- sub("Rnw$", "tex", nmRnw)
if (interactive()) file.show(nmtex)

## Stangle was called from within Sweave in the file
nmR <- sub("Rnw$", "R", nmRnw) # default R output file name
if (interactive()) file.show(nmR)

## Repeat using 'R CMD Sweave'
system2("R", c("CMD", "Sweave", exfile, "--encoding=utf-8"))
if (interactive()) file.show(nmtex)
if (interactive()) file.show(nmR)

## Do not use the function interactively
\dontrun{SweaveGetSourceName()}
}
\keyword{utilities}
