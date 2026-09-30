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
    \item an expression passed to \command{R -e} or \command{Rscript -e};
    \item \command{R CMD Sweave} or \command{R CMD Stangle}.
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
file.show(exfile)

## Sweave the file in a separate process using 'R CMD Sweave'
tools::Rcmd(c("Sweave", "--encoding=UTF-8", shQuote(exfile)))

## View the LaTeX file, including the 'exfile' path
nmtex <- sub("Rnw$", "tex", nmRnw)
\dontdiff{file.show(nmtex)}

## Stangle was called from within Sweave in the file
nmR <- sub("Rnw$", "R", nmRnw) # default R output file name
\dontdiff{file.show(nmR)}

## Also supported but less convenient here: Sweave via 'Rscript -e'
Rscript <- file.path(R.home("bin"), "Rscript")
expr <- sprintf("Sweave('\%s', encoding = 'UTF-8')", exfile)
system2(Rscript, c("-e", shQuote(expr)))
\donttest{file.show(nmtex)}
\donttest{file.show(nmR)}

## Not supported: SweaveGetSourceName() called from tools::R()
try(tools::R(Sweave, list(exfile, encoding = "UTF-8")))

## Do not use the function interactively
\dontrun{SweaveGetSourceName()}

## Cleanup
unlink(c(nmtex, nmR))
}
\keyword{utilities}
