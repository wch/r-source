% File src/library/grDevices/man/pdf.options.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{pdf.options}
\alias{pdf.options}
\title{Auxiliary Function to Set/View Defaults for Arguments of pdf}
\description{
  The auxiliary function \code{pdf.options} can be used to set or view
  (if called without arguments) the default values for some of the
  arguments to \code{\link{pdf}}.

  \code{pdf.options} needs to be called before calling \code{pdf},
  and the default values it sets can be overridden by supplying
  arguments to \code{pdf}.
  }
\usage{
pdf.options(\dots, reset = FALSE)
}
\arguments{
  \item{\dots}{arguments \code{width}, \code{height}, \code{onefile},
    \code{family}, \code{title}, \code{fonts}, \code{paper},
    \code{encoding}, \code{pointsize}, \code{bg}, \code{fg},
    \code{pagecentre}, \code{useDingbats}, \code{colormodel},
    \code{fillOddEven} and \code{compress} can be supplied.}
  \item{reset}{logical: should the defaults be reset to their
    \sQuote{factory-fresh} values?}
}
\details{
  If both \code{reset = TRUE} and \code{...} are supplied the defaults
  are first reset to the \sQuote{factory-fresh} values and then the new
  values are applied.
}
\value{
  A named list of all the defaults.  If any arguments are
  supplied the return values are the old values and the result has the
  visibility flag turned off.
}
\seealso{
  \code{\link{pdf}}, \code{\link{ps.options}}.
}
\examples{
pdf.options(bg = "pink")
utils::str(pdf.options())
pdf.options(reset = TRUE) # back to factory-fresh
}
\keyword{device}
