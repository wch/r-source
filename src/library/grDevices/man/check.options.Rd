\name{check.options}
\alias{check.options}
\title{Set Options with Consistency Checks}
\usage{
check.options(new, name.opt, reset = FALSE, assign.opt = FALSE,
              envir = .GlobalEnv, check.attributes = c("mode", "length"),
              override.check = FALSE)
}
\description{
  Utility function for setting options with some consistency checks.
  The \code{\link{attributes}} of the new settings in \code{new} are
  checked for consistency with the \emph{model} (often default)
  list in \code{name.opt}.
}
\arguments{
  \item{new}{a \emph{named} list}
  \item{name.opt}{character with the name of \R object containing the
    \dQuote{model} (default) list.}
  \item{reset}{logical; if \code{TRUE}, reset the options from
    \code{name.opt}.  If there is more than one \R object with name
    \code{name.opt}, remove the first one in the \code{\link{search}()} path.}
  \item{assign.opt}{logical; if \code{TRUE}, assign the \ldots}
  \item{envir}{the \code{\link{environment}} used for \code{\link{get}}
    and \code{\link{assign}}.}
  \item{check.attributes}{character containing the attributes which
    \code{check.options} should check.}
  \item{override.check}{logical vector of length \code{length(new)} (or 1
    which entails recycling).
    For those \code{new[i]} where \code{override.check[i] == TRUE}, the
    checks are overriden and the changes made anyway.}
}
%% Source in ../R/postscript.R
\value{
  A list of components with the same names as the one called
  \code{name.opt}.
  The values of the components are changed from the \code{new} list,
  as long as these pass the checks (when these are not overridden
  according to \code{override.check}).
}
\author{Martin Maechler}
\seealso{\code{\link{ps.options}} which uses \code{check.options}.}
% 
\examples{
(L1 <- list(a=1:3, b=pi, ch="CH"))
check.options(list(a=0:2), name.opt = "L1")
check.options(NULL, reset = TRUE, name.opt = "L1")
}
\keyword{utilities}
\keyword{programming}
