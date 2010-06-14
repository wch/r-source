\name{toRd}
\alias{toRd}
\title{
Generic function to convert object to a fragment of Rd code.
}
\description{
Methods for this function render their associated classes as a fragment of Rd
code, which can then be rendered into text, HTML, or LaTeX.
}
\usage{
toRd(obj, ...)
}
\arguments{
  \item{obj}{
The object to be rendered.
}
  \item{\dots}{
Additional arguments used by methods.
}
}
\value{
Returns a character vector containing a fragment of Rd code that could
be parsed and rendered.
}
\keyword{ utilities }
\keyword{ documentation }
