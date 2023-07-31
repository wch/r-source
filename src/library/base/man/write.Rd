% File src/library/base/man/write.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2023 R Core Team
% Distributed under GPL 2 or later

\name{write}
\title{Write Data to a File}
\alias{write}
\usage{
write(x, file = "data",
      ncolumns = if(is.character(x)) 1 else 5,
      append = FALSE, sep = " ")
}
\description{
  Write data \code{x} to a file or other \code{\link{connection}}.
  \cr
  As it simply calls \code{\link{cat()}}, less formatting happens than
  with \code{\link{print}()}ing.
  If \code{x} is a matrix you need to transpose it (and typically set
  \code{ncolumns}) to get the columns in \code{file} the same as those in
  the internal representation.

  Whereas atomic vectors (\code{\link{numeric}}, \code{\link{character}},
  etc, including matrices) are written plainly, i.e., without any names,
  less simple vector-like objects such as \code{"\link{factor}"},
  \code{"\link{Date}"}, or \code{"\link{POSIXt}"} may be
  \code{\link{format}}ted to character before writing.
}
\arguments{
  \item{x}{the data to be written out. }
  \item{file}{a \code{\link{connection}}, or a character string naming
    the file to write to.  If \code{""}, print to the standard output
    connection.

    When \code{\link{.Platform}$OS.type != "windows"}, and it
    is \code{"|cmd"}, the output is piped to the command given
    by \file{cmd}.
  }
  \item{ncolumns}{the number of columns to write the data in.}
  \item{append}{if \code{TRUE} the data \code{x} are appended to the
    connection.}
  \item{sep}{a string used to separate columns.  Using \code{sep = "\t"}
    gives tab delimited output; default is \code{" "}.}
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{write} is a wrapper for \code{\link{cat}}, which gives further
  details on the format used.

  \code{\link{write.table}} for matrix and data frame objects,
  \code{\link{writeLines}} for lines of text,
  and \code{\link{scan}} for reading data.

  \code{\link{saveRDS}} and \code{\link{save}} are often preferable (for
  writing any \R objects).
}
\examples{
# Demonstrate default ncolumns, writing to the console
write(month.abb,  "")  # 1 element  per line for "character"
write(stack.loss, "")  # 5 elements per line for "numeric"

# Build a file with sequential calls
fil <- tempfile("data")
write("# Model settings", fil)
write(month.abb, fil, ncolumns = 6, append = TRUE)
write("\n# Initial parameter values", fil, append = TRUE)
write(sqrt(stack.loss), fil, append = TRUE)
if(interactive()) file.show(fil)
unlink(fil) # tidy up
}
\keyword{file}
\keyword{connection}
