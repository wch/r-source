\name{prompt}
\title{Produce prototype of a help()-file}
\usage{
prompt(object, filename=)

prompt.default(object,
    filename = paste(name, ".man", sep = ""),
    force.function = FALSE)
}
\alias{prompt}
\alias{prompt.default}
\arguments{
\item{object}{(typically) an R function}
\item{filename}{name of the output file}
\item{force.function}{treat \code{object} as function in any case}
}
\description{
The main goal is to facilitate the constructing of  \code{help()} files for
function documentation.
An ASCII file \code{filename} is produced containing the proper function and
argument names of \code{object}. You have to edit it before
adding the documentation to the source tree, i.e. (currently) to
\code{$RHOME/src/library/base/man/}.
}
\seealso{
\code{\link{help}} and the documentation about writing help-files, currently
in the file \code{$RHOME/mansrc/ABOUT.tex}.
}
\examples{
prompt(plot.default)
prompt(interactive, force.f=TRUE)
}
