% File src/library/tools/man/read.00Index.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2010 R Core Team
% Distributed under GPL 2 or later

\name{read.00Index}
\alias{read.00Index}
\title{Read 00Index-style Files}
\description{
  Read item/description information from \file{00Index}-like files.  Such
  files are description lists rendered in tabular form, and currently
  used for the \file{INDEX} and \file{demo/00Index} files of add-on
  packages.
}
\usage{
read.00Index(file)
}
\arguments{
  \item{file}{the name of a file to read data values from.  If the
    specified file is \code{""}, then input is taken from the keyboard
    (in this case input can be terminated by a blank line).
    Alternatively, \code{file} can be a \code{\link{connection}}, which
    will be opened if necessary, and if so closed at the end of the
    function call.}
}
\value{
  A character matrix with 2 columns named \code{"Item"} and
  \code{"Description"} which hold the items and descriptions.
}
\seealso{
  \code{\link{formatDL}} for the inverse operation of creating a
  00Index-style file from items and their descriptions.
}
\keyword{file}
\keyword{connection}
