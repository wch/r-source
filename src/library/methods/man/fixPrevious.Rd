% File src/library/methods/man/fixPrevious.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{fixPre1.8}
\alias{fixPre1.8}
\title{Fix Objects Saved from R Versions Previous to 1.8}
\description{
  Beginning with R version 1.8.0, the class of an object contains the
  identification of the package in which the class is defined.  The
  function \code{fixPre1.8} fixes and re-assigns objects missing that information
  (typically because they were loaded from a file saved with a previous
  version of R.)
}
\usage{
fixPre1.8(names, where)
}
\arguments{
  \item{names}{ Character vector of the names of all the objects to be
    fixed and re-assigned.}
\item{where}{The environment from which to look for the objects, and
    for class definitions.  Defaults to the top environment of the
    call to \code{fixPre1.8}, the global environment if the function
    is used interactively.}
}
\details{
 The named object will be saved where it was found.  Its class
    attribute will be changed to the full form required by R 1.8;
    otherwise, the contents of the object should be unchanged.

Objects will be fixed and re-assigned only if all the following
conditions hold:
\enumerate{
\item The named object exists.
\item It is from a defined class (not a basic datatype which
    has no actual class attribute).
\item The object appears to be from an earlier version of R.
\item The class is currently defined.
\item The object is consistent with the current class definition.
}
If any condition except the second fails, a warning message is
generated.

Note that \code{fixPre1.8} currently fixes \emph{only} the change in
class attributes.  In particular, it will not fix binary versions of
packages installed with earlier versions of R if these use
incompatible features.  Such packages must be re-installed from
source, which is the wise approach always when major version changes
occur in R.

}
\value{
  The names of all the objects that were in fact re-assigned.
}
\keyword{classes}
\keyword{programming}
