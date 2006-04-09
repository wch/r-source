\name{deparseOpts}
\alias{.deparseOpts}
\title{Options for Expression Deparsing}
\description{
  Process the deparsing options for \code{deparse}, \code{dput} and
  \code{dump}.
}
\usage{
.deparseOpts(control)
}
\arguments{
  \item{control}{character vector of deparsing options.}
}
\details{
  This is called by \code{\link{deparse}}, \code{\link{dput}} and
  \code{\link{dump}} to process their \code{control} argument.

  The \code{control} argument is a vector containing zero or more of the
  following strings.  Partial string matching is used.
  \describe{
    \item{\code{keepInteger}}{
      Surround integer vectors by \code{as.integer()}, so they are not
      converted to floating point when re-parsed.
    }
    \item{\code{quoteExpressions}}{
      Surround expressions with \code{quote()}, so they are not
      evaluated when re-parsed.
    }
    \item{\code{showAttributes}}{
      If the object has attributes (other than a \code{source}
      attribute), use \code{structure()} to display them as well as the
      object value.  This is the default for \code{\link{deparse}} and
      \code{\link{dput}}.
    }
    \item{\code{useSource}}{
      If the object has a \code{source} attribute, display that instead
      of deparsing the object.  Currently only applies to function
      definitions.
    }
    \item{\code{warnIncomplete}}{
      Some exotic objects such as \link{environment}s,
      external pointers, etc. can not be deparsed properly.  This option
      causes a warning to be issued if any of those may give problems.
    }
    \item{\code{all}}{
      An abbreviated way to specify all of the options
      listed above.  May not be used with other options.  This is the
      default for \code{dump}.
    }
    \item{\code{delayPromises}}{
      Deparse promises in the form <promise: expression> rather than evaluating 
      them.  The value and the environment of the promise will not be shown
      and the deparsed code cannot be sourced.
    }
  }
  For the most readable (but perhaps incomplete) display, use
  \code{control = NULL}. This displays the object's value, but not its
  attributes.  The default is to display the attributes as well, but
  not to use any of the other options to make the result parseable.

  Using \code{control = "all"} comes closest to making \code{deparse()}
  an inverse of \code{parse()}.  However, not all objects are
  deparseable even with this option.  A warning will be issued if the
  function recognizes that it is being asked to do the impossible.
}
\value{
  A numerical value corresponding to the options selected.
}
\keyword{programming}
