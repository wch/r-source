% File src/library/utils/man/aspell.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{aspell}
\alias{aspell}
\title{Spell Check Interface}
\description{
  Spell check given files via Aspell, Hunspell or Ispell.
}
\usage{
aspell(files, filter, control = list(), encoding = "unknown",
       program = NULL)
}
\arguments{
  \item{files}{a character vector with the names of files to be checked.}
  \item{filter}{an optional filter for processing the files before spell
    checking, given as either a function (with formals \code{ifile} and
    \code{encoding}), or a character string specifying a built-in
    filter, or a list with the name of a built-in filter and additional
    arguments to be passed to it.  See \bold{Details} for available
    filters.  If missing or \code{NULL}, no filtering is performed.}
  \item{control}{a list or character vector of control options for the
    spell checker.}
  \item{encoding}{the encoding of the files.  Recycled as needed.}
  \item{program}{a character string giving the name (if on the system
    path) or full path of the spell check program to be used, or
    \code{NULL} (default).  By default, the system path is searched for
    \command{aspell}, \command{hunspell} and \command{ispell} (in that
    order), and the first one found is used.}
}
\details{
  The spell check programs employed must support the so-called Ispell
  pipe interface activated via command line option \option{-a}.  In
  addition to the programs, suitable dictionaries need to be available.
  See \url{http://aspell.net},
  \url{http://hunspell.sourceforge.net/} and
  \url{http://lasr.cs.ucla.edu/geoff/ispell.html}, respectively, for
  obtaining the Aspell, Hunspell and (International) Ispell programs and
  dictionaries.
  
  Currently the only available built-in filters are \code{"Rd"},
  corresponding to \code{\link{RdTextFilter}}, and \code{"Sweave"},
  corresponding to \code{\link{SweaveTeXFilter}}.

  The print method has for the objects returned by \code{aspell} has an
  \code{indent} argument controlling the indentation of the positions of
  possibly mis-spelled words.  The default is 2; Emacs users may find it
  useful to use an indentation of 0 and visit output in grep-mode.  It
  also has a \code{verbose} argument: when this is true, suggestions for
  replacements are shown as well.
}
\value{
  A data frame inheriting from \code{aspell} (which has a useful print
  method) with the information about possibly mis-spelled words.
}
\seealso{
  \link{aspell-utils} for utilities for spell checking packages.
  
  Package \pkg{Aspell} on Omegahat
  (\url{http://www.omegahat.org/Aspell}) for a fine-grained R interface
  to the Aspell library.
}
\examples{
\dontrun{
## To check all Rd files in a directory, (additonally) skipping the
## \references sections.
files <- Sys.glob("*.Rd")
aspell(files, filter = list("Rd", drop = "\\references"))

# To check all Sweave files
files <- Sys.glob(c("*.Rnw", "*.Snw", "*.rnw", "*.snw"))
aspell(files, filter = "Sweave", control = "-t")

# To check all Texinfo files (Aspell only)
files <- Sys.glob("*.texi")
aspell(files, control = "--mode=texinfo")
}
}
\keyword{utilities}
