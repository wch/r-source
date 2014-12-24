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
       program = NULL, dictionaries = character())
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
  \item{dictionaries}{a character vector of names or file paths of
    additional R level dictionaries to use.  Elements with no path
    separator specify R system dictionaries (in subdirectory
    \file{share/dictionaries} of the R home directory).  The file
    extension (currently, only \file{.rds}) can be omitted.}
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

  The currently available built-in filters are \code{"Rd"}
  (corresponding to \code{\link{RdTextFilter}}), \code{"Sweave"}
  (corresponding to \code{\link{SweaveTeXFilter}}), \code{"R"},
  \code{"pot"} and \code{"dcf"}.

  Filter \code{"R"} is for R code and extracts the message string
  constants in calls to \code{\link{message}}, \code{\link{warning}},
  \code{\link{stop}}, \code{\link{packageStartupMessage}},
  \code{\link{gettext}}, \code{\link{gettextf}}, and
  \code{\link{ngettext}} (the unnamed string constants for the first
  five, and \code{fmt} and \code{msg1}/\code{msg2} string constants,
  respectively, for the latter two).
  Filter \code{"pot"} is for message string catalog \file{.pot} files.
  Both have an argument \code{ignore} allowing to give regular
  expressions for parts of message strings to be ignored for spell
  checking: e.g., using \code{"[ \t]'[^']*'[ \t[:punct:]]"} ignores all
  text inside single quotes.

  Filter \code{"dcf"} is for files in Debian Control File format.
  The fields to keep can be controlled by argument \code{keep} (a
  character vector with the respective field names).  By default,
  \samp{Title} and \samp{Description} fields are kept.

  The print method has for the objects returned by \code{aspell} has an
  \code{indent} argument controlling the indentation of the positions of
  possibly mis-spelled words.  The default is 2; Emacs users may find it
  useful to use an indentation of 0 and visit output in grep-mode.  It
  also has a \code{verbose} argument: when this is true, suggestions for
  replacements are shown as well.

  It is possible to employ additional R level dictionaries.  Currently,
  these are files with extension \file{.rds} obtained by serializing
  character vectors of word lists using \code{\link{saveRDS}}.  If such
  dictionaries are employed, they are combined into a single word list
  file which is then used as the spell checker's personal dictionary
  (option \option{-p}): hence, the default personal dictionary is not
  used in this case.
}
\value{
  A data frame inheriting from \code{aspell} (which has a useful print
  method) with the information about possibly mis-spelled words.
}
\references{
  Kurt Hornik and Duncan Murdoch (2011),
  Watch your spelling!
  \emph{The R Journal} \bold{3}(2), 22--28.
  \url{http://journal.R-project.org/archive/2011-2/RJournal_2011-2_Hornik+Murdoch.pdf}.
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
aspell(files, filter = list("Rd", drop = "\\\\references"))

## To check all Sweave files
files <- Sys.glob(c("*.Rnw", "*.Snw", "*.rnw", "*.snw"))
aspell(files, filter = "Sweave", control = "-t")

## To check all Texinfo files (Aspell only)
files <- Sys.glob("*.texi")
aspell(files, control = "--mode=texinfo")
}

## List the available R system dictionaries.
Sys.glob(file.path(R.home("share"), "dictionaries", "*.rds"))
}
\keyword{utilities}
