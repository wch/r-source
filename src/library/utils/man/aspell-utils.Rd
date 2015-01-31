% File src/library/utils/man/aspell-utils.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{aspell-utils}
\alias{aspell-utils}
\alias{aspell_package_Rd_files}
\alias{aspell_package_vignettes}
\alias{aspell_package_R_files}
\alias{aspell_package_C_files}
\alias{aspell_write_personal_dictionary_file}
\title{Spell Check Utilities}
\description{
  Utilities for spell checking packages via Aspell, Hunspell or Ispell.
}
\usage{
aspell_package_Rd_files(dir, drop = c("\\\\author", "\\\\references"),
                        control = list(), program = NULL,
                        dictionaries = character())
aspell_package_vignettes(dir,
                         control = list(), program = NULL,
                         dictionaries = character())
aspell_package_R_files(dir, ignore = character(), control = list(),
                       program = NULL, dictionaries = character())
aspell_package_C_files(dir, ignore = character(), control = list(),
                       program = NULL, dictionaries = character())

aspell_write_personal_dictionary_file(x, out, language = "en",
                                      program = NULL)
}
\arguments{
  \item{dir}{a character string specifying the path to a package's root
    directory.}
  \item{drop}{a character vector naming additional Rd sections to drop
    when selecting text via \code{\link[tools]{RdTextFilter}}.}
  \item{control}{a list or character vector of control options for
    the spell checker.}
  \item{program}{a character string giving the name (if on the system
    path) or full path of the spell check program to be used, or
    \code{NULL} (default).  By default, the system path is searched for
    \command{aspell}, \command{hunspell} and \command{ispell} (in that
    order), and the first one found is used.}
  \item{dictionaries}{a character vector of names or file paths of
    additional R level dictionaries to use.  See \code{\link{aspell}}.}
  \item{ignore}{a character vector with regular expressions to be
    replaced by blanks when filtering the message strings.}
  \item{x}{a character vector, or the result of a call to
    \code{\link{aspell}()}.}
  \item{out}{a character string naming the personal dictionary file to
    write to.}
  \item{language}{a character string indicating a language as used by
    Aspell.}
}
\details{
  Functions \code{aspell_package_Rd_files},
  \code{aspell_package_vignettes}, \code{aspell_package_R_files} and
  \code{aspell_package_C_files} perform spell checking on the Rd files,
  vignettes, R files, and C-level messages of the package with root
  directory \code{dir}.  They determine the respective files, apply the
  appropriate filters, and run the spell checker.

  See \code{\link{aspell}} for details on filters.

  The C-level message string are obtained from the
  \file{po/\var{PACKAGE}.pot} message catalog file, with \var{PACKAGE}
  the basename of \code{dir}.  See the section on \dQuote{C-level
  messages} in \dQuote{Writing \R Extensions} for more information.

  When using Aspell, the vignette checking skips parameters and/or
  options of commands \verb{\Sexpr}, \verb{\citep}, \verb{\code},
  \verb{\pkg}, \verb{\proglang} and \verb{\samp}.  Further commands can
  be skipped by adding \verb{--add-tex-command} options to the
  \code{control} argument.  E.g., to skip both option and parameter of
  \verb{\mycmd}, add \verb{--add-tex-command='mycmd op'}.

  Suitable values for \code{control}, \code{program},
  \code{dictionaries}, \code{drop} and \code{ignore} can also be
  specified using a package defaults file which should go as
  \file{defaults.R} into the \file{.aspell} subdirectory of \code{dir},
  and provides defaults via assignments of suitable named lists, e.g.,
\preformatted{vignettes <- list(control = "--add-tex-command='mycmd op'")}
  for vignettes (when using Aspell) and similarly assigning to
  \code{Rd_files}, \code{R_files} and \code{C_files} for Rd files, R
  files and C level message defaults.

  Maintainers of packages using both English and American spelling will
  find it convenient to pass control options \option{--master=en_US} and
  \option{--add-extra-dicts=en_GB} to Aspell and control options
  \option{-d en_US,en_GB} to Hunspell (provided that the corresponding
  dictionaries are installed).

  Older versions of \R had no support for R level dictionaries, and
  hence provided the function
  \code{aspell_write_personal_dictionary_file} to create (spell check)
  program-specific personal dictionary files from words to be accepted.
  The new mechanism is to use R level dictionaries, i.e., \file{.rds}
  files obtained by serializing character vectors of such words using
  \code{\link{saveRDS}}.  For such dictionaries specified via the
  package defaults mechanism, elements with no path separator can be R
  system dictionaries or dictionaries in the \file{.aspell}
  subdirectory.
}
\seealso{
  \code{\link{aspell}}
}
\keyword{utilities}
