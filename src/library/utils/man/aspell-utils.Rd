% File src/library/utils/man/aspell-utils.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{aspell-utils}
\alias{aspell-utils}
\alias{aspell_package_Rd_files}
\alias{aspell_package_vignettes}
\alias{aspell_write_personal_dictionary_file}
\title{Spell Check Utilities}
\description{
  Utilities for spell checking packages via Aspell, Hunspell or Ispell.
}
\usage{
aspell_package_Rd_files(dir, drop = c("\\\\author", "\\\\references"),
                        control = list(), program = NULL)
aspell_package_vignettes(dir, control = list(), program = NULL)
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
  \item{x}{a character vector, or the result of a call to
    \code{\link{aspell}()}.}
  \item{out}{a character string naming the personal dictionary file to
    write to.}
  \item{language}{a character string indicating a language as used by
    Aspell.}
}
\details{
  \code{aspell_package_Rd_files} and \code{aspell_package_vignettes}
  perform spell checking on the Rd files and vignettes of the package
  with root directory \code{dir}.  They determine the respective files,
  apply the appropriate filters, and run the spell checker.

  When using Aspell, the vignette checking skips parameters and/or
  options of commands \verb{\Sexpr}, \verb{\citep}, \verb{\code},
  \verb{\pkg}, \verb{\proglang} and \verb{\samp}.  Further commands can
  be added by adding \verb{--add-tex-command} options to the
  \code{control} argument.  E.g., to skip both option and parameter of
  \verb{\mycmd}, add \verb{--add-tex-command='mycmd op'}.

  Suitable values for \code{control}, \code{program} and \code{drop} and
  personal dictionaries can also be specified using a package defaults
  file which should go as \file{defaults.R} into the \file{.aspell}
  subdirectory of \code{dir}, and provides defaults via assignments of
  suitable named lists, as e.g. \preformatted{  vignettes <- list(control = "--add-tex-command='mycmd op'")}
  for vignettes (when using Aspell) and assigning to \code{Rd_files} for
  Rd files defaults, and using elements \code{program}, \code{drop} and
  \code{personal} for the respective default values.

  Maintainers of packages using both English and American spelling will
  find it convenient to pass control options \option{--master=en_US} and
  \option{--add-extra-dicts=en_GB} to Aspell and \option{-d en_US,en_GB}
  to Hunspell (provided that the corresponding dictionaries are
  installed).

  One can also use personal dictionaries containing additional words to
  be accepted as spelled correctly.  Via
  \code{aspell_write_personal_dictionary_file}, a personal dictionary
  file can be created by either giving the words directly as a character
  vector, or as an object from a call to \code{aspell()} (in which case
  all possibly misspelled words contained in the object are taken).
  Most conveniently, the file is then moved to the package source
  \file{.aspell} subdirectory (named, e.g., \file{vignettes.pws}) and
  then activated via the defaults file using, e.g., \preformatted{  vignettes <- list(control = "--add-tex-command='mycmd op'",
                    personal = "vignettes.pws")}
}
\seealso{
  \code{\link{aspell}}
}
\keyword{utilities}
