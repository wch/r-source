\name{Rscript}
\alias{Rscript}
\title{Scripting Front-End for R}
\description{
  This is an alternative front end for use in \code{#!} scripts and
  other scripting applications.
}
\usage{
\special{Rscript [options] [-e expression] file [args]}
}
\arguments{
  \item{options}{A list of options beginning with \code{--}.  These can
    be any of the options of the standard \R front-end, and also those
    described in the details.}
  \item{expression}{a \R expression.}
  \item{file}{The name of a file containing \R commands.  \code{-}
    indicates \file{stdin}.}
  \item{args}{Arguments to be passed to the script in \code{file}.}
}
\details{
  \code{Rscript --help} gives details of usage, and \code{Rscript
    --version} gives the version of \code{Rscript}.

  Other invocations invoke the \R front-end with selected options.  This
  front-end is convenient for writing \code{#!} scripts since it is an
  executable and takes \code{file} directly as an argument.  Options
  \code{--slave --no-restore} are always supplied: these imply
  \code{--no-save}.

  \emph{Either} one or more \option{-e} options or \code{file} should
  be supplied.

  Additional options accepted (before \code{file} or \code{args}) are
  \describe{
    \item{\code{--verbose}}{gives details of what \code{Rscript} is
      doing.  Also passed on to \R.}
    \item{\code{--default-packages=list}}{where \code{list} is a
      comma-separated list of package names or \code{NULL}.  Sets the
      environment variable \env{R\_DEFAULT\_PACKAGES} which determines the
      packages loaded on startup.  The default for \code{Rscript} omits
      \pkg{methods} as it takes about 60\% of the startup time.
    }
  }

#ifdef unix
  Normally the version of \R is determined at installation, but this can
  be overridden by setting the environment variable \env{RHOME}.
#endif
#ifdef windows
  The \R files are found from the location of the \file{Rscript.exe}
  executable.   If this is copied elsewhere, the environment variable
  \env{RHOME} should be set to the top directory of the \R installation.
  
  Unlike Unix-alikes, this links directly to \file{R.dll} rather than
  running a separate process.
#endif

  \code{\link{stdin}()} refers to the input file, and
  \code{\link{file}("stdin")} to the \code{stdin} file stream of the
  process.
}
#ifdef unix
\note{
  \code{Rscript} is only supported on systems with the \code{execv}
  system call.
}
#endif
\examples{\dontrun{
Rscript -e 'date()' -e 'format(Sys.time(), "%a %b %d %X %Y")'

## example #! script

#! /path/to/Rscript --vanilla --default-packages=utils
args <- commandArgs(); args <- args[-(1:match("--args", args)]
res <- try(install.packages(args))
if(inherits(res, "try-error")) q(status=1) else q()

}}
\keyword{utilities}
