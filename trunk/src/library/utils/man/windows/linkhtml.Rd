\name{link.html.help}
\alias{link.html.help}
\alias{make.packages.html}
\alias{make.search.html}
\alias{fixup.package.URLs}
\alias{fixup.libraries.URLs}
\title{Update HTML documentation files}
\usage{
link.html.help(verbose = FALSE, lib.loc = .libPaths())
make.packages.html(lib.loc = .libPaths())
make.search.html(lib.loc = .libPaths())

fixup.package.URLs(pkg, force = FALSE)
fixup.libraries.URLs(lib.loc = .libPaths())
}
\description{
  Functions to re-create the HTML documentation files to reflect all
  installed packages. \code{make.packages.html} creates
  the list of packages and \code{make.search.html} creates the
  database used by the Java search engine. \code{link.html.help} runs
  both, as well as \code{fixup.libraries.URLs} which attempts to
  fix up cross-library links.
}
\arguments{
  \item{verbose}{logical. If true, print out a message. For use to
    explain a delay when called from other functions.}
  \item{lib.loc}{character vector. List of libraries to be included.}
  \item{pkg}{character vector. The full path to a package.}
  \item{force}{logical. Should the links be fixed even if the stamp is
    present?}
}
\details{
  Cross-library links do not work on this platform.
  \code{fixup.package.URLs} attempts to correct links in the
  named package to the \code{doc} directory (usually to icons) and to the
  \code{base}, \code{datasets}, \code{utils}, \code{grDevices},
  \code{graphics} and \code{stats} packages,
  and then stamps a file \file{fixedHTMLlinks} in the package directory.
  If that file is found, no correction is attempted unless \code{force =
    TRUE} or the \R installation directory (recorded in the stamp) has been
  changed.

  \code{fixup.libraries.URLs} calls \code{fixup.package.URLs} on all
  packages in current library trees except the standard one.

  The functions will give a warning if the files are not writable.
}
\value{
  \code{make.packages.html} and \code{fixup.package.URLs} return a
  logical showing if they succeeded.
  
  The files
  \file{R\_HOME\\doc\\html\\packages.html} and
  \file{R\_HOME\\doc\\html\\search\\index.txt} are (re-)created by
  \code{make.packages.html} and \code{make.search.html} respectively.
}
\note{
  You will need write permission in the \file{R\_HOME\\doc\\html}
  directory to use any of the first three functions, and in the
  appropriate library trees to use the last two.
}
\keyword{utilities}
