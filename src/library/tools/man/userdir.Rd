% File src/library/tools/man/userdir.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2020 R Core Team
% Distributed under GPL 2 or later

\name{userdir}
\alias{R_user_dir}
\title{R User Directories}
\description{
  Directories for storing R-related user-specific data, configuration
  and cache files.
}
\usage{
R_user_dir(package, which = c("data", "config", "cache"))
}
\arguments{
  \item{package}{a character string giving the name of an \R package}
  \item{which}{a character string indicating the kind of file(s) of
    interest.  Can be abbreviated.}
}
\details{
  For desktop environments using X Windows, the \I{freedesktop.org} project
  (formerly X Desktop Group, \abbr{XDG}) developed the \abbr{XDG} Base Directory
  Specification
  (\url{https://specifications.freedesktop.org/basedir-spec}) for
  standardizing the location where certain files should be placed.  CRAN
  package \CRANpkg{rappdirs} provides these general locations with
  appropriate values for all platforms for which \R is available.

  \code{R_user_dir} specializes the general mechanism to R package
  specific locations for user files, by providing package specific
  subdirectories inside a \file{R} subdirectory inside the ``base''
  directories appropriate for user-specific data, configuration and
  cache files (see the examples), with the intent that packages will not
  interfere if they work within their respective subdirectories.

  The locations of these base directories can be customized via the
  specific environment variables
  \env{R_USER_DATA_DIR},
  \env{R_USER_CONFIG_DIR} and
  \env{R_USER_CACHE_DIR}.
  If these are not set, the general \abbr{XDG}-style environment variables
  \env{XDG_DATA_HOME},
  \env{XDG_CONFIG_HOME} and
  \env{XDG_CACHE_HOME}
  are used if set, and otherwise, defaults appropriate for the \R
  platform in use are employed.
}
\examples{\dontdiff{
R_user_dir("FOO", "cache")

## Create one, platform agnostically, must work if <normal> :
(Rdb <- R_user_dir("base"))
if(newD <- !dir.exists(Rdb)) # should work user specifically:
    newD <- dir.create(Rdb, recursive=TRUE)% FALSE + warning when testing homeless
dir(Rdb) # typically empty
if(newD) unlink(Rdb) # cleaning up

list.files(R_user_dir("grid"), full.names = TRUE)
}}
