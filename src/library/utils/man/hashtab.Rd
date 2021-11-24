% File src/library/utils/man/hashtab.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2009-2021 R Core Team
% Distributed under GPL 2 or later

\name{hashtab}
\alias{hashtab}
\alias{gethash}
\alias{sethash}
\alias{remhash}
\alias{numhash}
\alias{typhash}
\alias{maphash}
\alias{clrhash}
\alias{is.hashtab}
\alias{[[.hashtab}
\alias{[[<-.hashtab}
\alias{print.hashtab}
\alias{format.hashtab}
\title{
  Hash Tables (Experimental)
}
\description{
  Create and manipulate mutable hash tables.
}
\usage{
hashtab(type = c("identical", "address"))
gethash(h, key, nomatch = NULL)
sethash(h, key, value)
remhash(h, key)
numhash(h)
typhash(h)
maphash(h, FUN)
clrhash(h)
is.hashtab(x)
\method{[[}{hashtab}(h, key, nomatch = NULL, \dots)
\method{[[}{hashtab}(h, key, \dots) <- value
\method{print}{hashtab}(x, \dots)
\method{format}{hashtab}(x, \dots)
\method{length}{hashtab}(x)
}
\arguments{
  \item{type}{Character string specifying the hash table type.}
  \item{h}{A hash table.}
  \item{key}{An R object to use as a key.}
  \item{nomatch}{Value to return if \code{key} does not match.}
  \item{value}{New value to associate with \code{key}.}
  \item{FUN}{A function of two arguments, the key and the value, to call
    for each entry.}
  \item{x}{Object to be tested, printed, or formatted.}
  \item{\dots}{Additional arguments.}
}

\details{
  Hash tables are a data structure for efficiently associating keys with
  values.  Hash tables are similar to environments, but keys can be
  arbitrary objects. Like environments, and unlike named lists and most
  other objects in R, hash tables are mutable.

  New hash tables are created by \code{hashtab}.  Two variants are
  available: keys can be considered to match if they are
  \code{\link{identical}()} (\code{type = "identical"}, the default), or
  if their addresses in memory are equal (\code{type = "address"}). The
  default \code{"identical"} type is almost always the right choice.

  \code{gethash} returns the value associated with \code{key}.  If
  \code{key} is not present in the table, then the value of
  \code{nomatch} is returned.

  \code{sethash} adds a new key/value association or changes the current
  value for an existing key. \code{remhash} removes the entry for
  \code{key}, if there is one.

  \code{maphash} calls \code{FUN} for each entry in the hash table with
  two arguments, the entry key and the entry value. The order in which
  the entries are processed is not predictable. The consequence of
  \code{FUN} adding entries to the table or deleting entries from the
  table is also not predictable, except that removing the entry
  currently being processed will have the desired effect.

  \code{clrhash} removes all entries from the hash table.
}

\value{
  \code{hashtab} returns a new hash table of the specified \code{type}.

  \code{gethash} returns the value associated with \code{key}, or
  \code{nomatch} if there is no such value.

  \code{sethash} returns \code{value} invisibly.
  
  \code{remhash} invisibly returns \code{TRUE} if an entry for
  \code{key} was found and removed, and \code{FALSE} if no entry was
  found.

  \code{numhash} returns the current number of entries in the table.

  \code{typhash} returns a character string specifying the type of the
  hash table, one of \code{"identical"} or \code{"address"}.

  \code{maphash} and \code{clrhash} return \code{NULL} invisibly.
}

\section{Notes}{
  The interface design is based loosely on hash table support in Common
  Lisp.

  The hashing function used for \code{"identical"} hash tables is the
  same as the one used internally by \code{\link{duplicated}} and
  \code{\link{unique}}.

  As an experimental feature, the element operator \code{[[} can also be
  used get or set hash table entries, and \code{length} can be used to
  obtain the number of entries. It is not yet clear whether this is a
  good idea.
}

\examples{
## Create a new empty hash table.
h1 <- hashtab()
h1

## Add some key/value pairs.
sethash(h1, NULL, 1)
sethash(h1, .GlobalEnv, 2)
for (i in seq_along(LETTERS)) sethash(h1, LETTERS[i], i)

## Look up values for some keys.
gethash(h1, NULL)
gethash(h1, .GlobalEnv)
gethash(h1, "Q")

## Remove an entry.
(remhash(h1, NULL))
gethash(h1, NULL)
(remhash(h1, "XYZ"))

## Using the element operator.
h1[["ABC"]]
h1[["ABC", nomatch = 77]]
h1[["ABC"]] <- "DEF"
h1[["ABC"]]

## Integers and real numbers that are equal are considered different
## (not identical) as keys:
identical(3, 3L)
sethash(h1, 3L, "DEF")
gethash(h1, 3L)
gethash(h1, 3)

## Two variables can refer to the same hash table.
h2 <- h1
identical(h1, h2)
sethash(h2, NULL, 77)
gethash(h1, NULL)

}
\keyword{data}
\keyword{programming}
\keyword{utilities}
