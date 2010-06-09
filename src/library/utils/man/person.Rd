% File src/library/utils/man/person.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{person}
\title{Person Names and Contact Information}
\alias{person}
\alias{as.person}
\alias{as.person.default}
\alias{as.character.person}
\alias{toBibtex.person}
\alias{personList}
\alias{as.personList}
\alias{as.personList.person}
\alias{as.personList.default}
% <FIXME>
%   \alias{as.character.personList}
%   \alias{toBibtex.personList}
% </FIXME>
\description{
  A class and utility methods for holding information about persons
  like name and email address.
}
\usage{
person(given = NULL, family = NULL, middle = NULL,
       email = NULL, role = NULL, comment = NULL,
       first = NULL, last = NULL)
as.person(x)

personList(...)
as.personList(x)
}
% <FIXME>
%   \S3method{as.character}{person}(x, ...)
%   \S3method{as.character}{personList}(x, ...)
%   \S3method{toBibtex}{person}(object, ...)
%   \S3method{toBibtex}{personList}(object, ...)
% </FIXME>
\arguments{
  \item{given}{a character string with the \emph{given} names.}
  \item{family}{a character string with the \emph{family} name.}
  \item{middle}{character string, middle name(s)}
  \item{email}{character string, email address}
  \item{role}{a character string specifying the role of the person (see
    \bold{Details}).}
  \item{comment}{a character string providing a comment.}
  \item{first}{character string, first name.}
  \item{last}{character string, last name}
  
  \item{\dots}{for \code{personList} an arbitrary number of \code{person}
    objects}
  \item{x}{a character string or an object of class \code{person} or
    \code{personList}}
% <FIXME>  
%  \item{object}{an object of class \code{person} or
%    \code{personList}}
% </FIXME>
}
\details{
  Personal names are rather tricky, e.g.,
  \url{http://en.wikipedia.org/wiki/Personal_name}.

  The current implementation of the \code{"person"} class uses the
  notions of \emph{given} (including middle names) and \emph{family}
  names, as specified by \code{given} and \code{family} respectively.
  Earlier version used a scheme based on first, middle and last names,
  as appropriate for most of Western culture where the given name
  precedes the family name, but not universal, as some other cultures
  place it after the family name, or use no family name.  To smooth the
  transition to the new scheme, arguments \code{first}, \code{middle}
  and \code{last} are still supported, but there use is deprecated and
  they must not be given in combination with the corresponding new style
  arguments.

  The new scheme also adds the possibility of specifying \emph{roles}
  based on a subset of the MARC Value List for Relators and Roles
  (\url{http://www.loc.gov/standards/sourcelist/relator-role.html}).
  When giving the roles of persons in the context of authoring \R
  packages, the following usage is suggested.
  \describe{
    \item{\code{"aut"}}{(Author) Use for full authors who have made
      substantial contributions to the package and should show up in the
      package citation.}
    \item{\code{"com"}}{(Compiler) Use for package maintainers that
      collected code (potentially in other languages) but did not make
      further substantial contributions to the package.} 
    \item{\code{"ctb"}}{(Contributor) Use for authors who have made
      smaller contributions (such as code patches etc.) but should not
      show up in the package citation.}
    \item{\code{"cph"}}{(Copyright holder) Use for all copyright holders.}
    \item{\code{"cre"}}{(Creator) Use for the package maintainer.}
    \item{\code{"ths"}}{(Thesis advisor) If the package is part of a
      thesis, use for the thesis advisor.}
    \item{\code{"trl"}}{(Translator) If the R code is merely a
      translation from another language (typically S), use for the
      translator to R.}
  }

}
\keyword{misc}
\examples{
## create a person object directly
p1 <- person("Karl", "Pearson", email = "pearson@stats.heaven")
p1

## convert a string
p2 <- as.person("Ronald Aylmer Fisher")
p2

## create one object holding both
p <- personList(p1, p2)
ps <- as.character(p)
ps
as.personList(ps)

## convert to BibTeX author field
toBibtex(p)
}
