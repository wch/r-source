% File src/library/base/man/zMethods.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{InternalMethods}
\alias{InternalMethods}
\alias{InternalGenerics}
\alias{internal generic}
\alias{.S3PrimitiveGenerics}
\title{Internal Generic Functions}
\description{
  Many \R-internal functions are \emph{generic} and allow
  methods to be written for.
}

  % -------------------- grep -nw DispatchOrEval src/*/*[ch]
  %                         --------------
\details{
  The following primitive and internal functions are \emph{generic},
  i.e., you can write \code{\link{methods}} for them:

  \code{\link{[}},%            % do_subset()         [subset.c]
  \code{\link{[[}},%           % do_subset2()        [subset.c]
  \code{\link{$}},
  \code{\link{[<-}},
  \code{\link{[[<-}},
  \code{\link{$<-}},

  \code{\link{length}},%      % do_length()          [array.c]
  \code{\link{length<-}},%    % do_lengthgets()      [builtin.c]
  \code{\link{dimnames}},%    % do_dimnames()        [attrib.c]
  \code{\link{dimnames<-}},%  % do_dimnamesgets()    [attrib.c]
  \code{\link{dim}},%         % do_dim()             [attrib.c]
  \code{\link{dim<-}},%       % do_dimgets()         [attrib.c]
  \code{\link{names}},%       % do_names()           [attrib.c]
  \code{\link{names<-}},%     % do_namesgets()       [attrib.c]
  \code{\link{levels<-}},%    % do_levelsgets()      [attrib.c]

  \code{\link{c}},%           % do_c()               [bind.c]
  \code{\link{unlist}},%      % do_unlist()          [bind.c]
  \code{\link{cbind}},%       not via DispatchOrEval [bind.c]
  \code{\link{rbind}},

  \code{\link{as.character}},
  \code{\link{as.complex}},
  \code{\link{as.double}},
  \code{\link{as.integer}},
  \code{\link{as.logical}},
  \code{\link{as.raw}},
  \code{\link{as.vector}},%   % do_asvector()        [coerce.c]
  \code{\link{is.array}},
  \code{\link{is.matrix}},
  \code{\link{is.na}},%       % do_isna()            [coerce.c]
  \code{\link{is.nan}},%      % do_isnan()           [coerce.c]
  \code{\link{is.numeric}},
  \code{\link{rep}},%                                [seq.c]
  \code{\link{seq.int}}
  (which dispatches methods for \code{"seq"})%       [seq.c]
  and \code{\link{xtfrm}}%                           [sort.c]

  In addition, \code{is.name} is a synonym for \code{is.symbol} and
  dispatches methods for the latter.

  Note that all of the \link{group generic} functions are also
  internal/primitive and allow methods to be written for them.

  \code{.S3PrimitiveGenerics} is a character vector listing the
  primitives which are internal generic and not \link{group generic}.
  Currently \code{\link{as.vector}}, \code{\link{cbind}},
  \code{\link{rbind}} and \code{\link{unlist}} are the internal
  non-primitive functions which are internally generic.

  For efficiency, internal dispatch only occurs on \emph{objects}, that
  is those for which \code{\link{is.object}} returns true.
}
\seealso{
  \code{\link{methods}} for the methods which are available.
}
\keyword{methods}
