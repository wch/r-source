\name{InternalMethods}
\alias{InternalMethods}
\alias{InternalGenerics}
\alias{internal generic}
\title{Internal Generic Functions}
\description{
  Many \R-internal functions are \emph{generic} and allow
  methods to be written for.
}

\details{
  % -------------------- grep -nw DispatchOrEval src/*/*[ch]
  %                         --------------
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

  \code{\link{as.character}},
  \code{\link{as.vector}},%   % do_asvector()        [coerce.c]
  \code{\link{is.array}},
  \code{\link{is.atomic}},
  \code{\link{is.call}},
  \code{\link{is.character}},
  \code{\link{is.complex}},
  \code{\link{is.double}},
  \code{\link{is.environment}},
  \code{\link{is.function}},
  \code{\link{is.integer}},
  \code{\link{is.language}},
  \code{\link{is.logical}},
  \code{\link{is.list}},
  \code{\link{is.matrix}},
  \code{\link{is.na}},%       % do_isna()            [coerce.c]
  \code{\link{is.nan}},%      % do_isnan()           [coerce.c]
  \code{\link{is.null}},
  \code{\link{is.numeric}},
  \code{\link{is.object}},
  \code{\link{is.pairlist}},
  \code{\link{is.recursive}},
  \code{\link{is.single}},
  \code{\link{is.symbol}},
  \code{\link{rep}} and%                             [seq.c]
  \code{\link{seq.int}}.%                            [seq.c]

  In addition, \code{is.name} is a synonym for \code{is.symbol} and
  dispatches methods for the latter.

  Note that most of the \link{group generic} functions are also
  internal/primitive and allow methods to be written for them.
}
\seealso{
  \code{\link{methods}} for the methods which are available.
}
\keyword{methods}
