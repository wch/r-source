\name{InternalMethods}
\alias{InternalMethods}
\title{Internal Generic Functions}
\description{
  Many \R-internal functions are \emph{generic} and allow
  methods to be written for.
}

\details{
  % -------------------- grep -nw DispatchOrEval src/*/*[ch]
  %                         --------------
  The following builtin functions are \emph{generic} as well, i.e., you
  can write \code{\link{methods}} for them:

  \code{\link{[}},            % do_subset()          [subset.c:464]
  \code{\link{[[}},           % do_subset2()         [subset.c:567]
  \code{\link{$}},
  \code{\link{[<-}},
  \code{\link{[[<-}},
  \code{\link{$<-}},

  \code{\link{length}},
  \code{\link{length<-}},
    
  \code{\link{dimnames<-}},   % do_dimnamesgets()    [attrib.c:422]
  \code{\link{dimnames}},     % do_dimnames()        [attrib.c:489]
  \code{\link{dim<-}},        % do_dimgets()         [attrib.c:513]
  \code{\link{dim}}           % do_dim()             [attrib.c:501]
    
  \code{\link{c}},            % do_c()               [bind.c:524]
  \code{\link{unlist}},       % do_unlist()          [bind.c:648]
  
  \code{\link{as.character}},
  \code{\link{as.vector}},    % do_asvector()        [coerce.c:948]
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
  \code{\link{is.na}},        % do_isna()            [coerce.c:1244]
  \code{\link{is.nan}}        % do_isnan()           [coerce.c:1332]
  \code{\link{is.null}},
  \code{\link{is.numeric}},
  \code{\link{is.object}},
  \code{\link{is.pairlist}},
  \code{\link{is.recursive}},
  \code{\link{is.single}},
  \code{\link{is.symbol}}.
}
\seealso{
  \code{\link{methods}} for the methods of non-Internal generic functions.
}
\keyword{methods}
