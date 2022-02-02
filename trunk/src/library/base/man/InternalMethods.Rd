% File src/library/base/man/zMethods.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
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

  % DispatchOrEval internal generic: [
  \code{\link{[}},%            % do_subset()         [subset.c]
  % DispatchOrEval internal generic: [[
  \code{\link{[[}},%           % do_subset2()        [subset.c]
  % DispatchOrEval internal generic: $
  \code{\link{$}},
  % DispatchOrEval internal generic: [<-
  \code{\link{[<-}},
  % DispatchOrEval internal generic: [[<-
  \code{\link{[[<-}},
  % DispatchOrEval internal generic: $<-
  \code{\link{$<-}},

  % DispatchOrEval internal generic: length
  \code{\link{length}},%      % do_length()          [array.c]
  % DispatchOrEval internal generic: length<-
  \code{\link{length<-}},%    % do_lengthgets()      [builtin.c]
  % DispatchOrEval internal generic: lengths
  \code{\link{lengths}},%     % do_lengths()         [array.c]
  % DispatchOrEval internal generic: dimnames
  \code{\link{dimnames}},%    % do_dimnames()        [attrib.c]
  % DispatchOrEval internal generic: dimnames<-
  \code{\link{dimnames<-}},%  % do_dimnamesgets()    [attrib.c]
  % DispatchOrEval internal generic: dim
  \code{\link{dim}},%         % do_dim()             [attrib.c]
  % DispatchOrEval internal generic: dim<-
  \code{\link{dim<-}},%       % do_dimgets()         [attrib.c]
  % DispatchOrEval internal generic: names
  \code{\link{names}},%       % do_names()           [attrib.c]
  % DispatchOrEval internal generic: names<-
  \code{\link{names<-}},%     % do_namesgets()       [attrib.c]
  % DispatchOrEval internal generic: levels<-
  \code{\link{levels<-}},%    % do_levelsgets()      [attrib.c]
  % DispatchOrEval internal generic: @<-
  \code{\link{@<-}},%         % do_attrgets()        [attrib.c]  

  % DispatchOrEval internal generic: c  
  \code{\link{c}},%           % do_c()               [bind.c]
  % DispatchOrEval internal generic: unlist
  \code{\link{unlist}},%      % do_unlist()          [bind.c]
  \code{\link{cbind}},%       not via DispatchOrEval [bind.c]
  \code{\link{rbind}},

  % DispatchOrEval internal generic: as.character
  \code{\link{as.character}},
  % DispatchOrEval internal generic: as.complex
  \code{\link{as.complex}},
  % DispatchOrEval internal generic: as.double
  \code{\link{as.double}},
  % DispatchOrEval internal generic: as.integer
  \code{\link{as.integer}},
  % DispatchOrEval internal generic: as.logical
  \code{\link{as.logical}},
  % DispatchOrEval internal generic: as.raw
  \code{\link{as.raw}},
  % DispatchOrEval internal generic: as.vector
  \code{\link{as.vector}},%    % do_asvector()       [coerce.c]
  % DispatchOrEval internal generic: as.call
  \code{\link{as.call}},%      % do_ascall()         [coerce.c]
  % DispatchOrEval internal generic: as.environment
  \code{\link{as.environment}}%% do_as_environment() [envir.c]
  % DispatchOrEval internal generic: is.array
  \code{\link{is.array}},
  % DispatchOrEval internal generic: is.matrix
  \code{\link{is.matrix}},
  % DispatchOrEval internal generic: is.na
  \code{\link{is.na}},%       % do_isna()            [coerce.c]
  % DispatchOrEval internal generic: anyNA
  \code{\link{anyNA}},%       % do_anyNA()           [coerce.c]  
  % DispatchOrEval internal generic: is.nan
  \code{\link{is.nan}},%      % do_isnan()           [coerce.c]
  % DispatchOrEval internal generic: is.finite
  \code{\link{is.finite}}%    % do_isfinite()        [coerce.c]
  % DispatchOrEval internal generic: is.infinite
  \code{\link{is.infinite}}%    % do_isinfinite()    [coerce.c]
  % DispatchOrEval internal generic: is.numeric
  \code{\link{is.numeric}},
  % DispatchOrEval internal generic: nchar
  \code{\link{nchar}}%          % do_nchar()         [character.c]
  % DispatchOrEval internal generic: rep
  \code{\link{rep}},%                                [seq.c]
  % DispatchOrEval internal generic: rep.int
  \code{\link{rep.int}}%        % do_rep_int()       [seq.c]
  % DispatchOrEval internal generic: rep_len
  \code{\link{rep_len}}%        % do_rep_len()       [seq.c]
  % DispatchOrEval internal generic: seq.int
  \code{\link{seq.int}}
  (which dispatches methods for \code{"seq"}),%      [seq.c]
  % DispatchOrEval internal generic: is.unsorted
  \code{\link{is.unsorted}}%    % do_isunsorted()    [sort.c]
  and
  % DispatchOrEval internal generic: xtfrm
  \code{\link{xtfrm}}%                               [sort.c]

  In addition, \code{is.name} is a synonym for \code{\link{is.symbol}} and
  dispatches methods for the latter.  Similarly, \code{\link{as.numeric}}
  is a synonym for \code{as.double} and dispatches methods for the
  latter, i.e., S3 methods are for \code{as.double}, whereas S4 methods
  are to be written for \code{as.numeric}.

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
