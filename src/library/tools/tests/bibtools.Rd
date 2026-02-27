\name{bibtools}
\title{Bibliographic Citations and References}
\description{
  \bibcitet{|R:Meyer:1953|p.\sspace{}1}
  \bibcitep{non-existent-key}% should warn
}
\references{
  \bibshow{*,R:Lamport:1994}
}
\seealso{
  \bibcitet{|R:Hornik+Murdoch:2011|
    which is cited only here so not shown above}
  % --> tools:::.check_Rd_bibentries_cited_not_shown()
}
