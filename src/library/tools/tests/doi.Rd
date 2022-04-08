\RdOpts{stage = build}   % emulate pre-4.2.0 default for \doi
\name{doi}
\title{Test \verb{\\doi} with hash or Rd specials}
\description{
  \doi{10.1000/456#789}  % example from DOI handbook (Section 2.5.2.3)
  
  \doi{10.1000/\{\}}     % hypothetical DOI with curly braces
}
