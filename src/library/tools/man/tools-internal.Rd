\name{tools-internal}
\title{Internal tools functions}
\alias{.convertFilePathToAbsolute}
\alias{.listFilesWithExts}
\alias{.loadPackageQuietly}
\alias{.makeS3MethodsStopList}
\alias{.sourceAssignments}
\alias{sQuote}
\alias{SweaveParseOptions}
\alias{SweaveHooks}
\alias{RweaveLatexRuncode}
\alias{RweaveLatexWritedoc}
\alias{RweaveLatexFinish}
\alias{RweaveLatexOptions}
\alias{RweaveChunkPrefix}
\alias{RweaveEvalWithOpt}
\alias{RtangleRuncode}
\alias{RtangleWritedoc}
\alias{RtangleFinish}
\alias{SweaveGetSyntax}
\alias{buildVignettes}
\alias{pkgVignettes}
\description{
  Internal tools functions.
}
\usage{
sQuote(s)
.convertFilePathToAbsolute(path)
.listFilesWithExts(dir, exts, path = TRUE)
.loadPackageQuietly(package, lib.loc)
.makeS3MethodsStopList(package)
.sourceAssignments(file, envir)
}
\details{
  These are not to be called by the user.
}
\keyword{internal}
