\name{tools-internal}
\title{Internal tools Objects}
\alias{.convertFilePathToAbsolute}
\alias{.fileTest}
\alias{.isS3Generic}
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
\alias{Rdpp}
\alias{Rdinfo}
\alias{Rdcontents}
\alias{Rdindex}
\alias{Rd2contents}
\description{
  Internal tools functions.
}
\usage{
sQuote(s)
.convertFilePathToAbsolute(path)
.fileTest(op, x, y)
.isS3Generic(fname, envir)
.listFilesWithExts(dir, exts, path = TRUE)
.loadPackageQuietly(package, lib.loc)
.makeS3MethodsStopList(package)
.sourceAssignments(file, envir)
Rdpp(lines)
Rdinfo(file)
Rdcontents(RdFiles)
Rdindex(RdFiles, outFile = "", type = NULL,
        width = 0.9 * getOption("width"), indent = NULL)
Rd2contents(dir, outFile = "")
}
\details{
  These are not to be called by the user.
}
\keyword{internal}
