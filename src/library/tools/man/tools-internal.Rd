\name{tools-internal}
\title{Internal tools Objects}
\alias{.convertFilePathToAbsolute}
\alias{.fileTest}
\alias{.isS3Generic}
\alias{.listFilesWithExts}
\alias{.loadPackageQuietly}
\alias{.makeFileExts}
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
\alias{.installPackageDescription}
\alias{.installPackageIndices}
\alias{.installPackageRdIndices}
\alias{.installPackageVignetteIndex}
\alias{.writeContentsRDA}
\alias{.writeContentsDCF}
\alias{.buildRdIndex}
\alias{.buildVignetteIndex}
\description{
  Internal tools functions.
}
\usage{
sQuote(s)
.convertFilePathToAbsolute(path)
.fileTest(op, x, y)
.isS3Generic(fname, envir)
.listFilesWithExts(dir, exts, path = TRUE)
.listFilesWithType(dir, type)
.loadPackageQuietly(package, lib.loc)
.makeFileExts(type)
.makeS3MethodsStopList(package)
.sourceAssignments(file, envir)
Rdpp(lines)
Rdinfo(file)
Rdcontents(RdFiles)
Rdindex(RdFiles, outFile = "", type = NULL,
        width = 0.9 * getOption("width"), indent = NULL)
Rd2contents(dir, outFile = "")
.writeContentsRDA(contents, outFile)
.writeContentsDCF(contents, packageName, outFile)
.buildRdIndex(contents, type = NULL)
.buildVignetteIndex(vignetteFiles)
.installPackageDescription(dir, outDir)
.installPackageIndices(dir, outDir)
.installPackageRdIndices(dir, outDir)
.installPackageVignetteIndex(dir, outDir)
}
\details{
  These are not to be called by the user.
}
\keyword{internal}
