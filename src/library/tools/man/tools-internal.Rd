\name{tools-internal}
\title{Internal tools Objects}
% \alias{.isS3Generic}
% \alias{.loadPackageQuietly}
% \alias{.makeFileExts}
% \alias{.makeS3MethodsStopList}
% \alias{.sourceAssignments}
% \alias{SweaveParseOptions}
% \alias{SweaveHooks}
% \alias{RweaveLatexRuncode}
% \alias{RweaveLatexWritedoc}
% \alias{RweaveLatexFinish}
\alias{RweaveLatexOptions}
% \alias{RweaveChunkPrefix}
% \alias{RweaveEvalWithOpt}
% \alias{RtangleRuncode}
\alias{RtangleWritedoc}
% \alias{RtangleFinish}
% \alias{SweaveGetSyntax}
\alias{buildVignettes}
\alias{pkgVignettes}
% \alias{Rdpp}
% \alias{Rdinfo}
% \alias{Rdcontents}
% \alias{Rd2contents}
\alias{.installPacakgeCodeFiles}
\alias{.installPackageDescription}
\alias{.installPackageIndices}
% \alias{.installPackageRdIndices}
% \alias{.installPackageVignetteIndex}
% \alias{.installPackageDemoIndex}
% \alias{.writeContentsRDS}
% \alias{.writeContentsDCF}
% \alias{.buildRdIndex}
% \alias{.buildVignetteIndex}
% \alias{.buildDemoIndex}
% \alias{.buildDataIndex}
\alias{.checkVignetteIndex}
\alias{print.checkVignetteIndex}
\alias{.checkDemoIndex}
\alias{print.checkDemoIndex}
% \alias{.stripRdComments}
\description{
  Internal tools functions.
}
\usage{
RweaveLatexOptions(options)
RtangleWritedoc(object, chunk)
buildVignettes(package, dir, lib.loc = NULL)
pkgVignettes(package, dir, lib.loc = NULL)

% .isS3Generic(fname, envir)
% .loadPackageQuietly(package, lib.loc)
% .makeFileExts(type)
% .makeS3MethodsStopList(package)
% .sourceAssignments(file, envir)
% Rdpp(lines)
% Rdinfo(file)
% Rdcontents(RdFiles)
% Rd2contents(dir, outFile = "")
% .stripRdComments(lines)
% .writeContentsRDS(contents, outFile)
% .writeContentsDCF(contents, packageName, outFile)
% .buildDataIndex(dataDir, contents)
% .buildDemoIndex(demoDir)
% .buildRdIndex(contents, type = NULL)
% .buildVignetteIndex(vignetteDir)
.installPackageCodeFiles(dir, outDir)
.installPackageDescription(dir, outDir)
.installPackageIndices(dir, outDir)
% .installPackageRdIndices(dir, outDir)
% .installPackageVignetteIndex(dir, outDir)
% .installPackageDemoIndex(dir, outDir)
.checkDemoIndex(demoDir)
.checkVignetteIndex(vignetteDir)
}
\details{
  These are not to be called by the user.
}
\keyword{internal}
