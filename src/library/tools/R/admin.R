### * .install_package_description

.install_package_description <-
function(dir, outDir)
{
    ## Function for taking the DESCRIPTION package meta-information,
    ## checking/validating it, and installing it with the 'Built:'
    ## field added.  Note that from 1.7.0 on, packages without
    ## compiled code are not marked as being from any platform.

    ## Check first.  Note that this also calls .read_description(), but
    ## .check_package_description() currently really needs to know the
    ## path to the DESCRIPTION file, and returns an object with check
    ## results and not the package metadata ...
    ok <- .check_package_description(file.path(dir, "DESCRIPTION"))
    if(any(as.integer(sapply(ok, length))) > 0) {
        stop(paste("Invalid DESCRIPTION file",
                   paste(.capture_output_from_print(ok),
                         collapse = "\n"),
                   sep = "\n\n"), call.=FALSE)
    }

    db <- .read_description(file.path(dir, "DESCRIPTION"))
    ## should not have a Built: field, so ignore it if it is there
    nm <- names(db)
    if("Built" %in% nm) {
        db <- db[-match("Built", nm)]
        warning("*** someone has corrupted the Built field in package ",
                sQuote(db["Package"]), " ***", call.=FALSE)
    }

    OS <- Sys.getenv("R_OSTYPE")
    OStype <- if(nchar(OS) && OS == "windows")
        "i386-pc-mingw32"
    else
        R.version$platform
    Built <-
        paste("R ",
              paste(R.version[c("major", "minor")],
                    collapse = "."),
              "; ",
              if(file_test("-d", file.path(dir, "src"))) OStype
              else "",
              "; ",
              ## Prefer date in ISO 8601 format.
              ## Could also use
              ##   format(Sys.time(), "%a %b %d %X %Y")
              Sys.time(),
              "; ",
              .OStype(),
              sep = "")

    ## we must not split the Built: field across lines
    writeLines(c(formatDL(names(db), db, style = "list"),
                 paste("Built", Built, sep=": ")),
               file.path(outDir, "DESCRIPTION"))
    db["Built"] <- Built

    outMetaDir <- file.path(outDir, "Meta")
    if(!file_test("-d", outMetaDir) && !dir.create(outMetaDir))
         stop(paste("cannot open directory", sQuote(outMetaDir)))
    saveInfo <- .split_description(db)
    .saveRDS(saveInfo, file.path(outMetaDir, "package.rds"))

    invisible()
}

### * .split_description

.split_description <-
function(db)
{
    if(!is.na(Built <- db["Built"])) {
        Built <- as.list(strsplit(Built, "; ")[[1]])
        if(length(Built) != 4) {
            warning("*** someone has corrupted the Built field in package ",
                    sQuote(db["Package"]), " ***", call.=FALSE)
            Built <- NULL
        } else {
            names(Built) <- c("R", "Platform", "Date", "OStype")
            Built[["R"]] <- package_version(sub("^R ([0-9.]+)", "\\1",
                                                Built[["R"]]))
        }
    } else Built <- NULL
    ## might perhaps have multiple entries
    Depends <- .split_dependencies(db[names(db) %in% "Depends"])
    if("R" %in% names(Depends)) {
        Rdeps <- Depends[["R"]]
        Depends <- Depends[-match("R", names(Depends))]
    } else Rdeps <- NULL
    Rdeps <- as.vector(Rdeps)
    Suggests <- .split_dependencies(db[names(db) %in% "Suggests"])
    Imports <- .split_dependencies(db[names(db) %in% "Imports"])
    structure(list(DESCRIPTION = db, Built = Built, Rdepends = Rdeps,
                   Depends = Depends, Suggests = Suggests,
                   Imports = Imports),
              class = "packageDescription2")
}

### * .vinstall_package_descriptions_as_RDS

.vinstall_package_descriptions_as_RDS <-
function(dir, packages)
{
    ## For the given packages installed in @file{dir}, install their
    ## DESCRIPTION package metadata as R metadata.
    ## Really only useful for base packages under Unix.
    ## See @file{src/library/Makefile.in}.

    for(p in unlist(strsplit(packages, "[[:space:]]+"))) {
        meta_dir <- file.path(dir, p, "Meta")
        if(!file_test("-d", meta_dir) && !dir.create(meta_dir))
            stop(paste("cannot open directory", sQuote(meta_dir)))
        package_info_dcf_file <- file.path(dir, p, "DESCRIPTION")
        package_info_rds_file <- file.path(meta_dir, "package.rds")
        if(file_test("-nt",
                     package_info_rds_file,
                     package_info_dcf_file))
            next
        .saveRDS(.split_description(.read_description(package_info_dcf_file)), package_info_rds_file)
    }
    invisible()
}

### * .update_package_rds

.update_package_rds <-
function(lib.loc = NULL)
{
    ## rebuild the dumped package descriptions for all packages in lib.loc
    if (is.null(lib.loc)) lib.loc <- .libPaths()
    lib.loc <- lib.loc[file.exists(lib.loc)]
    for (lib in lib.loc) {
        a <- list.files(lib, all.files = FALSE, full.names = TRUE)
        for (nam in a) {
            dfile <- file.path(nam, "DESCRIPTION")
            if (file.exists(dfile)) {
                print(nam)
                .install_package_description(nam, nam)
            }
        }
    }
}

### * .install_package_code_files

.install_package_code_files <-
function(dir, outDir)
{
    if(!file_test("-d", dir))
        stop(paste("directory", sQuote(dir), "does not exist"))
    dir <- file_path_as_absolute(dir)

    ## Attempt to set the LC_COLLATE locale to 'C' to turn off locale
    ## specific sorting.
    curLocale <- Sys.getlocale("LC_COLLATE")
    on.exit(Sys.setlocale("LC_COLLATE", curLocale), add = TRUE)
    ## (Guaranteed to work as per the Sys.setlocale() docs.)
    lccollate <- "C"
    if(Sys.setlocale("LC_COLLATE", lccollate) != lccollate) {
        ## <NOTE>
        ## I don't think we can give an error here.
        ## It may be the case that Sys.setlocale() fails because the "OS
        ## reports request cannot be honored" (src/main/platform.c), in
        ## which case we should still proceed ...
        warning("cannot turn off locale-specific sorting via LC_COLLATE")
        ## </NOTE>
    }

    ## We definitely need a valid DESCRIPTION file.
    db <- .read_description(file.path(dir, "DESCRIPTION"))

    codeDir <- file.path(dir, "R")
    if(!file_test("-d", codeDir)) return(invisible())

    codeFiles <- list_files_with_type(codeDir, "code", full.names = FALSE)

    collationField <-
        c(paste("Collate", .OStype(), sep = "."), "Collate")
    if(any(i <- collationField %in% names(db))) {
        ## We have a Collate specification in the DESCRIPTION file:
        ## currently, file paths relative to codeDir, separated by
        ## white space, possibly quoted.  Note that we could have
        ## newlines in DCF entries but do not allow them in file names,
        ## hence we gsub() them out.
        collationField <- collationField[i][1]
        codeFilesInCspec <-
            scan(textConnection(gsub("\n", " ", db[collationField])),
                 what = character(), strip.white = TRUE, quiet = TRUE)
        ## Duplicated entries in the collaction spec?
        badFiles <-
            unique(codeFilesInCspec[duplicated(codeFilesInCspec)])
        if(length(badFiles)) {
            out <- paste("\nduplicated files in",
                         sQuote(collationField),
                         "field:")
            out <- paste(out,
                         paste(" ", badFiles, collapse = "\n"),
                         sep = "\n")
            stop(out)
        }
        ## See which files are listed in the collation spec but don't
        ## exist.
        badFiles <- codeFilesInCspec %w/o% codeFiles
        if(length(badFiles)) {
            out <- paste("\nfiles in ", sQuote(collationField),
                         " field missing from ", sQuote(codeDir),
                         ":",
                         sep = "")
            out <- paste(out,
                         paste(" ", badFiles, collapse = "\n"),
                         sep = "\n")
            stop(out)
        }
        ## See which files exist but are missing from the collation
        ## spec.  Note that we do not want the collation spec to use
        ## only a subset of the available code files.
        badFiles <- codeFiles %w/o% codeFilesInCspec
        if(length(badFiles)) {
            out <- paste("\nfiles in", sQuote(codeDir),
                         "missing from", sQuote(collationField),
                         "field:")
            out <- paste(out,
                         paste(" ", badFiles, collapse = "\n"),
                         sep = "\n")
            stop(out)
        }
        ## Everything's groovy ...
        codeFiles <- codeFilesInCspec
    }

    codeFiles <- file.path(codeDir, codeFiles)

    if(!file_test("-d", outDir) && !dir.create(outDir))
        stop(paste("cannot open directory", sQuote(outDir)))
    outCodeDir <- file.path(outDir, "R")
    if(!file_test("-d", outCodeDir) && !dir.create(outCodeDir))
        stop(paste("cannot open directory", sQuote(outCodeDir)))
    outFile <- file.path(outCodeDir, db["Package"])
    ## <NOTE>
    ## It may be safer to do
    ##   writeLines(sapply(codeFiles, readLines), outFile)
    ## instead, but this would be much slower ...
    if(!file.create(outFile))
        stop(paste("unable to create", outFile))
    writeLines(paste(".packageName <- \"", db["Package"], "\"", sep=""),
               outFile)
    # use fast version of file.append that ensure LF between files
    if(!all(.Internal(codeFiles.append(outFile, codeFiles))))
        stop("unable to write code files")
    ## </NOTE>

    invisible()
}


### * .install_package_indices

.install_package_indices <-
function(dir, outDir)
{
    options(warn = 1)                   # to ensure warnings get seen
    if(!file_test("-d", dir))
        stop(paste("directory", sQuote(dir), "does not exist"))
    if(!file_test("-d", outDir))
        stop(paste("directory", sQuote(outDir), "does not exist"))

    ## If there is an @file{INDEX} file in the package sources, we
    ## install this, and do not build it.
    if(file_test("-f", file.path(dir, "INDEX")))
        if(!file.copy(file.path(dir, "INDEX"),
                      file.path(outDir, "INDEX"),
                      overwrite = TRUE))
            stop(paste("unable to copy INDEX to",
                       file.path(outDir, "INDEX")))

    outMetaDir <- file.path(outDir, "Meta")
    if(!file_test("-d", outMetaDir) && !dir.create(outMetaDir))
         stop(paste("cannot open directory", sQuote(outMetaDir)))
    .install_package_Rd_indices(dir, outDir)
    .install_package_vignette_index(dir, outDir)
    .install_package_demo_index(dir, outDir)
    invisible()
}

### * .install_package_Rd_indices

.install_package_Rd_indices <-
function(dir, outDir)
{
    dir <- file_path_as_absolute(dir)
    docsDir <- file.path(dir, "man")
    if(!file_test("-d", docsDir)) return(invisible())

    dataDir <- file.path(outDir, "data")
    outDir <- file_path_as_absolute(outDir)
    ## <FIXME>
    ## Not clear whether we should use the basename of the directory we
    ## install to, or the package name as obtained from the DESCRIPTION
    ## file in the directory we install from (different for versioned
    ## installs).  We definitely do not want the basename of the dir we
    ## install from.
    packageName <- basename(outDir)
    ## </FIXME>

    indices <- c(file.path("Meta", "Rd.rds"),
                 file.path("Meta", "hsearch.rds"),
                 "CONTENTS", "INDEX")
    upToDate <- file_test("-nt", file.path(outDir, indices), docsDir)
    if(file_test("-d", dataDir)) {
        ## Note that the data index is computed from both the package's
        ## Rd files and the data sets actually available.
        upToDate <-
            c(upToDate,
              file_test("-nt",
                        file.path(outDir, "Meta", "data.rds"),
                        c(dataDir, docsDir)))
    }
    if(all(upToDate)) return(invisible())

    contents <- Rdcontents(list_files_with_type(docsDir, "docs"))

    .write_contents_as_RDS(contents,
                           file.path(outDir, "Meta", "Rd.rds"))

    .saveRDS(.build_hsearch_index(contents, packageName, outDir),
             file.path(outDir, "Meta", "hsearch.rds"))

    .write_contents_as_DCF(contents, packageName,
                           file.path(outDir, "CONTENTS"))

    ## If there is no @file{INDEX} file in the package sources, we
    ## build one.
    ## <NOTE>
    ## We currently do not also save this in RDS format, as we can
    ## always do
    ##   .build_Rd_index(.readRDS(file.path(outDir, "Meta", "Rd.rds"))
    if(!file_test("-f", file.path(dir, "INDEX")))
        writeLines(formatDL(.build_Rd_index(contents)),
                   file.path(outDir, "INDEX"))
    ## </NOTE>

    if(file_test("-d", dataDir)) {
        .saveRDS(.build_data_index(dataDir, contents),
                 file.path(outDir, "Meta", "data.rds"))
    }
    invisible()
}

### * .install_package_vignette_index

.install_package_vignette_index <-
function(dir, outDir)
{
    dir <- file_path_as_absolute(dir)
    vignetteDir <- file.path(dir, "inst", "doc")
    ## Create a vignette index only if the vignette dir exists.
    if(!file_test("-d", vignetteDir))
        return(invisible())

    outDir <- file_path_as_absolute(outDir)
    ## <FIXME>
    ## Not clear whether we should use the basename of the directory we
    ## install to, or the package name as obtained from the DESCRIPTION
    ## file in the directory we install from (different for versioned
    ## installs).  We definitely do not want the basename of the dir we
    ## install from.
    packageName <- basename(outDir)
    ## </FIXME>
    outVignetteDir <- file.path(outDir, "doc")
    if(!file_test("-d", outVignetteDir) && !dir.create(outVignetteDir))
        stop(paste("cannot open directory", sQuote(outVignetteDir)))

    ## If there is an HTML index in the @file{inst/doc} subdirectory of
    ## the package source directory (@code{dir}), we do not overwrite it
    ## (similar to top-level @file{INDEX} files).  Installation already
    ## copies/d this over.
    hasHtmlIndex <- file_test("-f", file.path(vignetteDir, "index.html"))
    htmlIndex <- file.path(outDir, "doc", "index.html")

    ## Write dummy HTML index if no vignettes are found and exit.
    if(!length(list_files_with_type(vignetteDir, "vignette"))) {
        if(!hasHtmlIndex)
            .writeVignetteHtmlIndex(packageName, htmlIndex)
        return(invisible())
    }

    vignetteIndex <- .build_vignette_index(vignetteDir)
    ## For base package vignettes there is no PDF in @file{vignetteDir}
    ## but there might/should be one in @file{outVignetteDir}.
    if(NROW(vignetteIndex) > 0) {
        vignettePDFs <-
            sub("$", ".pdf",
                basename(file_path_sans_ext(vignetteIndex$File)))
        ind <- file_test("-f", file.path(outVignetteDir, vignettePDFs))
        vignetteIndex$PDF[ind] <- vignettePDFs[ind]
    }
    if(!hasHtmlIndex)
        .writeVignetteHtmlIndex(packageName, htmlIndex, vignetteIndex)

    .saveRDS(vignetteIndex,
             file = file.path(outDir, "Meta", "vignette.rds"))

    invisible()
}

### * .install_package_demo_index

.install_package_demo_index <-
function(dir, outDir)
{
    demoDir <- file.path(dir, "demo")
    if(!file_test("-d", demoDir)) return(invisible())
    demoIndex <- .build_demo_index(demoDir)
    .saveRDS(demoIndex,
             file = file.path(outDir, "Meta", "demo.rds"))
    invisible()
}

### * .vinstall_package_indices

.vinstall_package_indices <-
function(src_dir, out_dir, packages)
{
    ## For the given packages with sources rooted at @file{src_dir} and
    ## installations rooted at @file{out_dir}, install the package
    ## indices.
    ## Really only useful for base packages under Unix.
    ## See @file{src/library/Makefile.in}.

    for(p in unlist(strsplit(packages, "[[:space:]]+")))
        tools:::.install_package_indices(file.path(src_dir, p),
                                         file.path(out_dir, p))
    invisible()
}

### * .install_package_vignettes

.install_package_vignettes <-
function(dir, outDir)
{
    dir <- file_path_as_absolute(dir)
    vignetteDir <- file.path(dir, "inst", "doc")
    if(!file_test("-d", vignetteDir))
        return(invisible())
    vignetteFiles <- list_files_with_type(vignetteDir, "vignette")
    if(!length(vignetteFiles))
        return(invisible())

    outDir <- file_path_as_absolute(outDir)
    outVignetteDir <- file.path(outDir, "doc")
    if(!file_test("-d", outVignetteDir) && !dir.create(outVignetteDir))
        stop(paste("cannot open directory", sQuote(outVignetteDir)))
    ## For the time being, assume that no PDFs are available in
    ## vignetteDir.
    vignettePDFs <-
        file.path(outVignetteDir,
                  sub("$", ".pdf",
                      basename(file_path_sans_ext(vignetteFiles))))
    upToDate <- file_test("-nt", vignettePDFs, vignetteFiles)
    if(all(upToDate))
        return(invisible())

    ## For the time being, the primary use of this function is to
    ## install (and build) vignettes in base packages.  Hence, we build
    ## in a subdir of the current directory rather than a temp dir: this
    ## allows inspection of problems and automatic cleanup via Make.
    cwd <- getwd()
    buildDir <- file.path(cwd, ".vignettes")
    if(!file_test("-d", buildDir) && !dir.create(buildDir))
        stop(paste("cannot create directory", sQuote(buildDir)))
    on.exit(setwd(cwd))
    setwd(buildDir)

    ## Argh.  We need to ensure that vignetteDir is in TEXINPUTS and
    ## BIBINPUTS.
    envSep <- if(.Platform$OS.type == "windows") ";" else ":"
    ## (Yes, it would be nice to have envPath() similar to file.path().)
    texinputs <- Sys.getenv("TEXINPUTS")
    bibinputs <- Sys.getenv("BIBINPUTS")
    on.exit(Sys.putenv(TEXINPUTS = texinputs, BIBINPUTS = bibinputs),
            add = TRUE)
    Sys.putenv(TEXINPUTS = paste(vignetteDir, Sys.getenv("TEXINPUTS"),
               sep = envSep),
               BIBINPUTS = paste(vignetteDir, Sys.getenv("BIBINPUTS"),
               sep = envSep))

    for(srcfile in vignetteFiles[!upToDate]) {
        base <- basename(file_path_sans_ext(srcfile))
        texfile <- paste(base, ".tex", sep = "")
        yy <- try(Sweave(srcfile, pdf = TRUE, eps = FALSE, quiet =
                         TRUE))
        if(inherits(yy, "try-error"))
            stop(yy)
        ## In case of an error, do not clean up: should we point to
        ## buildDir for possible inspection of results/problems?
        if(.Platform$OS.type == "windows") {
            ## may not have texi2dvi
            res <- system(paste("pdflatex", texfile))
            if(res)
                stop(paste("unable to run pdflatex on",
                           sQuote(texfile)))
            if(length(grep("\\bibdata",
                           readLines(paste(base, ".aux", sep = ""))))) {
                res <- system(paste("bibtex", base))
                if(res)
                    stop(paste("unable to run bibtex on",
                               sQuote(base)))
                res <- system(paste("pdflatex", texfile))
                if(res)
                    stop(paste("unable to run pdflatex on",
                               sQuote(texfile)))
            }
            res <- system(paste("pdflatex", texfile))
            if(res)
                stop(paste("unable to run pdflatex on",
                           sQuote(texfile)))
        } else
            texi2dvi(texfile, pdf = TRUE, quiet = TRUE)
        pdffile <-
            paste(basename(file_path_sans_ext(srcfile)), ".pdf", sep = "")
        if(!file.exists(pdffile))
            stop(paste("file", sQuote(pdffile), "was not created"))
        if(!file.copy(pdffile, outVignetteDir, overwrite = TRUE))
            stop(paste("cannot copy", sQuote(pdffile), "to",
                       sQuote(outVignetteDir)))
    }
    ## Need to change out of this dir before we delete it, at least on
    ## Windows.
    setwd(cwd)
    unlink(buildDir, recursive = TRUE)
    ## Now you need to update the HTML index!
    .install_package_vignette_index(dir, outDir)
    invisible()
}

### * .install_package_namespace_info

.install_package_namespace_info <-
function(dir, outDir)
{
    dir <- file_path_as_absolute(dir)
    nsFile <- file.path(dir, "NAMESPACE")
    if(!file_test("-f", nsFile)) return(invisible())
    nsInfoFilePath <- file.path(outDir, "Meta", "nsInfo.rds")
    if(file_test("-nt", nsInfoFilePath, nsFile)) return(invisible())
    nsInfo <- parseNamespaceFile(basename(dir), dirname(dir))
    outMetaDir <- file.path(outDir, "Meta")
    if(!file_test("-d", outMetaDir) && !dir.create(outMetaDir))
        stop(paste("cannot open directory", sQuote(outMetaDir)))
    .saveRDS(nsInfo, nsInfoFilePath)
    invisible()
}

### * .vinstall_package_namespaces_as_RDS

.vinstall_package_namespaces_as_RDS <-
function(dir, packages)
{
    ## For the given packages installed in @file{dir} which have a
    ## NAMESPACE file, install the namespace info as R metadata.
    ## Really only useful for base packages under Unix.
    ## See @file{src/library/Makefile.in}.

    for(p in unlist(strsplit(packages, "[[:space:]]+")))
        .install_package_namespace_info(file.path(dir, p),
                                        file.path(dir, p))
    invisible()
}


### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***
