#  File src/library/tools/R/apitools.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2024 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

##
## Work out the function API from information in WRE
##

## WRE data is now installed in system.file(package = "tools", "wre.txt")
## WRE(newpath) forces a new load with the new path.

apidata <-
    list2env(
        list(
            wrelines = NULL,
            wreloc = NULL,
            fapi = NULL,
            rfuns = NULL))

resetAPI <- function(newloc = "") {
    if (newloc != "")
        apidata$wreloc <- newloc
    apidata$wrelines <- NULL
    apidata$fapi <- NULL
    ## reset rfuns also?
}

WRE <- function() {
    if (is.null(apidata$wrelines)) {
        if (is.null(apidata$wreloc)) {
            apidata$wreloc <- system.file(package = "tools", "wre.txt")
            if (apidata$wreloc == "")
                apidata$wreloc <-
                    "https://svn.r-project.org/R/trunk/doc/manual/R-exts.texi"
        }
        apidata$wrelines <- readLines(apidata$wreloc)
    }
    apidata$wrelines
}

unmap <- function(x) sub("^Rf_", "", gsub("^_|_$", "", trimws(x)))

getOneFunAPI <- function(apitype) {
    wrelines <- WRE()
    fpat <- sprintf("^@(%s)fun +", apitype)
    hpat <- sprintf("^@(%s)hdr +", apitype)
    funs <- sub(fpat, "", grep(fpat, wrelines, value = TRUE))
    hdrs <- sub(hpat, "", grep(hpat, wrelines, value = TRUE))
    wAPI <- data.frame(name = funs, loc = rep("WRE", length(names)))
    getHdrAPI <- function(hdr) {
        hfuns <- getFunsHdr(file.path(R.home("include"), hdr))
        data.frame(name = hfuns, loc = rep(hdr, length(hfuns)))
    }
    hAPI <- lapply(hdrs, getHdrAPI)
    val <- rbind(wAPI, do.call(rbind, hAPI))
    val$apitype <- rep(apitype, nrow(val))
    val$unmapped <-unmap(val$name)
    rownames(val) <- NULL
    val
}

getFunAPI <- function() {
    apitypes <- c("api", "eapi", "emb")
    val <- do.call(rbind, lapply(apitypes, getOneFunAPI))
    val <- unique(val)
    val <- by(val,
              list(val$unmapped),
              ## picks max WRE > api > eapi > emb
              ## picks first if unmapped and mapped are in WRE
              function(x) if (nrow(x) > 1) x[1, ] else x,
              simplify = FALSE)
    val <- do.call(rbind, val)
    val$unmapped <- NULL ## not needed in final output
    rownames(val) <- NULL
    val
}

funAPI <- function() {
    if (is.null(apidata$fapi))
        apidata$fapi <- getFunAPI()
    apidata$fapi
}

## getFunsHdr tries to get the functions declared in a header file
## without additional tools beyond cc -E. Using a proper
## header-parsing tool would be more accurate, but this seems adequate
## for now.
getFunsHdr <- function(fpath, lines) {
    if (missing(lines)) {
        lines <- readLines(fpath)
        name <- basename(fpath)
    }
    else name <- NULL

    ## NORET has to be handled before ccE since what it expands into varies
    lines <- ifelse(grepl("^#", lines),
                    lines,
                    gsub(r"{.*\s*NORET\s*}", " ", lines))

    lines <- lines[! grepl("^#\\s*error", lines)] ## for GraphicsDevice.h

    lines <- ccE(lines)
    lines <- dropBraces(lines)

    ## these could be incorporated into the regex
    lines <- gsub(r"{\s*(const|extern|long|unsigned)\s*}", "", lines)
    lines <- sub(r"{^\s*(\w*[(])}", "void \\1", lines)
    lines <- gsub(r"{\(\s*\*\s*(\w+)\s*\)}", "(\\1)", lines)

    ## original from SO: https://stackoverflow.com/questions/476173/regex-to-pull-out-c-function-prototype-declarations
    ## funcRegexp <- r"{^\s*(?:(?:inline|static)\s+){0,2}(?!else|typedef|return)\w+\s+\*?\s*(\w+)\s*\([^0]+\)\s*;?}"
    ## allow for parens around function name
    ## make closing paren for arguments optional
    funcRegexp <- r"{^\s*(?:(?:inline|static)\s+){0,2}(?!else|typedef|return)\w+\s*\*?\s*\(?(\w+)\)?\s*\([^0]+\)?\s*;?}"

    m <- gregexec(funcRegexp, lines, perl = TRUE)
    v <- regmatches(lines, m)
    val <- sapply(v[lengths(v) > 0], `[[`, 2)
    val <- unique(as.character(val))

    ## drop halucinations
    val <- val[! (val %in% letters | val %in% LETTERS)]
    val <- val[! grepl("_t$", val)]
    val <- val[! grepl("user_(unif|norm)", val)]
    val <- val[! grepl("Quartz|Win32", val)]

    val
}

ccE <- function(lines, include = R.home("include"), clean = TRUE) {
    if (Sys.which("cc") == "")
        stop("'cc' is not on the path")
    tfile <- tempfile(fileext = ".h")
    on.exit(unlink(tfile))
    writeLines(lines, tfile)
    cmd <- sprintf("cc -E -I%s %s", include, tfile)
    val <- system(cmd, intern=TRUE)
    if (clean)
        ccEclean(val, tfile)
    else val
}

ccEclean <- function(lines, pattern = "Rtmp") {
    fline <- grepl("^#", lines)
    keep <- grepl(pattern, lines[fline])
    len <- diff(c(which(fline), length(lines) + 1))
    keep <- unlist(mapply(rep, keep, len, USE.NAMES = FALSE))
    lines <- lines[keep & ! fline]
    lines
}

dropBraces <- function(lines) {
    ## drop {...} fully within a line
    lines <- sub("[{].*[}]", " ", lines)

    ## drop {...} crossing several lines
    start <- grepl("[{]", lines)
    end <- grepl("[}]", lines)
    ## could check for balance
    lines <- lines[cumsum(start - end) == 0 | start | end]
    lines <- sub("[{].*", "", lines)      ## keep stuff before {
    lines <- lines[! grepl(".*[}]", lines)] ## don't keep stuff after }

    lines
}


##
## Check a shared library's use of R entry points
##

checkLibAPI <- function(lpath) {
    ldata <- readFileSyms(lpath)
    lsyms <- ldata[ldata$type == "U", ]$name
    lsyms <- inRfuns(lsyms)
    lsyms <- data.frame(name = lsyms, unmapped = unmap(lsyms))
    api <- funAPI()
    api$unmapped <- unmap(api$name)
    api$name <- NULL
    api$loc <- NULL
    val <- merge(lsyms, api, all.x = TRUE)
    val <- val[order(val$apitype), ]
    val$unmapped <- NULL ## not needed in final output
    rownames(val) <- NULL
    val
}

readFileSyms <- function(fpath) {
    ## this uses nm
    ## could try objdump if nm doesn't work
    v <- read_symbols_from_object_file(fpath)
    if (is.null(v))
        data.frame(name = character(0), type = character(0))
    else as.data.frame(v)[c("name", "type")]
}

## crude approach based on string matching
## **** this is to crude -- needs to allow more
inRfunsCrude <- function(syms) {
    syms <- union(syms[syms == toupper(syms)],
                  grep("^_?Rf?_", syms, value = TRUE))
    pat <- "R_MB_CUR_MAX|R_BaseNamespace|R_BlankScalarString|R_BlankString"
    pat <- sprintf("%s|R_CStackDir|R_CStackLimit|R_CStackStart", pat)
    pat <- sprintf("%s|R_Consolefile|R_CurrentExpression|R_Interactive", pat)
    pat <- sprintf("%s|R_Outputfile|R_Srcref|R_TempDir", pat)
    pat <- sprintf("%s|R_compact_.*_class|R_ignore_SIGPIPE", pat)
    pat <- sprintf("%s|R_interrupts_pending|R_interrupts_suspended", pat)
    pat <- sprintf("%s|R_isForkedChild", pat)
    pat <- sprintf("%s|R_NilValue|R_MissingArg|R_Visible", pat)
    pat <- sprintf("%s|R_.*Symbol$|R_dot_|R_Na", pat)
    pat <- sprintf("%s|R_NilValue|R_GlobalEnv|R_BaseEnv|R_EmptyEnv", pat)
    pat <- sprintf("%s|R_(Pos|Neg)Inf|R_.*Value$|R_.*Handlers$", pat)
    syms[! grepl(pat, syms)]
}

## approach based on computing the entry points in the executable and core libs
## fall back to the crude approach if entry points can't be found
inRfuns <- function(syms) {
    rfuns <- Rfuns()
    if (length(rfuns) == 0)
        inRfunsCrude(syms)
    else
        syms[unmap(syms) %in% unmap(rfuns)]
}

cleanRfuns <- function(val) {
    ## if Rf_XLENGTH and XLENGTH are both there then keep Rf_XLENGTH
    if (any(grepl("^_*Rf_XLENGTH_*$", val)) &&
        any(grepl("^_*XLENGTH_*$", val)))
        val <- val[! grepl("^_*XLENGTH_*$", val)]
    
    ## drop tre_ stuff if it is there and some others
    val[! grepl("tre_|^_*(main|MAIN|start)_*$|yyparse", val)]
}

getRfuns <- function() {
    pat <- sprintf("(\\.dylib|%s)$", .Platform$dynlib.ext)
    ofiles <- c(file.path(R.home("bin"), "exec", "R"),
                dir(R.home("lib"), pattern = pat, full.names = TRUE),
                dir(R.home("modules"), pattern = pat, full.names = TRUE))
    data <- do.call(rbind, lapply(ofiles, readFileSyms))
    fdata <- data[data$type == "T", ]
    cleanRfuns(fdata$name)
}

Rfuns <- function() {
    if (is.null(apidata$rfuns))
        apidata$rfuns <- getRfuns()
    apidata$rfuns
}


##
## Check an installed package's use of R entry points
##

checkPkgAPI <- function(pkg, lib.loc = NULL, all = FALSE) {
    libdir <- system.file("libs", package = pkg, lib.loc = lib.loc)
    libs <- Sys.glob(file.path(libdir, sprintf("*%s", .Platform$dynlib.ext)))
    if (length(libs) > 0) {
        val <- do.call(rbind, lapply(libs, checkLibAPI))
        if (! all)
            val <- val[is.na(val$apitype), ]
        val <- unique(val)
        rownames(val) <- NULL
        val
    }
    else NULL
}

checkAllPkgsAPI <- function(lib.loc = NULL, priority = NULL, all = FALSE,
                            Ncpus = getOption("Ncpus", 1L),
                            verbose = getOption("verbose")) {
    p <- rownames(utils::installed.packages(lib.loc = lib.loc,
                                            priority = priority))
    checkOne <- function(pkg) {
        data <- checkPkgAPI(pkg, lib.loc = lib.loc, all = all)
        if (! is.null(data))
            data$pkg <- rep(pkg, nrow(data))
        data
    }
    val <- do.call(rbind, .package_apply(p, checkOne,
                                         Ncpus = Ncpus, verbose = verbose))
    rownames(val) <- NULL
    val
}


##
## Find R entry points and variables used in installed packages
##


clear_rownames <- function(val) {
    rownames(val) <- NULL
    val
}

rbind_list <- function(args)
    clear_rownames(do.call(rbind, args))

ofile_syms <- function(fname, keep = c("F", "V", "U")) {
    ## this uses nm on Linux/macOS; probably doesn't work on Windows, so bail
    stopifnot(isFALSE(.Platform$OS.type == "windows"))
    v <- read_symbols_from_object_file(fname)
    if (is.character(v) && nrow(v) == 0) 
        ofile_syms_od(fname, keep)
    else if (is.null(v))
        data.frame(name = character(0), type = character(0))
    else {
        match_type <-function(type)
            ifelse(type == "T", "F", ifelse(type == "U", "U", "V"))
        val <- as.data.frame(v)[c("name", "type")]
        val <- val[val$type %in% c("U", "B", "D", "T"), ]
        val$type <- match_type(val$type)
        val <- val[val$type %in% keep, ]
        val
    }    
}

ofile_syms_od <- function(fpath, keep = c("F", "V", "U")) {
    if (Sys.which("objdump") == "")
        stop("'objdump' is not on the path")
    v <- system(sprintf("objdump -T %s", fpath), intern = TRUE)
    v <- grep("\t", v, value = TRUE)      ## data lines contain a \t
    name <- sub(".*\t.* (.*$)", "\\1", v) ## the name is at the end after the \t
    type <- sub(".* (.*)\t.*", "\\1", v)  ## the type is right before the \t
    ttbl <-
        c("*UND*" = "U", ".text" = "F", ".bss" = "V", ".data" = "V", w = "w")
    val <- data.frame(name, type = ttbl[match(type, names(ttbl), length(ttbl))])
    val <- val[val$type %in% keep, ]
    clear_rownames(val[order(val$name), ])
}

Rsyms <- function(keep = c("F", "V")) {
    rsyms <- apidata$rsyms
    if (is.null(rsyms)) {
        ofiles <- c(file.path(R.home("bin"), "exec", "R"),
                    dir(R.home("lib"), full.names = TRUE),
                    dir(R.home("modules"), full.names = TRUE))
        rsyms <- rbind_list(lapply(ofiles, ofile_syms, keep))
        apidata$rsyms <- rsyms
    }
    rsyms
}

pkgRsyms <- function(pkg, lib.loc = NULL) {
    libdir <- system.file("libs", package = pkg, lib.loc = lib.loc)
    libs <- Sys.glob(file.path(libdir, "*.so"))
    if (length(libs) > 0) {
        val <- rbind_list(lapply(libs, ofile_syms, keep = "U"))
        val$package <- rep(pkg, nrow(val))
        val$type <- NULL
        merge(val, Rsyms())
    }
    else NULL
}

allPkgsRsyms <- function(lib.loc = NULL,
                           Ncpus = getOption("Ncpus", 1L),
                           verbose = getOption("verbose")) {
    p <- rownames(utils::installed.packages(lib.loc = lib.loc))
    rbind_list(.package_apply(p, pkgRsyms, Ncpus = Ncpus, verbose = verbose))
}

