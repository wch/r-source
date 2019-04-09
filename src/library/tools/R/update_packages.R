#  File src/library/tools/R/update_packages.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2017 The R Core Team
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

## Code in this file adapted by Gabriel Becker from
## code distributed as part of the switchr R package.
## The modifications in this file and copyright thereof are
## donated without restriction to the R project.
##
## Original code and the switchr R package are
## Copyright 2018 Genentech Inc. All Rights Reserved.
## Author: Gabriel Becker <gabembecker@gmail.com>
## Distributed under the Artistic 2.0 License
## (re-licensed here to GPL 2+)

## canonical field order, from calling available.packages
## on CRAN repository (same result for Bioconductor)
fieldorder = c("Package", "Version", "Priority", "Depends",
               "Imports", "LinkingTo", "Suggests", "Enhances",
               "License", "License_is_FOSS", "License_restricts_use",
               "OS_type", "Archs", "MD5sum", "NeedsCompilation",     
               "File", "Repository")         

update_PACKAGES <- function(dir = ".", fields = NULL,
                            type = c("source", "mac.binary", 
                                     "win.binary"),
                            verbose.level = as.integer(dryrun),
                            latestOnly = TRUE,
                            addFiles = FALSE,
                            rds_compress = "xz",
                            strict = TRUE,
                            dryrun = FALSE)
{
    if(!is.integer(verbose.level))
        verbose.level = as.integer(verbose.level)
    type <- match.arg(type)
    stopifnot(verbose.level >= 0L && verbose.level <= 2L)
    PKGSfile <- file.path(dir, "PACKAGES")
    ## whether we will call write_PACKAGES directly/immediately
    calldown <- FALSE
    retdat <- NULL
    if(type == "win.binary" && strict) {
        warning("PACKAGES files do not include MD5 sums in the win.binary case",
                ", so strict checking is impossible. Calling down to write_PACKAGES ",
                "directly.")
        calldown <- TRUE
    } else if (!file.exists(PKGSfile)) {
        ## no PACKAGES file to update
        warning("No existing PACKAGES file found at ", PKGSfile)
        calldown <- TRUE
    } else if (!all(dim(retdat <- as.data.frame(read.dcf(PKGSfile),
                                         stringsAsFactors = FALSE)) > 0L)) {
        ## retdat is populated in the if condition here.
        ## read without fields restriction, because reducing number
        ## of fields is ok, adding fields means we need reprocessing

        ##0 rows and/or 0 columns
        warning("Existing PACKAGES file contained no rows and/or no columns")
        calldown <- TRUE
    }
    okfields <- names(retdat)
    
    ## can't update PACKAGES file if existing entries don't have all
    ## the required fields
    if(!calldown && !is.null(fields) && !all(fields %in% okfields)) {
        warning("Specified fields no present in existing PACKAGES file: ",
                paste(setdiff(fields, okfields), collapse = " "))
        calldown <- TRUE
    }
    
    ## call straight down to write_PACKAGES if:
    ## 1. type is win.binary and strict is TRUE (no MD5 sums to check against,
    ##    only way to get full strictness is write_PACKAGES
    ## 2. no PACKAGES file already exists or it's empty
    ## 3. 1+ specified field not present in existing PACKAGES file
    if(calldown) {
        if(verbose.level > 0L)
            message("Unable to update existing PACKAGES file. Calling write_PACKAGES directly.")
        return(write_PACKAGES(dir = dir, fields = fields, type = type,
                              verbose = verbose.level == 2,
                              latestOnly = latestOnly,
                              addFiles = addFiles, rds_compress = rds_compress))
    }
    ## we know file exists by this point
    pmtime <- file.info(PKGSfile)$mtime
    if(verbose.level > 0L) {
        message("Updating existing repository [strict mode: ",
                if(strict) "ON" else "OFF",
                "]\nDetected PACKAGES file with ", nrow(retdat),
                " entries at ", PKGSfile)
    }
    
    if(!is.null(fields))
        retdat <- retdat[, fields]
    
    pkgfiles <- list.files(dir, pattern = .get_pkg_file_pattern(type),
                           full.names = TRUE)
    if(length(pkgfiles) == 0L)
        stop("unable to find any package tarballs in ", dir)
    
    if(is.null(retdat$File)) {
        tbmatches <- match(paste(retdat$Package,
                                 retdat$Version,
                                 sep = "_"),
                           ## above doesn't have the extensions
                           gsub(.get_pkg_file_pattern(type, ext.only = TRUE),
                                "",
                                basename(pkgfiles)))
        ## this gets NAs for entries that don't have tarballs
        ## taken care of via keeprows below.
        retdat$tarball <- pkgfiles[tbmatches]
    } else
        retdat$tarball <- retdat$File

    ## for accounting purposes, removed before final write
    retdat$IsNew = FALSE

    ## detect and remove entries whose files have been deleted
    ## file.exists(NA_character_) returns FALSE, so this
    ## is ok without an explicit NA check
    keeprows <- file.exists(retdat$tarball)
    if(verbose.level > 0L) {
        msg <- paste("Tarballs found for", sum(keeprows), " of ",
                     nrow(retdat), "existing PACKAGES entries.")
        message(msg)
    }
    retdat <- retdat[keeprows,]

    ## check for tarballs that are too new
    ## remove entries which might appear to match them
    ## because the new tarball takes precedence.
    tbmtimes <- file.info(retdat$tarball)$mtime
    toonew <- which(tbmtimes > pmtime)
    if(length(toonew) > 0L) {
        if(verbose.level > 0L){
            msg <- paste(length(toonew), " tarball(s) matching existing entries are ",
                         "newer than PACKAGES file and must be reprocessed.")
            message(msg)
        }
        retdat <- retdat[-toonew, ]
    }
    
    ## If in strict mode we confirm that the MD5 sums match for
    ## tarballs which match pre-existing PACKAGES entries.
    ##
    ## Otherwise we skip this check for speed, assuming that
    ## any tarball we find is the one used to create the entry.
    ##
    ## Note: skipping the check can lead to a 'bad' repo in rare
    ## cases, but the installation machinery would still protect
    ## against non-malicious cases of this by failing out when the
    ##
    ## Note: MD5 sum didn't match what PACKAGES said it should be.
    ## In the win.binary case the existing PACKAGES file has no MD5
    ## sums, but we caught that above, so if strict is TRUE, we know
    ## type != win.binary.
    if(strict && NROW(retdat) > 0L) {
        if(verbose.level > 0L) {
            msg <- paste("[strict mode] Checking if MD5sums match ",
                         "for existing tarballs")
            message(msg)
        }
        curMD5sums <- md5sum(normalizePath(retdat$tarball))
        ## There are no NAs in retdat$MD5sum here, as the only data in
        ## there now is from the existing PACKAGES file.
        notokinds <- which(retdat$MD5sum != curMD5sums)
        if(length(notokinds) > 0L) {
            msg <- paste0("Detected ", length(notokinds), " MD5sum mismatches",
                          " between existing PACKAGES file and tarballs")
            warning(msg)
        } else if(verbose.level > 0L) {
            message("All existing entry MD5sums match tarballs.") 
        }
        ## tarballs that don't already ahve an entry
        ## OR that mismatched their existing entry
        ## possibly needing to be added
        if(length(notokinds) > 0L) {
            retdat <- retdat[-notokinds,]
        }
    }
    
    newpkgfiles <- setdiff(normalizePath(pkgfiles),
                           normalizePath(retdat$tarball))
    
    ## If we're willing to assume the filenames are honest and
    ## accurate, we can skip non-newest package versions without
    ## ever untaring them and reading their DESCRIPTION files.
    ##
    ## this is not the default because it is technically speaking
    ## less safe than what write_PACKAGES(,latestOnly=TRUE) does
    ## which is always process everything then prune.
    if(!strict &&
       latestOnly &&
       length(newpkgfiles) > 0L) {
        ##strip extension, left with pkgname_version
        newpkgtmp <- gsub(.get_pkg_file_pattern(type, ext.only = TRUE),
                          "",
                          basename(newpkgfiles))
        newpkgspl <- strsplit(basename(newpkgtmp), "_")
        newpkgdf <- do.call(rbind.data.frame,
                            c(newpkgspl, stringsAsFactors = FALSE))
        ## We create a dummy new repository db with only
        ## Package and Version columns, then fill them
        ## out with NAs so we can hit .remove_stale_dups
        ## before ever reading in the DESCRIPTION files
        ##
        ## These dummy db rows will all be replaced by the
        ## real data later in the process before the
        ## new PACKAGES files are written.
        
        newpkgdf <- newpkgdf[,1:2]
        names(newpkgdf) <- c("Package", "Version")
        newpkgdf <- .filldfcols(newpkgdf, retdat)
        ## for accounting purposes, taken back off later
        newpkgdf$IsNew <- TRUE
        newpkgdf$tarball <- newpkgfiles
        retdat <- rbind(retdat, newpkgdf) 
        ## remove non-latest ones now to avoid the expensive stuff
        ## this is non-strict because it assumes the package name and
        ## version in the filename are accurate. Technically, not
        ## guaranteed.
        retdat <- .remove_stale_dups(retdat)
        newpkgfiles <- retdat$tarball[retdat$IsNew]
    }
    
    ## Do any packages/package versions need to be added?
    numnew <- length(newpkgfiles)
    if(numnew > 0L) {
        if(verbose.level > 0L) {
            message("Found ", numnew, " package versions to process.")
        }
        
        ## returns a list of character vectors suitable for construction
        ## into a read.dcf output-style character matrix
        newpkgdat <- .process_package_files_for_repository_db(newpkgfiles,
                                                              type,
                                                              fields,
                                                              verbose.level > 1)
        newpkgdat <- .process_repository_package_db_to_matrix(newpkgdat,
                                                              path = "", #unused here
                                                              addFiles,
                                                              addPaths = FALSE, 
                                                              latestOnly)
        
        newpkgdf <- as.data.frame(newpkgdat, stringsAsFactors = FALSE)
           
        if(!identical(names(newpkgdf), names(retdat))) {
            ## make sure we catch columns only present in one or
            ## the other in both directions.
            ##
            ## the order of columns that comes out of this is columns
            ## in retdat (ie the original PACKAGES) in the order
            ## they appear there, THEN fields unique to the new tarballs
            ## appended in the order they appear there.
            newpkgdf <- .filldfcols(newpkgdf, retdat)
            retdat <- .filldfcols(retdat, newpkgdf)
        }
        
        if(verbose.level > 0L) {
            msg <- paste("Processed", nrow(newpkgdf), "entries from ",
                         "package tarballs.")
            message(msg)
        }
        
        ## just for accounting purposes
        ## taken back off later
        
        newpkgdf$IsNew <- TRUE
        retdat <- rbind(retdat[!retdat$IsNew,],
                        newpkgdf)
        if(latestOnly) {
            retdat <- .remove_stale_dups(retdat)
        }
        if(verbose.level > 0L) {
            msg <- paste(sum(retdat$IsNew), "entries added or updated, ",
                         sum(!retdat$IsNew), " entries retained unchanged.")
            message(msg)
        }
        
    } else if (verbose.level > 0L) {
        message("No new packages or updated package versions detected")
    }
    
    if(verbose.level > 0L) {
        msg <- paste("Final updated PACKAGES db contains ",
                     nrow(retdat), " entries.")
        message(msg)
    }

    ## write_PACKAGES docs don't define an order of entries, but I
    ## think it should(?) be sort order of files it processes. We
    ## reorder our db to give the same order.
    retdat <- retdat[order(paste0(retdat$Package, retdat$Version)),]

    ## clean up temp columns (note this works even if they aren't
    ## there so we don't need to worry about ones that are only
    ## defined within if blocks
    retdat$IsNew <- NULL
    retdat$tarball <- NULL

    ## guarantee canonical field order, with non-canonical fields
    ## appearing after
    noncanonfs <- setdiff(names(retdat), fieldorder)
    canonfs <- fieldorder[fieldorder %in% names(retdat)]
    retdat <- retdat[,c(canonfs, noncanonfs)]

    
    if(dryrun) {
        if(verbose.level > 0L)
            message("[dryrun mode] Dryrun complete.")
    } else {
        if(verbose.level > 0L)
            message("Writing final updated PACKAGES files.")
        ## crucial that db is written as a matrix
        ## otherwise available.packages, etc will fail
        db <- as.matrix(retdat)
        np <- .write_repository_package_db(db, dir, rds_compress)
        if(verbose.level > 0L)
            message("update_PACKAGES complete.")
    }
}


## pad df with columns from srcdf that it is missing,
## populated with NAs of the type appropriate for the
## column in srcdf. Must work for 0 row df or 0 row srcdf
##
## final col order: names(srcdf) followed by
## any columns unique to df
.filldfcols <- function(df, srcdf) {
    srcnames <- names(srcdf)
    dfnames <- names(df)
    newcols <- setdiff(srcnames, dfnames)
    df[,newcols] <- srcdf[integer(), newcols]
    df <- df[,unique(c(srcnames, dfnames))]
    df
}

