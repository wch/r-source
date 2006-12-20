## Need to ensure this comes late enough ...
## Perhaps even merge it into the common profile?

.dynLibs <- local({
    ## <NOTE>
    ## Versions of R prior to 1.4.0 had .Dyn.libs in .AutoloadEnv
    ## (and did not always ensure getting it from there).
    ## Until 1.6.0, we consistently used the base environment.
    ## Now we have a dynamic variable instead.
    ## </NOTE>
    .Dyn.libs <- structure(list(), class = "DLLInfoList")
    function(new) {
        if(!missing(new)) {
            class(new) <- "DLLInfoList"
            .Dyn.libs <<- new
        }
        else
            .Dyn.libs
    }
})

.libPaths <- local({
    .lib.loc <- character(0)            # Profiles need to set this.
    function(new) {
        if(!missing(new)) {
            new <- Sys.glob(path.expand(new))
            paths <- unique(path.expand(c(new, .Library.site, .Library)))
            .lib.loc <<- paths[file.info(paths)$isdir %in% TRUE]
        }
        else
            .lib.loc
    }
})
