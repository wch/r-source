texi2dvi <- function(file, pdf = FALSE, clean = TRUE,
                     quiet = TRUE, texi2dvi = getOption("texi2dvi")) 
{
    if(pdf) pdf <- "--pdf" else pdf <- ""
    if(clean) clean <- "--clean" else clean <- ""
    if(quiet) quiet <- "--quiet" else quiet <- ""
    if(is.null(texi2dvi)) {
        if(file.exists(file.path(R.home(), "bin", "texi2dvi")))
            texi2dvi <- file.path(R.home(), "bin", "texi2dvi")
        else
            texi2dvi <- "texi2dvi"
    }
    
    yy <- system(paste(texi2dvi, quiet, pdf, clean, file))
    if(yy > 0) stop(paste("running texi2dvi on", file, "failed"))
}

