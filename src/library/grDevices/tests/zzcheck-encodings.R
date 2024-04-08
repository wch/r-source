### Optonally compare encodings results with the ref directories

if(is.na(Sys.getenv("R_GRDEVICES_COMPARE_PS_PDF", unset = NA))) q("no")
## mustl has a very different libiconv.
musl <- grepl("musl", R.version$os)
if(musl) q("no")

## This must follow encodings*.R

options(warn = 1)
library(tools)
files <- dir("ref", full.names = TRUE)
sys <- Sys.info()
## different reference output for macOS >= 14
## reference done with 14.2.
if(sys["sysname"] == "Darwin" &&
   as.numeric_version(sys["release"]) >= "23") {
    ind <- files %in% c("ref/PDF-encoding.pdf", "ref/PS-encoding.ps")
    files[ind] <- sub("^ref", "ref-macOS", files[ind])
}

if(.Platform$OS.type == "windows") {
    ind <- files %in% c("ref/PDF-encoding.pdf", "ref/PS-encoding.ps",
                        "ref/Encoding3.pdf")
    files[ind] <- sub("^ref", "ref-windows", files[ind])
}

cnt <- 0L
for(f in files) {
    ff <- basename(f)
    message("comparing ", sQuote(ff))
    res <- Rdiff(ff, f, useDiff = TRUE)
    if(res == 0) message("OK")
    else {
        message("Differences")
        cnt <- cnt + 1L
    }
}

if(cnt) stop("Differences found")
