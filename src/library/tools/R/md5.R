md5sum <- function(files)
    structure(.Call("Rmd5", files), names=files)
