shQuote <- function(string, type = "sh") {
    if(type != "sh")
        stop("currently only type = \"sh\" is supported")
    paste('"', gsub('(["$`\\])', "\\\\\\1", string), '"', sep="")
}
