shQuote <- function(string, type = c("sh", "csh", "cmd"))
{
    cshquote <- function(x) {
        xx <- strsplit(x, "'", fixed = TRUE)[[1]]
        paste(paste("'", xx, "'", sep = ""), collapse="\"'\"")
    }
    if(missing(type) && .Platform$OS.type == "windows") type <- "cmd"
    type <- match.arg(type)
    if(type == "cmd") {
        paste('"', gsub('"', '\\\\"', string), '"', sep = "")
    } else {
        if(!length(string)) return('')
        has_single_quote <- grep("'", string)
        if(!length(has_single_quote))
            return(paste("'", string, "'", sep = ""))
        if(type == "sh")
            paste('"', gsub('(["$`\\])', "\\\\\\1", string), '"', sep="")
        else {
            if(!length(grep('([$`])', string))) {
                paste('"', gsub('(["!\\])', "\\\\\\1", string), '"', sep="")
            } else sapply(string, cshquote)
        }
    }
}
