## we don't use white; it's for compatibility

parse <- function(file="", n=NULL, text=NULL, prompt=NULL, white=FALSE)
    .Internal(parse(file, n, text, prompt))
