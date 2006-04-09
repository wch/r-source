mat.or.vec <- function(nr,nc)
    if(nc==1) numeric(nr) else matrix(0,nr,nc)

## Use  'version' since that exists in all S dialects :
is.R <-
    function() exists("version") && !is.null(vl <- version$language) && vl == "R"

