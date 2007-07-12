strwidth <- function(s, units="user", cex=NULL)
    .Internal(strwidth(as.graphicsAnnot(s),
                       pmatch(units, c("user", "figure", "inches")), cex))

strheight <- function(s, units="user", cex=NULL)
    .Internal(strheight(as.graphicsAnnot(s),
                        pmatch(units, c("user", "figure", "inches")), cex))

