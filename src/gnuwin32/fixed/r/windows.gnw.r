windows<- function(width = 7, height = 7, pointsize = 12)
    .Internal(X11("",width=width,height=height,pointsize=pointsize, 1))

win.graph<- function(width = 7, height = 7, pointsize = 12)
    .Internal(X11("",width=width,height=height,pointsize=pointsize, 1))

win.print<- function(width = 7, height = 7, pointsize = 12)
    .Internal(X11("win.print",width=width,height=height,pointsize=pointsize,1))

win.metafile <- function(filename="",width=7,height=7,pointsize=12)
    .Internal(X11(paste("win.metafile:",filename,sep=""),
                  width=width,height=height,pointsize=pointsize, 1))
