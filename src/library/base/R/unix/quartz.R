quartz <- function (display = "", width = 5, height = 5, pointsize = 12, 
                    family="Helvetica", antialias = TRUE, autorefresh = TRUE){
  if (.Platform$GUI=="AQUA")
    .Internal(Quartz(display, width, height, pointsize,family, antialias,autorefresh))
  else
    stop("quartz() currently works only under RAqua")
}
