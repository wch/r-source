quartz <- function (display = "", width = 5, height = 5, pointsize = 12, 
                    family="Helvetica", antialias = TRUE, autorefresh = TRUE){
  if (.Platform$GUI != "AQUA")
   warning("quartz() device interactivity reduced without an event loop manager")

    .Internal(Quartz(display, width, height, pointsize,family, antialias,autorefresh))
}
