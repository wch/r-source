X11 <- function(display = "", width = 7, height = 7, pointsize = 12,
                gamma = 1, colortype = getOption("X11colortype"),
                maxcubesize = 256, canvas = "white",
                fonts = getOption("X11fonts"))
{

  if(display == "" && .Platform$GUI == "AQUA" && Sys.getenv("DISPLAY") == "") {
    Sys.putenv(DISPLAY = ":0")
  }

  .Internal(X11(display, width, height, pointsize, gamma, colortype,
                maxcubesize, canvas, fonts))
}

x11 <- X11

gnome <- function(display = "", width = 7, height = 7, pointsize = 12)
    .Internal(gnome(display, width, height, pointsize))

## no Gnome <- .Alias(gnome)
GNOME <- gnome
