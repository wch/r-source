X11 <- function(display = "", width = 7, height = 7, pointsize = 12,
                gamma = 1, colortype = getOption("X11colortype"),
                maxcubesize = 256, bg = "transparent", canvas = "white",
                fonts = getOption("X11fonts"))
{

  if(display == "" && .Platform$GUI == "AQUA" && Sys.getenv("DISPLAY") == "") {
    Sys.putenv(DISPLAY = ":0")
  }

  .Internal(X11(display, width, height, pointsize, gamma, colortype,
                maxcubesize, bg, canvas, fonts))
}

x11 <- X11

gnome <- function(display = "", width = 7, height = 7, pointsize = 12) {
    .Defunct()
}

## no Gnome <- .Alias(gnome)
GNOME <- gnome
