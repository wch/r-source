polygon <-
function(x, y=NULL, density = -1, angle = 45, border=par("fg"), ...)
{
  if (!missing(density))
    .NotYetUsed("density")
  if (!missing(angle))
    .NotYetUsed("angle")
  xy <- xy.coords(x, y)
  .Internal(polygon(xy$x, xy$y, border=border, ...))
}
