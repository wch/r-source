cm <- function(x) 2.54*x

xinch <- function(x=1) x * diff(par("usr")[1:2])/par("pin")[1]
yinch <- function(y=1) y * diff(par("usr")[3:4])/par("pin")[2]
xyinch <- function(xy=1)
 { u <- par("usr"); xy * c(u[2]-u[1], u[4]-u[3]) / par("pin") }
