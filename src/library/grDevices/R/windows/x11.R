#  File src/library/grDevices/R/windows/x11.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

x11 <- X11 <-
     function(width, height, pointsize, bg, gamma, xpos, ypos, title)
{
    new <- list()
    if(!missing(width)) new$width <- as.double(width)
    if(!missing(height)) new$height <- as.double(height)
    if(!missing(pointsize)) new$pointsize <- as.double(pointsize)
    if(!missing(bg)) new$bg <- bg
    if(!missing(gamma)) new$gamma <- gamma
    if(!missing(xpos)) new$xpos <- as.integer(xpos)
    if(!missing(ypos)) new$ypos <- as.integer(ypos)
    if(!missing(title)) new$title <- title
    old <- check.options(new = new, envir = .WindowsEnv,
                         name.opt = ".Windows.Options",
			 reset = FALSE, assign.opt = FALSE)
    invisible(.External(Cdevga, "", old$width, old$height, old$pointsize,
                        FALSE, 1L, old$xpinch, old$ypinch, "white",
                        old$gamma, old$xpos, old$ypos, old$buffered,
                        .PSenv, old$bg, old$restoreConsole, old$title, TRUE,
                        old$fillOddEven))
}

