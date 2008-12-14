#  File src/library/grDevices/R/colorRamp.R
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


colorRampPalette<-function(colors,...)
{
    ramp<-colorRamp(colors,...)
    function(n) {
        x <- ramp(seq.int(0, 1, length.out=n))
        rgb(x[,1], x[,2], x[,3], maxColorValue=255)
    }

}

colorRamp<-function(colors, bias=1, space=c("rgb","Lab"),
                    interpolate=c("linear","spline"))
{
    if (bias<=0) stop("'bias' must be positive")
    colors<-t(col2rgb(colors)/255)
    space<-match.arg(space)
    interpolate<-match.arg(interpolate)

    if (space=="Lab"){
        colors<-convertColor(colors, from="sRGB", to="Lab")
                                        #apply(colors,2,srgb2lab)
    }


    interpolate<-switch(interpolate, linear=stats::approxfun, spline=stats::splinefun)

    x<-seq.int(0, 1, length.out=nrow(colors))^{bias}

    palette<-c(interpolate(x, colors[,1]),
               interpolate(x, colors[,2]),
               interpolate(x, colors[,3]))

    roundcolor<-function(rgb){
        rgb[rgb<0]<-0
        rgb[rgb>1]<-1
        rgb
    }

    if (space=="Lab"){

        function(x) {
            roundcolor(convertColor(cbind(palette[[1L]](x),
                                          palette[[2L]](x),
                                          palette[[3L]](x)),from="Lab",to="sRGB"))*255
        }

    } else {

        function(x) {
            roundcolor(cbind(palette[[1L]](x),
                             palette[[2L]](x),
                             palette[[3L]](x)))*255
        }

    }

}
