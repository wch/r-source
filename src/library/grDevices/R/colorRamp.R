
colorRampPalette<-function(colors,...) {

  ramp<-colorRamp(colors,...)
  function(n) {
    x<-ramp(seq(0,1,length=n))
    rgb(x[1,],x[2,],x[3,], max=255)
  }
  
}

colorRamp<-function(colors, bias=1, space=c("rgb","Lab"),
                    interpolate=c("linear","spline"))
{

  if (bias<=0) stop("Bias must be positive")
  colors<-col2rgb(colors)/255
  space<-match.arg(space)
  interpolate<-match.arg(interpolate)
  
  if (space=="Lab"){
    colors<-convertColor(colors, from="sRGB", to="Lab")
    #apply(colors,2,srgb2lab)
  }

  colors<-t(colors)
  
  interpolate<-switch(interpolate, linear=approxfun, spline=splinefun)

  x<-seq(0,1,length=nrow(colors))^{bias}
  
  palette<-c(interpolate(x,colors[,1]),
             interpolate(x,colors[,2]),
             interpolate(x,colors[,3]))
  
  roundcolor<-function(rgb){
    rgb[rgb<0]<-0
    rgb[rgb>1]<-1
    rgb
  }
  
  if (space=="Lab"){

    function(x) {
      roundcolor(convertColor(rbind(palette[[1]](x),
                             palette[[2]](x),
                             palette[[3]](x)),from="Lab",to="sRGB"))*255
    }
    
  } else {

    function(x) {
      roundcolor(rbind(palette[[1]](x),
                       palette[[2]](x),
                       palette[[3]](x)))*255
    }
    
  }

}
