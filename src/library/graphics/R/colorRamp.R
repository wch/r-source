
srgb2lab<-function(srgb){
  ## http://www.srgb.com/basicsofsrgb.htm
  lrgb<-ifelse(srgb<=0.04045, srgb/12.92, ((srgb+0.055)/1.055)^2.4)
  mat<-matrix(c(0.4124, 0.3576, 0.1805,
                0.2126, 0.7152, 0.0722,
                0.0193, 0.1192, 0.9505), nrow=3, byrow=TRUE)
  xyz<-(mat %*%lrgb)*100
  ##http://www.easyrgb.com/math.php?MATH=M8#text8
  ## vxyz<-xyz/c(95.047, 100, 108.883)
  ## but it makes greys look pink 
  vxyz<-xyz/100
  vxyz<-ifelse(vxyz>0.008856, vxyz^(1/3), vxyz*7.787 + 16/116)
  c(vxyz[1]*116-16, 500*(vxyz[1]-vxyz[2]), 200*(vxyz[2]-vxyz[3]))
  
}
                   
lab2srgb<-function(Lab){
  L<-Lab[1]
  a<-Lab[2]
  b<-Lab[3]
kappa<-24389/27
epsilon<-216/24389

yr<-ifelse(L>kappa*epsilon, ((L+16)/116)^3, L/kappa)


fy<-ifelse(yr>epsilon, (L+16)/116, (kappa*yr+16)/116)
fx<-(a/500)+fy
fz<-fy-b/200

zr<- ifelse(fz^3>epsilon, fz^3, (116*fz-16)/kappa)
xr<-ifelse(fx^3>epsilon, fx^3, (116*fx-16)/kappa)

xyz<-rbind(xr,yr,zr)

mat<-matrix(c(3.240479, -1.537150, -0.498535,
	     -0.969256, 1.875992, 0.041556,
	      0.055648, -0.204043, 1.057311),nrow=3, byrow=TRUE)


rgb<-mat%*%xyz
rgb[rgb<0]<-0
rgb[rgb>1]<-1

srgb<-ifelse(rgb<=0.0031308, 12.92*rgb, 1.055*rgb^(1/2.4)-0.055)

srgb
}

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
    colors<-apply(colors,2,srgb2lab)
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
      roundcolor(apply(cbind(palette[[1]](x),
                             palette[[2]](x),
                             palette[[3]](x)),1,lab2srgb))*255
    }
    
  } else {

    function(x) {
      roundcolor(rbind(palette[[1]](x),
                       palette[[2]](x),
                       palette[[3]](x)))*255
    }
    
  }

}
