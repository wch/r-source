
## easyRGB scales Y=100 for white
## brucelindbloom uses xyz in [0,1], so multiply by 100 to convert


white.points<-list(A=c(x=0.44757, y=0.40745),
                   B=c(x=0.34842, y=0.35161),
                   C=c(x=0.31006, y=0.31616),
                   D50=c(x=0.34574, y=0.35867),
                   D55=c(x=0.33250, y=0.34761),
                   D65=c(x=0.3137,y=0.3291),
                   E=c(x=1/3,y=1/3))

## http://www.brucelindbloom.com/index.html?Equations.html


rgb.matrix<-list(Adobe=matrix(
                   c(0.576700, 0.297361, 0.0270328,
                     0.185556,    0.627355,    0.0706879,  
                     0.188212,    0.0752847,   0.991248),3,byrow=TRUE),
                 Apple=matrix(
                   c(0.449695,    0.244634,    0.0251829,  
                     0.316251,    0.672034,    0.141184,   
                     0.18452,     0.0833318,   0.922602),3,byrow=TRUE),
                 CIE=matrix(
                   c(0.488718,    0.176204,    0.000000,   
                     0.310680,    0.812985,    0.0102048,  
                     0.200602,    0.0108109,  0.989795),3,byrow=TRUE),
                 sRGB=matrix(
                   c(0.412424,    0.212656,    0.0193324,  
                     0.357579,    0.715158,    0.119193,   
                     0.180464,    0.0721856,   0.950444),3,byrow=TRUE),
                 NTSC=matrix(
                   c(0.606734,    0.298839,    0.000000,   
                     0.173564,    0.586811,    0.0661196,  
                     0.200112,    0.114350,    1.11491 ),3,byrow=TRUE)
                 )



XYZtoLab<-function(XYZ, white){

  epsilon <- 216/24389
  kappa <- 24389/27

  xyzr<-XYZ/white
  fxyz<-ifelse(xyzr<=epsilon, (kappa*xyzr+16)/116, xyzr^(1/3))

  c(L=116*fxyz[2]-16, a=500*(fxyz[1]-fxyz[2]), b=200*(fxyz[2]-fxyz[3]))
}

XYZtoLuv<-function(XYZ, white){
  epsilon <- 216/24389
  kappa <- 24389/27

  yr<-XYZ[2]/white[2]

  denom<-sum(XYZ*c(1,15,3))
  wdenom<-sum(white*c(1,15,3))
             
  u1<- ifelse(denom==0,1,4*XYZ[1]/denom)
  v1<- ifelse(denom==0,1,9*XYZ[2]/denom)
  ur<-4*white[1]/wdenom
  vr<-9*white[2]/wdenom

  L<-ifelse(yr<=epsilon, kappa*yr, 116*(yr^(1/3))-16)
  c(L=L, u=13*L*(u1-ur), v=13*L*(v1-vr))
  
}

LabtoXYZ<-function(Lab,white){

  epsilon <- 216/24389
  kappa <- 24389/27

  yr<-ifelse(Lab[1]<kappa*epsilon, Lab[1]/kappa, ((Lab[1]+16)/116)^3)
  fy<-ifelse(yr<=epsilon, (kappa*yr+16)/116, (Lab[1]+16)/116)
  fx<-Lab[2]/500+fy
  fz<-fy-Lab[3]/200

  zr<-ifelse(fz^3<=epsilon, (116*fz-16)/kappa, fz^3)
  xr<-ifelse(fx^3<=epsilon, (116*fx-16)/kappa, fx^3)

  c(X=xr,Y=yr,Z=zr)*white
  
}

LuvtoXYZ<-function(Luv,white){
  epsilon <- 216/24389
  kappa <- 24389/27

  if(Luv[1]==0) return(c(0,0,0))

  u0<-4*white[1]/(white[1]+15*white[2]+3*white[3])
  v0<-9*white[2]/(white[1]+15*white[2]+3*white[3])

  Y<-ifelse(Luv[1]<=kappa*epsilon, Luv[1]/kappa, ((Luv[1]+16)/116)^3)
  a<-(52*Luv[1]/(Luv[2]+13*Luv[1]*u0)-1)/3
  b<- -5*Y
  c<- -1/3
  d<- Y*(39*Luv[1]/(Luv[3]+13*Luv[1]*v0)-5)
  
  X<-(d-b)/(a-c)
  Z<-X*a+b
  
  c(X=X,Y=Y,Z=Z)
}


"%^%"<-function(a,b){
  ifelse(a<=0, -abs(a)^b, a^b)
}

RGBtoXYZ<-function(RGB, gamma, m){
  rgb<-RGB%^%gamma
  XYZ<-rgb%*%m
  colnames(XYZ)<-c("X","Y","Z")
  XYZ
}


sRGBtoXYZ<-function(sRGB){
  rgb<-ifelse(sRGB<=0.04045, sRGB/12.92, ((sRGB+0.055)/1.055)^2.4)
  XYZ<-rgb%*%rgb.matrix$sRGB
  colnames(XYZ)<-c("X","Y","Z")
  XYZ
}

XYZtoRGB<-function(XYZ,gamma,m){
  rgb<-drop(XYZ)%*%solve(m)
  RGB<-rgb%^%(1/gamma)
  colnames(RGB)<-c("R","G","B")
  RGB
}

XYZtosRGB<-function(XYZ){
  rgb<-drop(XYZ)%*%solve(rgb.matrix$sRGB)
  sRGB<-ifelse(rgb<=0.0031308, 12.92*rgb, 1.055*rgb%^%(1/2.4)-0.055)
  colnames(sRGB)<-c("R","G","B")
  sRGB
}


convertColor<-function(color,
                      from=c("sRGB","hexsRGB","XYZ","Lab","Luv","AppleRGB","AdobeRGB","CIE.RGB","NTSC.RGB"),
                      to=c("sRGB","hexsRGB","XYZ","Lab","Luv","AppleRGB","AdobeRGB","CIE.RGB","NTSC.RGB"),
                      white.point=c("D65","A","B","C","D50","D55","E"), gamma=2.2, scale=1, clip=TRUE)
{

  from<-match.arg(from)
  to<-match.arg(to)
  if (any( c(from,to) %in% c("Lab","Luv"))){
    white.color<-white.points[[match.arg(white.point)]]
    white.point<-c(white.color[1]/white.color[2], 1, (1-sum(white.color))/white.color[2])
 }

  if (from=="hexsRGB"){
      scale<-256
      color<-t(col2rgb(color))
  }
  
  if (is.null(nrow(color)))
    color<-matrix(color,nrow=1)

  if (ncol(color)!=3 && nrow(color)==3) color=t(color)

  color<-color/scale

  trim<-function(rgb){
    if (is.na(clip))
      rgb[rgb < 0 | rgb >1]<-NaN
    else if(clip){
      rgb[rgb < 0]<-0
      rgb[rgb>1]<-1
    }
    rgb
  }
  
  xyz<-switch(from,
              sRGB=, hexsRGB=apply(color,1,sRGBtoXYZ),
              XYZ=t(color),
              Lab=apply(color, 1, LabtoXYZ, white=white.point),
              Luv=apply(color, 1, LuvtoXYZ, white=white.point),
              AppleRGB=apply(color, 1, RGBtoXYZ, m=rgb.matrix$Apple, gamma=gamma),
              AdobeRGB=apply(color, 1, RGBtoXYZ, m=rgb.matrix$Adobe, gamma=gamma),
              CIE.RGB=apply(color, 1, RGBtoXYZ, m=rgb.matrix$CIE, gamma=gamma),
              NTSC.RGB=apply(color, 1, RGBtoXYZ, m=rgb.matrix$NTSC, gamma=gamma))

  if (is.null(nrow(xyz)))
    xyz<-matrix(xyz, nrow=1)

  rval<-switch(to,
              sRGB=, hexsRGB=trim(apply(xyz,2,XYZtosRGB)),
              XYZ=xyz,
              Lab=apply(xyz, 2, XYZtoLab, white=white.point),
              Luv=apply(xyz, 2, XYZtoLuv, white=white.point),
              AppleRGB=trim(apply(xyz, 2, XYZtoRGB, m=rgb.matrix$Apple, gamma=gamma)),
              AdobeRGB=trim(apply(xyz, 2, XYZtoRGB, m=rgb.matrix$Adobe, gamma=gamma)),
              CIE.RGB=trim(apply(xyz, 2, XYZtoRGB, m=rgb.matrix$CIE, gamma=gamma)),
              NTSC.RGB=trim(apply(xyz, 2, XYZtoRGB, m=rgb.matrix$NTSC, gamma=gamma)))


  if (to=="hexsRGB")
      rgb(rval[,1],rval[,2],rval[,3])
  else 
      t(rval)
             
              
}
