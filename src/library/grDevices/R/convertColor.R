
## easyRGB scales Y=100 for white
## brucelindbloom uses XYZ in [0,1], so multiply by 100 to convert

## white points in xyY format (Y=1 omitted)
white.points<-list(A=c(x=0.44757, y=0.40745),
                   B=c(x=0.34842, y=0.35161),
                   C=c(x=0.31006, y=0.31616),
                   D50=c(x=0.34574, y=0.35867),
                   D55=c(x=0.33250, y=0.34761),
                   D65=c(x=0.3137,y=0.3291),
                   E=c(x=1/3,y=1/3))

## http://www.brucelindbloom.com/index.html?Equations.html


make.rgb<-function( red, green, blue, name=NULL,
                   white="D65",gamma=2.2){
    white.color<-white.points[[white]]
    whitexyz<- c(white.color[1]/white.color[2], 1,
              (1-sum(white.color))/white.color[2])
    
    red<-c(red[1]/red[2], 1, (1-sum(red))/red[2])
    green<-c(green[1]/green[2], 1, (1-sum(green))/green[2])
    blue<-c(blue[1]/blue[2], 1, (1-sum(blue))/blue[2])
    S<-drop(whitexyz%*%solve(rbind(red,green,blue)))
    M<-S*rbind(red, green, blue)
    
    if (is.numeric(gamma) && length(gamma)==1){
        dogamma<-function(x) x%^%gamma
        ungamma<-function(x) x%^%(1/gamma)
    } else if (gamma=="sRGB"){
        dogamma<-function(x) ifelse(x<0.04045,
                                    x/12.92,
                                    ((x+0.055)/1.055)^2.4)
        ungamma<-function(x) ifelse(x<=0.0031308,
                                    12.92*x,
                                    1.055*x%^%(1/2.4)-0.055)
    } else stop("gamma must be a scalar or 'sRGB'")

    toXYZ<-function(rgb,...){
        dogamma(rgb)%*%M
    }
    toRGB<-function(xyz,...){
        ungamma(xyz%*%solve(M))
    }
    if (is.null(name)) name<-deparse(sys.call())
    rval<-list( toXYZ=toXYZ, fromXYZ=toRGB, gamma=gamma,
               reference.white=white, name=name)
    class(rval)<-c("RGBcolorConverter","colorConverter")
    rval
}

print.colorConverter<-function(x,...){
    cat("Color space converter: ",x$name,"\n")
    if (!is.null(x$reference.white))
        cat("Reference white: ",x$reference.white,"\n")
    invisible(NULL)
}

print.RGBcolorConverter<-function(x,...){
    cat("Color space converter: ",x$name,"\n")
    if (!is.null(x$reference.white))
        cat("Reference white: ",x$reference.white,"\n")
    if (!is.null(x$gamma))
        cat("display gamma =",x$gamma,"\n")
    invisible(NULL)
}


chromaticAdaptation<-function(xyz,from,to){
    ## bradford scaling algorithm
    Ma<-matrix(c(0.40024,  -0.22630,   0.00000,
                 0.70760,   1.16532,   0.00000,
                 -0.08081,   0.04570,   0.91822),
               nrow=3,byrow=TRUE)
    from<-match.arg(from,names(white.points))
    to<-match.arg(to,names(white.points))
    from<-white.points[[from]]
    to<-white.points[[to]]
    from<-c(from[1]/from[2],1,(1-sum(from))/from[2])
    to<-c(to[1]/to[2],1,(1-sum(to))/to[2])
    from.cone<-drop(from%*%Ma)
    to.cone<-drop(to%*%Ma)
    M<-Ma%*%diag(to.cone/from.cone)%*%solve(Ma)
    xyz%*%M
}


colorConverter<-function(toXYZ,fromXYZ,name,white=NULL){
    rval<-list(toXYZ=toXYZ, fromXYZ=fromXYZ,
               name=name,white=white)
    class(rval)<-"colorConverter"
    rval
}

colorspaces<-list("XYZ"=colorConverter(toXYZ=function(x,w) x,
                  fromXYZ=function(x,w) x,
                  white=NULL,name="XYZ"),

                  "Apple RGB"=make.rgb(red=c(0.6250,0.3400),
                  green=c(0.2800,0.5950),
                  blue=c(0.1550,0.0700),gamma=1.8,
                  white="D65", name="Apple RGB"),

                  sRGB=make.rgb(red=c(0.6400, 0.3300),
                  green=c(0.3000,0.6000),
                  blue=c(0.1500,0.0600), gamma="sRGB",
                  white="D65", name="sRGB"),

                  "CIE RGB"=make.rgb(red=c(0.7350,0.2650),
                  green=c(0.2740,0.7170),
                  blue=c(0.1670,0.0090), gamma=2.2,
                  white="E", name="CIE RGB"),
                  
                  Lab=colorConverter(fromXYZ=function(XYZ, white){
                      epsilon <- 216/24389
                      kappa <- 24389/27
                      
                      xyzr<-XYZ/white
                      fxyz<-ifelse(xyzr<=epsilon, (kappa*xyzr+16)/116, xyzr^(1/3))
                      
                      c(L=116*fxyz[2]-16, a=500*(fxyz[1]-fxyz[2]), b=200*(fxyz[2]-fxyz[3]))
                  },
                  toXYZ=function(Lab,white){
                      
                      epsilon <- 216/24389
                      kappa <- 24389/27
                      
                      yr<-ifelse(Lab[1]<kappa*epsilon, Lab[1]/kappa, ((Lab[1]+16)/116)^3)
                      fy<-ifelse(yr<=epsilon, (kappa*yr+16)/116, (Lab[1]+16)/116)
                      fx<-Lab[2]/500+fy
                      fz<-fy-Lab[3]/200
                      
                      zr<-ifelse(fz^3<=epsilon, (116*fz-16)/kappa, fz^3)
                      xr<-ifelse(fx^3<=epsilon, (116*fx-16)/kappa, fx^3)
                      
                      c(X=xr,Y=yr,Z=zr)*white
                      
                  },name="Lab",white=NULL),

                  Luv=colorConverter(fromXYZ=function(XYZ, white){
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
                  },
                  toXYZ=function(Luv,white){
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
                  },
                  name="Luv",white=NULL),

                  )


"%^%"<-function(a,b){
  ifelse(a<=0, -abs(a)^b, a^b)
}


convertColor<-function(color, from, to,
                       from.ref.white=NULL,
                       to.ref.white=NULL,
                       scale.in=1,
                       scale.out=1,
                       clip=TRUE)
{
  if (is.character(from))
      from<-colorspaces[[match.arg(from,names(colorspaces))]]
  if (!inherits(from,"colorConverter"))
      stop("'from' must be a colorConverter or string")
  if (is.character(to))
      to<-colorspaces[[match.arg(to,names(colorspaces))]]
  if (!inherits(to,"colorConverter"))
      stop("'to' must be a colorConverter or string")

  ## Need a reference white. If both the definition and the argument
  ## specify one they must agree.
  
  if (is.null(from.ref.white))
      from.ref.white<-from$white
  else if (!is.null(from$white) && from.ref.white!=from$white)
      stop("'from.ref.white' disagrees with definition of ",from$name)

  if (is.null(to.ref.white))
      to.ref.white<-to$white
  else if (!is.null(to$white) && to.ref.white!=to$white)
      stop("'to.ref.white' disagrees with definition of ",to$name)

  if (is.null(to.ref.white) && is.null(from.ref.white))
      to.ref.white<-from.ref.white<-"D65"

  if (is.null(to.ref.white))
      to.ref.white<-from.ref.white
  if (is.null(from.ref.white))
      from.ref.white<-to.ref.white
  
  w<-white.points[[from.ref.white]]
  from.ref.white<-c(w[1]/w[2],1,(1-sum(w))/w[2])
  w<-white.points[[to.ref.white]]
  to.ref.white<-c(w[1]/w[2],1,(1-sum(w))/w[2])
  
  if (is.null(nrow(color)))
    color<-matrix(color,nrow=1)

  if (!is.null(scale.in))
      color<-color/scale.in

  trim<-function(rgb){
      rgb<-round(rgb,5)
      if (is.na(clip))
          rgb[rgb < 0 | rgb >1]<-NaN
      else if(clip){
          rgb[rgb < 0]<-0
          rgb[rgb>1]<-1
      }
      rgb
  }
  
  xyz<-apply(color, 1, from$toXYZ, from.ref.white)

  if (is.null(nrow(xyz)))
    xyz<-matrix(xyz, nrow=1)

  if (!is.all.equal(from.ref.white,to.ref.white)){
      mc<-match.call()
      if (is.null(mc$from.ref.white) || is.null(mc$to.ref.white))
          warning("Color spaces use different reference whites.")
      xyz<-chromaticAdaptation(xyz,from.ref.white,to.ref.white)
  }
  
  rval<-apply(xyz, 2, to$fromXYZ, to.ref.white)
  
  if (inherits(to,"RGBcolorConverter"))
      rval<-trim(rval)

  if (is.matrix(rval))
      rval<-t(rval)
  
  if (is.null(scale.out))
      rval
  else 
      rval*scale.out
  
}
