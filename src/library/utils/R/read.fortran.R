#  File src/library/utils/R/read.fortran.R
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


read.fortran<-function(file, format, ...,as.is=TRUE, colClasses=NA){

     processFormat<-function(format){
       format<-toupper(format)
       template<-"^([0-9]*)([FXAI])([0-9]*)\\.?([0-9]*)"
       reps<-as.numeric(sub(template,"\\1",format))
       types<-sub(template, "\\2", format)
       lengths<-as.numeric(sub(template, "\\3", format))
       decimals<-as.numeric(sub(template, "\\4", format))

       reps[is.na(reps)]<-1
       lengths[is.na(lengths) & types=="X"]<-1

       charskip<-types=="X"
       lengths[charskip]<-reps[charskip]*lengths[charskip]
       reps[charskip]<-1

       if (any(is.na(lengths)))
         stop("missing lengths for some fields")

       lengths<-rep(lengths,reps)
       types<-rep(types,reps)
       decimals<-rep(decimals,reps)
       types<- match(types, c("F","D","X","A","I"))

       if (any(!is.na(decimals) & types>2))
         stop("invalid format")
       colClasses <- c("numeric", "numeric", NA,
                       if(as.is) "character" else NA, "integer")[types]
       colClasses <- colClasses[!(types==3)]
       decimals <-   decimals  [!(types==3)]
       lengths[types==3]<- -lengths[types==3]

       list(lengths,colClasses,decimals)
     }

     if(is.list(format)){
       ff<-lapply(format,processFormat)
       widths<-lapply(ff,"[[",1)
       if (is.na(colClasses))
         colClasses<-do.call("c",lapply(ff,"[[",2))
       decimals<-do.call("c",lapply(ff,"[[",3))
     } else {
       ff<-processFormat(format)
       widths<-ff[[1]]
       if (is.na(colClasses))
         colClasses<-ff[[2]]
       decimals<-ff[[3]]
     }

     rval<-read.fwf(file,widths=widths, ..., colClasses=colClasses)

     for(i in which(!is.na(decimals))){
       rval[,i]<-rval[,i]*(10^-decimals[i])
     }

     rval

}
