browse.wspace <- function(){
 objlist <- ls(sys.frame())
 n <- length(objlist)
 if(n==0){
  warning("Empty workspace, nothing to do!")
  return;
 }

 N <- 0
 M <- n
 IDS <- rep(NA,n)
 NAMES <- rep(NA,n)
 TYPES <- rep(NA,n)
 DIMS <- rep(NA,n)

 IsRoot <- rep(TRUE,n)
 Container <- rep(FALSE,n)
 ItemsPerContainer <- rep(0,n)
 
 for( object in objlist ){
  N <- N+1
  obj    <- get(object) 
  md     <- mode(obj)
  objdim <- dim(obj)
  if(length(objdim) == 0)
   dim.field <- paste("length:",length(obj))
  else{
     dim.field <- "dim:"    
     for(i in 1:length(objdim))
       dim.field <- paste(dim.field,objdim[i]) 
     if(is.matrix(obj))
      md <- "matrix"  
    }
  obj.class <- attr(obj,"class")
  if(length(obj.class)!=0){
    md <- obj.class[1]
    if(obj.class[1] == "factor")
       dim.field <- paste("levels:",length(levels(obj)))     
  }
  #cat("objname:",object,", type=",md,",",dim.field,"\n")

  IDS[N] <- N
  NAMES[N] <- object
  TYPES[N] <- md
  DIMS[N] <- dim.field

  if(md == "list"){
   lg <- length(obj)
   Container[N] <- TRUE
   ItemsPerContainer[N] <- lg
   nm <- names(obj)
   for(i in 1:lg){
    M <- M+1
    if(nm[i] == "") nm[i] = paste("[[",i,"]]",sep="") 
    md.l  <- mode(obj[[i]])
    objdim.l <- dim(obj[[i]])
    if(length(objdim.l) == 0)
     dim.field.l <- paste("length:",length(obj[[i]]))
    else{
     dim.field.l <- "dim:"    
      for(j in 1:length(objdim.l))
       dim.field.l <- paste(dim.field.l,objdim.l[i]) 
    }

    obj.at <- attr(obj[[i]],"class")
     if(length(obj.at)!=0)
      if(obj.at == "factor"){
       md.l <- obj.at
       dim.field.l <- paste("levels:",length(levels(obj[[i]])))
      }

    #cat("    objname:",nm[i],", type=",md.l,",",dim.field.l,"\n")
    IDS <- c(IDS,M)
    NAMES <- c(NAMES, nm[i])
    TYPES <- c(TYPES, md.l)
    DIMS <- c(DIMS,dim.field.l)
   } 
  } # "list"
   
  obj.class <- attr(obj,"class")
  if(length(obj.class)!=0){
   if( obj.class == "table"){
    obj.nms <- attr(obj,"dimnames")
    lg <- length(obj.nms)
    if(length(names(obj.nms)) >0)
     nm <- names(obj.nms)
    else
     nm <- rep("",lg)
    Container[N] <- TRUE
    ItemsPerContainer[N] <- lg
    for(i in 1:lg){
     M <- M+1
     if(nm[i] == "") nm[i] = paste("[[",i,"]]",sep="") 
     md.l  <- mode(obj.nms[[i]])
     objdim.l <- dim(obj.nms[[i]])
     if(length(objdim.l) == 0)
      dim.field.l <- paste("length:",length(obj.nms[[i]]))
     else{
      dim.field.l <- "dim:"    
       for(j in 1:length(objdim.l))
        dim.field.l <- paste(dim.field.l,objdim.l[i]) 
     }
     #cat("    objname:",nm[i],", type=",md.l,",",dim.field.l,"\n")
    IDS <- c(IDS,M)
    NAMES <- c(NAMES, nm[i])
    TYPES <- c(TYPES, md.l)
    DIMS <- c(DIMS,dim.field.l)
    } 
   } # "table"
  
   if( obj.class == "data.frame"){
    nm <- dimnames(obj)[[2]]
    lg <- length(nm)
    Container[N] <- TRUE
    ItemsPerContainer[N] <- lg
    for(i in 1:lg){
     M <- M +1
     md.l  <- mode(obj[[i]])
     dim.field.l <- paste("length:",length(obj[[i]]))

     obj.at <- attr(obj[[i]],"class")
     if(length(obj.at)!=0)
      if(obj.at == "factor"){
       md.l <- obj.at
       dim.field.l <- paste("levels:",length(levels(obj[[i]])))
      }
     #cat("    varname:",nm[i],", type=",md.l,",",dim.field.l,"\n")
     IDS <- c(IDS,M)
     NAMES <- c(NAMES, nm[i])
     TYPES <- c(TYPES, md.l)
     DIMS <- c(DIMS,dim.field.l)
    } 
   } # "data.frame"

  if( obj.class == "mts"){
    nm <- dimnames(obj)[[2]]
    lg <- length(nm)
    Container[N] <- TRUE
    ItemsPerContainer[N] <- lg
    for(i in 1:lg){
     M <- M+1
     md.l  <- mode(obj[[i]])
     dim.field.l <- paste("length:",dim(obj)[1])
     md.l <- "ts"
     #cat("    tseries:",nm[i],", type=",md.l,",",dim.field.l,"\n")
    IDS <- c(IDS,M)
    NAMES <- c(NAMES, nm[i])
    TYPES <- c(TYPES, md.l)
    DIMS <- c(DIMS,dim.field.l)
    } 
   } # "mts"
 
   if( obj.class[1] == "lm" | obj.class[1] == "glm"){
    nm <- names(obj)
    lg <- length(nm)
    Container[N] <- TRUE
    ItemsPerContainer[N] <- lg
    for(i in 1:lg){
     M <- M+1
     md.l  <- mode(obj[[i]])
     dim.field.l <- paste("length:",length(obj[[i]]))
#     md.l <- "ts"

     #cat("    name:",nm[i],", type=",md.l,",",dim.field.l,"\n")
     IDS <- c(IDS,M)
     NAMES <- c(NAMES, nm[i])
     TYPES <- c(TYPES, md.l)
     DIMS <- c(DIMS,dim.field.l)
    } 
   } # "lm"

 } # "length"
 
 } # "for"

 Container <- c(Container, rep(FALSE,M-N))
 IsRoot <- c(IsRoot, rep(FALSE,M-N))
 ItemsPerContainer <- c(ItemsPerContainer, rep(0,M-N))
 .Internal(wsbrowser(as.integer(IDS),IsRoot,Container,as.integer(ItemsPerContainer),NAMES,TYPES,DIMS))
}


