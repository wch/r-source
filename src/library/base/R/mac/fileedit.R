# Set of functions to open or create a new
# file in a editable window
# Nov 2001, Stefano M. Iacus

file.edit <-
    function(file){
         
    if( missing(file) ){
         new.file()
         return(invisible())
     }
         
     if(file.exists(file)) {
       .Internal(file.edit(file))
       return(invisible())
     } else {
         new.file(file)
         return(invisible())
     }

}


new.file <-
    function(file){
    
    if( missing(file) )
     file <- "NewFile"
   .Internal(new.file(file))
       
     return(invisible())
    
    
}
