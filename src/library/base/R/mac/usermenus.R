# Set of functions to create User menu commands
# Nov 2001, Stefano M. Iacus

add.menu.cmd <-
    function(label, command){
         
    if( missing(label) ){
        warning("please specify a label for the menu item")
         return(invisible())
    }
         
          
    if( missing(command) ){
        warning("please specify a command to associate to the menu item")
         return(invisible())
    }     
   
    return( .Internal(add.menu.cmd(label,command)) )


}


del.menu.cmd <-
    function(label){
         
    if( missing(label) ){
        warning("please specify a label for the menu item")
         return(invisible())
    }
         
          
         
    return( .Internal(del.menu.cmd(label)) )


}

get.menu.cmd <-
    function(label){
         
    if( missing(label) ){
        warning("please specify a label for the menu item")
         return(invisible())
    }
                  
         
    return( .Internal(get.menu.cmd(label)) )


}

del.num.cmd <-
    function(menunum){
         
    if( missing(menunum) ){
        warning("please specify a number for the menu item")
         return(invisible())
    }
         
          
         
    return( .Internal(del.num.cmd(as.integer(menunum))) )


}

get.num.cmd <-
    function(menunum){
         
    if( missing(menunum) ){
        warning("please specify a number for the menu item")
         return(invisible())
    }
                  
         
    return( .Internal(get.num.cmd(as.integer(menunum))) )


}


del.usr.cmd <-
    function(){
                  
    return( .Internal( del.usr.cmd() ) )


}


