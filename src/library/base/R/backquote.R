

bquote<-function(expr, where=parent.frame()){

    
    unquote<-function(e){

        if (length(e)<=1)
            e
        else if (e[[1]]==as.name("."))
            eval(e[[2]],where)
        else
            as.call(lapply(e,unquote))
        
    }

    unquote(substitute(expr))

}

