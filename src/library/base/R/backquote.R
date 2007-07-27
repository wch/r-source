#  File src/library/base/R/backquote.R
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

