
library(grid)

grid.rect(gp=gpar(gradientFill=linearGradient()))

grid.newpage()
grid.rect(width=.5, height=.5, gp=gpar(gradientFill=linearGradient()))

grid.newpage()
pushViewport(viewport(width=.5, height=.5))
grid.rect(gp=gpar(gradientFill=linearGradient()))

grid.newpage()
grid.rect(width=.5, height=.5, 
          gp=gpar(gradientFill=linearGradient(x1=.25, y1=.25, x2=.75, y2=.75)))

grid.newpage()
grid.rect(width=.5, height=.5, 
          gp=gpar(gradientFill=linearGradient(c("red", "white", "red"),
                                              c(0, .5, 1),
                                              x1=.25, y1=.25, x2=.75, y2=.75)))

