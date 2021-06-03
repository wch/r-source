library(grid)

## Set up
grid.newpage()
pushViewport(viewport(name="a.vp"))
grid.rect(name="a.grob")

## Default finds only grobs (no vpPath)
result <- grid.grep("a", grep=TRUE, global=TRUE)
result
attr(result[[1]], "vpPath")

## 'viewports = TRUE' (and 'vpPath = TRUE')
## finds grobs and viewports (AND vpPath on grobs)
result <- grid.grep("a", grep=TRUE, global=TRUE, viewports=TRUE)
result
attr(result[[2]], "vpPath")

## 'viewports = FALSE' and 'vpPath = TRUE'
## finds only grobs AND vpPath on grobs
result <- grid.grep("a", grep=TRUE, global=TRUE, vpPath=TRUE)
result
attr(result[[1]], "vpPath")

## 'viewports = TRUE' and 'vpPath = FALSE'
## finds viewports and grobs (no vpPath)
result <- grid.grep("a", grep=TRUE, global=TRUE, viewports=TRUE, vpPath=FALSE)
result
attr(result[[2]], "vpPath")

###########################
## global=FALSE versions

## grob (no vpPath)
result <- grid.grep("a", grep=TRUE)
result
attr(result, "vpPath")
## viewport (NULL vpPath)
result <- grid.grep("a", grep=TRUE, viewports=TRUE)
result
attr(result, "vpPath")
## grob with vpPath
result <- grid.grep("a", grep=TRUE, vpPath=TRUE)
result
attr(result, "vpPath")
## viewport (NULL vpPath)
result <- grid.grep("a", grep=TRUE, viewports=TRUE, vpPath=FALSE)
result
attr(result, "vpPath")
