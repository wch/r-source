getNumCConverters <-
function() {
 .Internal(getNumRtoCConverters())
}

getCConverterDescriptions <-
function() {
 .Internal(getRtoCConverterDescriptions())
}

getCConverterStatus <-
function() {
 v <- .Internal(getRtoCConverterStatus())
 names(v) <- getCConverterDescriptions()

 v
}


setCConverterStatus <-
function(id, status)
{
  .Internal(setToCConverterActiveStatus(id, as.logical(status)))
}

removeCConverter <-
function(id)
{
  .Internal(removeToCConverterActiveStatus(id))
}

