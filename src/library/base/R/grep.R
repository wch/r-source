grep <-
function(pattern, x, ignore.case = FALSE, extended = TRUE, perl = FALSE,
         value = FALSE, fixed = FALSE)
{
  ## behaves like == for NA pattern
  if (is.na(pattern)){
    if(value)
      return(rep.int(as.character(NA), length(x)))
    else
      return(rep.int(NA, length(x)))
  }

  if(perl)
    .Internal(grep.perl(pattern, x, ignore.case, value))
  else
    .Internal(grep(pattern, x, ignore.case, extended, value, fixed))
}

sub <-
function(pattern, replacement, x, ignore.case = FALSE, extended = TRUE,
         perl = FALSE, fixed = FALSE)
{
    if (is.na(pattern))
        return(rep.int(as.character(NA), length(x)))

    if(perl)
        .Internal(sub.perl(pattern, replacement, x, ignore.case))
    else
        .Internal(sub(pattern, replacement, x, ignore.case,
                      extended, fixed))
}

gsub <-
function(pattern, replacement, x, ignore.case = FALSE, extended = TRUE,
         perl = FALSE, fixed = FALSE)
{
  if (is.na(pattern))
      return(rep.int(as.character(NA), length(x)))

  if(perl)
        .Internal(gsub.perl(pattern, replacement, x, ignore.case))
    else
        .Internal(gsub(pattern, replacement, x, ignore.case,
                       extended, fixed))
}

regexpr <-
function(pattern, text, extended = TRUE, perl = FALSE, fixed = FALSE)
{
    if(perl)
        .Internal(regexpr.perl(pattern, text))
    else
        .Internal(regexpr(pattern, text, extended, fixed))
}

agrep <-
function(pattern, x, ignore.case = FALSE, value = FALSE,
         max.distance = 0.1)
{
  ## behaves like == for NA pattern
   if (is.na(pattern)){
     if (value)
       return(rep.int(as.character(NA), length(x)))
     else
       return(rep.int(NA, length(x)))
   }

    if(!is.character(pattern)
       || (length(pattern) < 1)
       || ((n <- nchar(pattern)) == 0))
        stop("pattern must be a non-empty character string")

    if(!is.list(max.distance)) {
        if(!is.numeric(max.distance) || (max.distance < 0))
            stop("max.distance must be non-negative")
        if(max.distance < 1)            # transform percentages
            max.distance <- ceiling(n * max.distance)
        max.insertions <- max.deletions <- max.substitutions <-
            max.distance
    }
    else {
        ## partial matching
        table <- c("all", "deletions", "insertions", "substitutions")
        ind <- pmatch(names(max.distance), table)
        if(any(is.na(ind)))
            warning("unknown match distance components ignored")
        max.distance <- max.distance[!is.na(ind)]
        names(max.distance) <- table[ind]
        ## sanity checks
        comps <- unlist(max.distance)
        if(!all(is.numeric(comps)) || any(comps < 0))
            stop("max.distance components must be non-negative")
        ## extract restrictions
        if(is.null(max.distance$all))
            max.distance$all <- 0.1
        max.insertions <- max.deletions <- max.substitutions <-
            max.distance$all
        if(!is.null(max.distance$deletions))
            max.deletions <- max.distance$deletions
        if(!is.null(max.distance$insertions))
            max.insertions <- max.distance$insertions
        if(!is.null(max.distance$substitutions))
            max.substitutions <- max.distance$substitutions
        max.distance <- max.distance$all
        ## transform percentages
        if(max.distance < 1)
            max.distance <- ceiling(n * max.distance)
        if(max.deletions < 1)
            max.deletions <- ceiling(n * max.deletions)
        if(max.insertions < 1)
            max.insertions <- ceiling(n * max.insertions)
        if(max.substitutions < 1)
            max.substitutions <- ceiling(n * max.substitutions)
    }

    .Internal(agrep(pattern, x, ignore.case, value, max.distance,
                    max.deletions, max.insertions, max.substitutions))
}
