formatUL <-
function(x, label = "*", offset = 0,
         width = 0.9 * getOption("width"))
{
    if(length(x) == 0)
        return(character())
    .format_rl_table(label, x, offset, width)
}

formatOL <-
function(x, type = "arabic", offset = 0, start = 1,
         width = 0.9 * getOption("width"))
{
    if(length(x) == 0)
        return(character())
    type_tokens <- c("1", "A", "a", "I", "i")
    type_full_names <- c("arabic", "Alph", "alph", "Roman", "roman")
    type <- match.arg(type, c(type_tokens, type_full_names))
    if(nchar(type) > 1)
        type <- type_tokens[match(type, type_full_names)]
    len <- length(x)
    labels <- seq.int(start[1], length = len)
    upper <- labels[len]
    if(type %in% c("A", "a")) {
        if(upper > 26)
            stop("too many list items (at most up to number 26)")
        labels <- if(type == "A")
            LETTERS[labels]
        else
            letters[labels]
    }
    else if(type %in% c("I", "i")) {
        if(upper > 3899)
            stop("too many list items (at most up to number 3899)")
        labels <- as.character(as.roman(labels))
        if(type == "i")
            labels <- tolower(labels)
    }
    .format_rl_table(sprintf("%s.", labels), x, offset, width)
}
         
.format_rl_table <-
function(labels, x, offset = 0, width = 0.9 * getOption("width"),
         sep = " ")
{
    ## Format a 2-column table with right-justified item labels and
    ## left-justified text.  Somewhat tricky because strwrap() eats up
    ## leading whitespace ...

    .make_empty_string <- function(n) {
        paste(rep.int(" ", n), collapse = "")
    }

    labels <- format(labels, justify = "right")
    len <- length(x)
    delta <- nchar(labels[1], "width") + offset
    x <- strwrap(x, width = width - delta - nchar(sep, "width"),
                 simplify = FALSE)
    nlines <- cumsum(sapply(x, length))
    prefix <- rep.int(.make_empty_string(delta), nlines[len])
    prefix[1 + c(0, nlines[-len])] <-
        paste(.make_empty_string(offset), labels, sep = "")
    paste(prefix, unlist(x), sep = sep)
}
