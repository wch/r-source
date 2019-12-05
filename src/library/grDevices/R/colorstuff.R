#  File src/library/grDevices/R/colorstuff.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2019 The R Core Team
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
#  https://www.R-project.org/Licenses/

colours <- colors <- function(distinct = FALSE)
{
    c <- .Call(C_colors)
    if(distinct) c[!duplicated(t(col2rgb(c)))] else c
}

col2rgb <- function(col, alpha = FALSE)
{
    ## R-level trap for now.
    if(any(as.character(col) %in% "0"))
        stop("numerical color values must be positive", domain = NA)
    if (is.factor(col)) col <- as.character(col)
    .Call(C_col2rgb, col, alpha)
}

gray <- function(level, alpha)
    .Call(C_gray, level, if (missing(alpha)) NULL else alpha)
grey <- gray

rgb <- function(red, green, blue, alpha, names = NULL, maxColorValue = 1)
{
    ## Only 'red' given
    if(missing(green) && missing(blue)) {
	if(is.matrix(red) || is.data.frame(red)) {
	    red <- data.matrix(red)
	    if(ncol(red) < 3L) stop("at least 3 columns needed")
	    green <- red[,2L]; blue <- red[,3L]; red <- red[,1L]
	}
    }

    .Call(C_rgb, red, green, blue, if (missing(alpha)) NULL else alpha,
          maxColorValue, names)
}

hsv <- function(h = 1, s = 1, v = 1, alpha)
    .Call(C_hsv, h, s, v, if(missing(alpha)) NULL else alpha)

hcl <- function (h = 0, c = 35, l = 85, alpha, fixup = TRUE)
    .Call(C_hcl, h, c, l, if(missing(alpha)) NULL else alpha, fixup)


rgb2hsv <- function(r, g = NULL, b = NULL, maxColorValue = 255)
{
    rgb <- if(is.null(g) && is.null(b)) as.matrix(r) else rbind(r, g, b)
    if(!is.numeric(rgb)) stop("rgb matrix must be numeric")
    d <- dim(rgb)
    if(d[1L] != 3L) stop("rgb matrix must have 3 rows")
    n <- d[2L]
    if(n == 0L) return(cbind(c(h = 1, s = 1, v = 1))[, 0L])
    ## else:
    rgb <- rgb/maxColorValue
    if(any(0 > rgb) || any(rgb > 1))
        stop("rgb values must be in [0, maxColorValue]")

    .Call(C_RGB2hsv, rgb)
}

## A quick little ''rainbow'' function -- improved by MM
## doc in	../man/palettes.Rd
rainbow <- function (n, s = 1, v = 1, start = 0, end = max(1,n - 1)/n,
                     alpha, rev = FALSE)
{
    if ((n <- as.integer(n[1L])) > 0) {
	if(start == end || any(c(start,end) < 0)|| any(c(start,end) > 1))
	    stop("'start' and 'end' must be distinct and in [0, 1].")
	cols <- hsv(h = seq.int(start, (start > end)*1 + end,
				length.out = n) %% 1,
		    s, v, alpha)
        if(rev) rev(cols) else cols
    } else character()
}

topo.colors <- function (n, alpha, rev = FALSE)
{
    if ((n <- as.integer(n[1L])) > 0) {
	j <- n %/% 3
	k <- n %/% 3
	i <- n - j - k
	cols <- c(if(i > 0) hsv(h = seq.int(from = 43/60, to = 31/60,
                                            length.out = i), alpha = alpha),
                  if(j > 0) hsv(h = seq.int(from = 23/60, to = 11/60,
                                            length.out = j), alpha = alpha),
                  if(k > 0) hsv(h = seq.int(from = 10/60, to =  6/60,
                                            length.out = k), alpha = alpha,
                                s = seq.int(from = 1, to = 0.3,
                                            length.out = k), v = 1))
        if(rev) rev(cols) else cols
    } else character()
}

terrain.colors <- function (n, alpha, rev = FALSE)
{
    if ((n <- as.integer(n[1L])) > 0) {
	k <- n%/%2
	h <- c(4/12, 2/12, 0/12)
	s <- c(1, 1, 0)
	v <- c(0.65, 0.9, 0.95)
	cols <- c(hsv(h = seq.int(h[1L], h[2L], length.out = k),
                      s = seq.int(s[1L], s[2L], length.out = k),
                      v = seq.int(v[1L], v[2L], length.out = k), alpha = alpha),
                  hsv(h = seq.int(h[2L], h[3L], length.out = n - k + 1)[-1L],
                      s = seq.int(s[2L], s[3L], length.out = n - k + 1)[-1L],
                      v = seq.int(v[2L], v[3L], length.out = n - k + 1)[-1L],
                      alpha = alpha))
        if(rev) rev(cols) else cols
    } else character()
}

heat.colors <- function (n, alpha, rev = FALSE)
{
    if ((n <- as.integer(n[1L])) > 0) {
	j <- n %/% 4
	i <- n - j
	cols <- c(rainbow(i, start = 0, end = 1/6, alpha = alpha),
                  if (j > 0)
                      hsv(h = 1/6,
                          s = seq.int(from = 1-1/(2*j), to = 1/(2*j),
                                      length.out = j),
                          v = 1, alpha = alpha))
        if(rev) rev(cols) else cols
    } else character()
}

cm.colors <- function (n, alpha, rev = FALSE)
{
    if ((n <- as.integer(n[1L])) > 0L) {
	even.n <- n %% 2L == 0L
	k <- n %/% 2L
	l1 <- k + 1L - even.n
	l2 <- n - k + even.n
	cols <- c(if(l1 > 0L)
                      hsv(h =  6/12,
                          s = seq.int(.5, if(even.n) .5/k else 0,
                                      length.out = l1),
                          v = 1, alpha = alpha),
                  if(l2 > 1)
                      hsv(h = 10/12, s = seq.int(0, 0.5, length.out = l2)[-1L],
                          v = 1, alpha = alpha))
        if(rev) rev(cols) else cols
    } else character()
}

gray.colors <- function(n, start = 0.3, end = 0.9, gamma = 2.2, alpha,
                        rev = FALSE) {
    cols <- gray(seq.int(from = start^gamma,
                         to = end^gamma, length.out = n)^(1/gamma),
                 alpha)
    if(rev) rev(cols) else cols
}

grey.colors <- gray.colors

##' match palette name (not exported)
palette.match <- function(pal) {
    fx <- function(x) tolower(gsub("[-, _, \\,, (, ), \\ , \\.]", "", x))
    charmatch(fx(pal), fx(names(.palette_colors_hex)))
}

palette <- function (value)
{
    ## if value missing return current palette (visibly)
    if (missing(value)) return(.Call(C_palette, character()))
    
    ## in case value is just a single string, select the corresponding set
    ## colors with "default" handled at C level
    if (length(value) == 1L && value != "default") {
        n <- palette.match(value)
	if (!is.na(n)) value <- .palette_colors_hex[[n]]
    }

    ## set new palette value, return old one invisibly
    ## if a .Device is open, record the .Call.graphics
    if (.Device == "null device") {
      invisible(.Call(C_palette, value))
    } else {
      invisible(.Call.graphics(C_palette, value))    
    }
}

## palette.colors() is a function for accessing the colors behind palette()
## directly. palette.pals() shows the available names (a la hcl.pals()).
palette.pals <- function() names(.palette_colors_hex)

palette.colors <- function(n = NULL, palette = "Okabe-Ito",
                           alpha, recycle = FALSE)
{
    ## number of colors
    if (!is.null(n)) {
        n <- as.integer(n[1L])
        if (n < 1L) return(character())
    }

    p <- palette.match(palette)
    if (is.na(p)) stop("'palette' does not match any given palette")
    if (p < 1L) stop("'palette' is ambiguous")
 
    ## select n colors from palette
    nc <- length(cols <- .palette_colors_hex[[p]])
    if (is.null(n))
        n <- nc
    else if(n > nc) {
        if(recycle) {
            cols <- cols[rep_len(seq_len(nc), n)]
        } else {
            warning(sprintf("'n' set to %s, the maximum available for %s palette",
                            n, palette))
            n <- nc
        }
    }
    else if(n < nc)
        cols <- cols[seq_len(n)]

    ## add alpha if specified as number:
    if (!(missing(alpha) || is.null(alpha))) {
        alpha <- format(as.hexmode(round(alpha * 255 + 0.0001)),
                        width = 2L, upper.case = TRUE)
	cols <- paste0(cols, alpha)
    }
    cols
}

## underlying hex codes for palette color sets
.palette_colors_hex <- list(
    ## default in R <= 3.6.x
    "R3" = c("#000000", "#FF0000", "#00CD00", "#0000FF",
             "#00FFFF", "#FF00FF", "#FFFF00", "#BEBEBE"),
    ## rgb(
    ##     r = c(0, 255,   0,   0,   0, 255, 255, 190),
    ##     g = c(0,   0, 205,   0, 255,   0, 255, 190),
    ##     b = c(0,   0,   0, 255, 255, 255,   0, 190),
    ##     maxColorValue = 255
    ## ),

    ## new default in R >= 4.0.0		       
    "R4" = c("#000000", "#DF536B", "#61D04F", "#2297E6",
             "#28E2E5", "#CD0BBC", "#F5C710", "#9E9E9E"),
    ## hcl(h = c(0,   5, 125, 245, 195, 315,  65,   0),
    ##     c = c(0, 100,  90,  85,  63, 105,  94,   0),
    ##     l = c(0,  55,  75,  60,  82,  48,  82,  65)
    ## ),

    ## scales::hue_pal (Hadley Wickham)
    ## re-ordered for RGBCMY plus black/gray
    "ggplot2" = c("#000000", "#F8766D", "#00BA38", "#619CFF",
                  "#00BFC4", "#F564E3", "#B79F00", "#9E9E9E"),
    ## hcl(
    ##     h = c(0,  15, 135, 255, 195, 315,  75,   0),
    ##     c = c(0, 100, 100, 100, 100, 100, 100,   0),
    ##     l = c(0,  65,  65,  65,  65,  65,  65,  65)
    ## ),

    ## Masataka Okabe & Kei Ito
    ## http://jfly.iam.u-tokyo.ac.jp/color/
    "Okabe-Ito" = c(black = "#000000", orange = "#E69F00", skyblue = "#56B4E9", 
        bluishgreen = "#009E73", yellow = "#F0E442", blue = "#0072B2", 
        vermillion = "#D55E00", reddishpurple = "#CC79A7", gray = "#999999"),

    ## ColorBrewer.org (Mark A. Harrower & Cynthia A. Brewer)
    ## http://ColorBrewer2.org/
    "Accent" = c("#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0", 
        "#F0027F", "#BF5B17", "#666666"),
    "Dark 2" = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E",
        "#E6AB02", "#A6761D", "#666666"),
    "Paired" = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", 
        "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A"),
    "Pastel 1" = c("#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6",
        "#FFFFCC", "#E5D8BD", "#FDDAEC", "#F2F2F2"),
    "Pastel 2" = c("#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4", "#E6F5C9",
        "#FFF2AE", "#F1E2CC", "#CCCCCC"),
    "Set 1" = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
        "#FFFF33", "#A65628", "#F781BF", "#999999"),
    "Set 2" = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854",
        "#FFD92F", "#E5C494", "#B3B3B3"),
    "Set 3" = c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3",
        "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD"),

    ## Tableau 10 (Maureen Stone & Cristy Miller)
    ## https://www.tableau.com/about/blog/2016/7/colors-upgrade-tableau-10-56782
    "Tableau 10" = c(blue = "#4E79A7", orange = "#F28E2B", red = "#E15759",
        lightteal = "#76B7B2", green = "#59A14F", yellow = "#EDC948",
        purple = "#B07AA1", pink = "#FF9DA7", brown = "#9C755F",
        lightgray = "#BAB0AC"),
    "Classic Tableau" = c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD",
        "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF"),
	
    ## Polychrome
    ## (Kevin R. Coombes, Guy Brock, Zachary B. Abrams, Lynne V. Abruzzo)
    ## https://doi.org/10.18637/jss.v090.c01
    "Polychrome 36" = c(darkpurplishgray = "#5A5156", purplishwhite = "#E4E1E3",
        vividred = "#F6222E", vividpurple = "#FE00FA",
        vividyellowishgreen = "#16FF32",
	strongpurplishblue = "#3283FE", vividorangeyellow = "#FEAF16",
	vividpurplishred = "#B00068", brilliantgreen = "#1CFFCE",
	vividyellowgreen = "#90AD1C", vividblue = "#2ED9FF",
        brilliantpurple = "#DEA0FD",
	vividviolet = "#AA0DFE", strongpink = "#F8A19F", strongblue = "#325A9B",
	strongreddishorange = "#C4451C", vividgreen = "#1C8356",
	lightolivebrown = "#85660D", vividreddishpurple = "#B10DA1",
	vividgreenishyellow = "#FBE426", vividyellowishgreen = "#1CBE4F", 
        vividred = "#FA0087", vividpurplishred = "#FC1CBF",
        paleyellow = "#F7E1A0", 
        strongreddishpurple = "#C075A6", vividviolet = "#782AB6",
	vividyellowgreen = "#AAF400", verylightblue = "#BDCDFF",
	strongreddishbrown = "#822E1C", verylightyellowishgreen = "#B5EFB5", 
        verylightbluishgreen = "#7ED7D1", deepgreenishblue = "#1C7F93", 
        vividpurple = "#D85FF7", deeppurple = "#683B79",
        brilliantblue = "#66B0FF", 
        vividviolet = "#3B00FB"),
    "Alphabet" = c(amethyst = "#AA0DFE", blue = "#3283FE", caramel = "#85660D", 
        damson = "#782AB6", ebony = "#565656", forest = "#1C8356",
        green = "#16FF32", honey = "#F7E1A0", iron = "#E2E2E2",
        jade = "#1CBE4F", kingcrab = "#C4451C",  lavender = "#DEA0FD",
        magenta = "#FE00FA", navy = "#325A9B", orange = "#FEAF16",
        pink = "#F8A19F", quagmire = "#90AD1C", red = "#F6222E", 
        sea = "#1CFFCE", turquoise = "#2ED9FF", ultraviolet = "#B10DA1", 
        violet = "#C075A6", wine = "#FC1CBF", xanthin = "#B00068",
        yellow = "#FBE426", zinnia = "#FA0087")
)

## An unexported version that works with internal representation as 'rcolor'
## We could avoid this if we knew at R level whether the display list was
## enabled or inhibited: but we do need to record a call to C_palette2.
recordPalette <- function()
    .Call.graphics(C_palette2, .Call(C_palette2, NULL))

