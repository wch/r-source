\name{penguins}
\encoding{UTF-8}
\docType{data}
\title{Measurements of Penguins near Palmer Station, Antarctica}
\alias{penguins}
\alias{penguins_raw}
\description{
  Data on adult penguins covering three species found on three islands in the 
  Palmer Archipelago, Antarctica, including their size 
  (flipper length, body mass, bill dimensions), and sex.

  The columns of \code{penguins} are a subset of the more extensive
  \code{penguins_raw} data frame, which includes nesting observations 
  and blood isotope data. There are differences in the column names 
  and data types. See the \sQuote{Format} section for details. 
}
\usage{
penguins
penguins_raw
}
\format{
  \code{penguins} is a data frame with 344 rows and 8 variables:
  \describe{
    \item{\code{species}}{\code{\link{factor}}, with levels 
      \code{Adelie}, \code{Chinstrap}, and \code{Gentoo}}
    \item{\code{island}}{\code{factor}, 
      with levels \code{Biscoe}, \code{Dream}, and \code{Torgersen})}
    \item{\code{bill_len}}{\code{\link{numeric}}, bill length (millimeters)}
    \item{\code{bill_dep}}{\code{numeric}, bill depth (millimeters)}
    \item{\code{flipper_len}}{\code{\link{integer}}, flipper length (millimeters)}
    \item{\code{body_mass}}{\code{integer}, body mass (grams)}
    \item{\code{sex}}{\code{factor}, with levels \code{female} and \code{male}}
    \item{\code{year}}{\code{integer}, study year: 2007, 2008, or 2009}
  }

  \code{penguins_raw} is a data frame with 344 rows and 17 variables. 
  8 columns correspond to columns in \code{penguins}, 
  though with different variable names and/or classes:
  \describe{
    \item{\code{Species}}{\code{character}}
    \item{\code{Island}}{\code{character}}
    \item{\code{Culmen Length (mm)}}{\code{numeric}, bill length}
    \item{\code{Culmen Depth (mm)}}{\code{numeric}, bill depth}
    \item{\code{Flipper Length (mm)}}{\code{numeric}, flipper length}
    \item{\code{Body Mass (g)}}{\code{numeric}, body mass}
    \item{\code{Sex}}{\code{character}}
    \item{\code{Date Egg}}{\code{\link{Date}}, when study nest observed with 1 egg.
      The year component is the \code{year} column in \code{penguins}}
  }

  There are 9 further columns in \code{penguins_raw}:
  \describe{
    \item{\code{studyName}}{\code{character}, expedition during which the data was collected}
    \item{\code{Sample Number}}{\code{numeric}, continuous numbering sequence for each sample}
    \item{\code{Region}}{\code{character}, the region of Palmer LTER sampling grid}
    \item{\code{Stage}}{\code{character}, denoting reproductive stage at sampling}
    \item{\code{Individual ID}}{\code{character}, unique ID for each individual in dataset}
    \item{\code{Clutch Completion}}{\code{character}, 
      if the study nest was observed with a full clutch, i.e., 2 eggs}
    \item{\code{Delta 15 N (o/oo)}}{\code{numeric}, the ratio of stable isotopes 15N:14N}
    \item{\code{Delta 13 C (o/oo)}}{\code{numeric}, the ratio of stable isotopes 13C:12C}
    \item{\code{Comments}}{\code{character}, additional relevant information}
  }
}
\source{
  \describe{
    \item{\enc{Adélie}{Adelie} penguins:}{Palmer Station Antarctica LTER and K. Gorman (2020).
      Structural size measurements and isotopic signatures of foraging
      among adult male and female \enc{Adélie}{Adelie} penguins (Pygoscelis adeliae)
      nesting along the Palmer Archipelago near Palmer Station, 2007-2009
      ver 5. Environmental Data Initiative, \doi{10.6073/pasta/98b16d7d563f265cb52372c8ca99e60f}.}

    \item{Gentoo penguins:}{Palmer Station Antarctica LTER and K. Gorman (2020).
      \doi{10.6073/pasta/7fca67fb28d56ee2ffa3d9370ebda689}.}

    \item{Chinstrap penguins:}{Palmer Station Antarctica LTER and K. Gorman (2020).
      \doi{10.6073/pasta/c14dfcfada8ea13a17536e73eb6fbe9e}.}
  }

  The title naming convention for the source for the Gentoo and Chinstrap
  data is that same as for \enc{Adélie}{Adelie} penguins.
}
\references{
  Gorman, K. B., Williams, T. D. and Fraser, W. R. (2014)
  Ecological Sexual Dimorphism and Environmental Variability within a
  Community of Antarctic Penguins (Genus Pygoscelis).
  \emph{PLoS ONE} \bold{9}, 3, e90081; \doi{10.1371/journal.pone.0090081}.

  Horst, A. M., Hill, A. P. and Gorman, K. B. (2022) 
  Palmer Archipelago Penguins Data in the palmerpenguins R Package 
  - An Alternative to Anderson's Irises.
  \emph{R Journal} \bold{14}, 1; \doi{10.32614/RJ-2022-020}.

  Kaye, E., Turner, H., Gorman, K. B., Horst, A. M. and Hill, A. P. (2025)
  Preparing the Palmer Penguins Data for the \pkg{datasets} Package in R. 
  \doi{10.5281/zenodo.14902740}.
}
\details{
  \bibcite{Gorman \abbr{et al.}\sspace(2014)}
  used the data to study sex dimorphism separately for the three species.

  \bibcite{Horst \abbr{et al.}\sspace(2022)} popularized the data as an illustration 
  for different statistical methods, as an alternative to the \code{\link{iris}} data.

  \bibcite{Kaye \abbr{et al.}\sspace(2025)} provide the scripts used to create
  these data sets from the original source data, 
  and a notebook reproducing results from \bibcite{Gorman \abbr{et al.}\sspace(2014)}.
}
\note{
  These data sets are also available in the \CRANpkg{palmerpenguins} package.  
  See the \href{https://allisonhorst.github.io/palmerpenguins/}{package website} 
  for further details and resources.

  The \code{penguins} data has some shorter variable names than the \bold{palmerpenguins} version, 
  for compact code and data display. 
}
\examples{
## view summaries
summary(penguins)
summary(penguins_raw) # not useful for character vectors
## convert character vectors to factors first
dFactor <- function(dat) {
  dat[] <- lapply(dat, \(.) if (is.character(.)) as.factor(.) else .)
  dat
}
summary(dFactor(penguins_raw))

## visualise distribution across factors
plot(island ~ species, data = penguins)
plot(sex ~ interaction(island, species, sep = "\n"), data = penguins)

## bill depth vs. length by species (color) and sex (symbol):
## positive correlations for all species, males tend to have bigger bills
sym <- c(1, 16)
pal <- c("darkorange","purple","cyan4")
plot(bill_dep ~ bill_len, data = penguins, pch = sym[sex], col = pal[species])

## simplified sex dimorphism analysis for Adelie species:
## proportion of males increases with several size measurements
adelie <- subset(penguins, species == "Adelie")
plot(sex ~ bill_len, data = adelie)
plot(sex ~ bill_dep, data = adelie)
plot(sex ~ body_mass, data = adelie)
m <- glm(sex ~ bill_len + bill_dep + body_mass, data = adelie, family = binomial)
summary(m)

## Produce the long variable names as from {palmerpenguins} pkg:
long_nms <- sub("len", "length_mm",
                sub("dep","depth_mm",
                    sub("mass", "mass_g", colnames(penguins))))
## compare long and short names:
noquote(rbind(long_nms, nms = colnames(penguins)))

\dontrun{ # << keeping shorter 'penguins' names in this example:
    colnames(penguins) <- long_nms
}
}
\keyword{datasets}
