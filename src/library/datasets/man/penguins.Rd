\name{penguins}
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
      with levels \code{Biscoe}, \code{Dream}, and \code{Torgersen}}
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
    \item{\code{Region}}{\code{character}, the region of Palmer \acronym{LTER} sampling grid}
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
    \item{\enc{Adélie}{Adelie} penguins:}{
      \bibshow{R:Palmer_Station_Antarctica_LTER+Gorman:2020a}
    }
    \item{Gentoo penguins:}{
      \bibshow{R:Palmer_Station_Antarctica_LTER+Gorman:2020b}
    }
    \item{Chinstrap penguins:}{
      \bibshow{R:Palmer_Station_Antarctica_LTER+Gorman:2020c}
    }
  }

  The title naming convention for the source for the Gentoo and Chinstrap
  data is that same as for \enc{Adélie}{Adelie} penguins.
}
\details{
  \bibcitet{R:Gorman+Williams+Fraser:2014}
  used the data to study sex dimorphism separately for the three species.

  \bibcitet{R:Horst+Presmanes_Hill+Gorman:2022} popularized the data as
  an illustration for different statistical methods, as an alternative
  to the \code{\link{iris}} data.

  \bibcitet{R:Kaye+Turner+Gorman:2025} provide the scripts used to
  create these data sets from the original source data, and a notebook
  reproducing results from \bibcitet{R:Gorman+Williams+Fraser:2014}.
}
\note{
  These data sets are also available in the \CRANpkg{palmerpenguins} package.  
  See the \href{https://allisonhorst.github.io/palmerpenguins/}{package website} 
  for further details and resources.

  The \code{penguins} data has some shorter variable names than the \pkg{palmerpenguins} version, 
  for compact code and data display. 
}
\references{
  \bibshow{*}
}
\examples{
## view summaries
summary(penguins)
summary(penguins_raw)  # categorical variables are (still) character vectors
## summarize character vectors as factors:
summary(penguins_raw, character.method = "factor")

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
