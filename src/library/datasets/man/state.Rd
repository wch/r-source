% File src/library/datasets/man/state.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{state}
\docType{data}
\alias{state}
\alias{state.abb}
\alias{state.area}
\alias{state.center}
\alias{state.division}
\alias{state.name}
\alias{state.region}
\alias{state.x77}
\title{US State Facts and Figures}
\description{Data sets related to the 50 states of the United States of
  America.}
\usage{
state.abb
state.area
state.center
state.division
state.name
state.region
state.x77
}
\details{
  \R currently contains the following \dQuote{state} data sets.  Note
  that all data are arranged according to alphabetical order of the
  state names.
  \describe{
    \item{\code{state.abb}:}{character vector of 2-letter abbreviations
      for the state names.}
    \item{\code{state.area}:}{numeric vector of state areas (in square
      miles).}
    \item{\code{state.center}:}{ list with components named \code{x} and
      \code{y} giving the approximate geographic center of each state in
      negative longitude and latitude.  Alaska and Hawaii are placed
      just off the West Coast.}
    \item{\code{state.division}:}{factor giving state divisions (New
      England, Middle Atlantic, South Atlantic, East South Central, West
      South Central, East North Central, West North Central, Mountain,
      and Pacific).}
    \item{\code{state.name}:}{character vector giving the full state
      names.}
    \item{\code{state.region}:}{factor giving the region (Northeast,
      South, North Central, West) that each state belongs to.}
    \item{\code{state.x77}:}{matrix with 50 rows and 8 columns giving
      the following statistics in the respective columns.
      \describe{
        \item{\code{Population}:}{population estimate as of July 1,
          1975}
        \item{\code{Income}:}{per capita income (1974)}
        \item{\code{Illiteracy}:}{illiteracy (1970, percent of
          population)}
        \item{\code{Life Exp}:}{life expectancy in years (1969--71)}
        \item{\code{Murder}:}{murder and non-negligent manslaughter rate
          per 100,000 population (1976)}
        \item{\code{HS Grad}:}{percent high-school graduates (1970)}
        \item{\code{Frost}:}{mean number of days with minimum
          temperature below freezing (1931--1960) in capital or large
          city}
       \item{\code{Area}:}{land area in square miles}
    }}
  }
}
\source{
  U.S. Department of Commerce, Bureau of the Census (1977)
  \emph{Statistical Abstract of the United States}.

  U.S. Department of Commerce, Bureau of the Census (1977)
  \emph{County and City Data Book}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\keyword{datasets}
