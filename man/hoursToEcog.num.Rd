% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ecog_functions.r
\name{hoursToEcog.num}
\alias{hoursToEcog.num}
\title{Hours to ECOG (numeric)}
\usage{
hoursToEcog.num(
  num.hours,
  vec.breaks = c(0, 3, 6, 8, 10, 20),
  vec.labels = c(4, 3, 2, 1, 0)
)
}
\arguments{
\item{num.hours}{number of hours}

\item{vec.breaks}{vector of hour cutoffs corresponding to ECOG classes}

\item{vec.labels}{vector of ecog classes}
}
\value{
numeric ECOG score
}
\description{
Function to convert hours of activity to ECOG performance status
}
\details{
Default active hours -> ECOG conversion:= 10 hours : 08-10 : 16-8 : 23-5: 3< 3: 4
}
