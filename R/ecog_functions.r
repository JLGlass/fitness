#' @import data.table
NULL

#' Hours to ECOG (numeric)
#'
#' Function to convert hours of activity to ECOG performance status
#'
#' Default active hours -> ECOG conversion:
#'   >= 10 hours : 0
#'   8-10 : 1
#'   6-8 : 2
#'   3-5: 3
#'   < 3: 4
#'
#' @param num.hours number of hours
#' @param vec.breaks vector of hour cutoffs corresponding to ECOG classes
#' @param vec.labels vector of ecog classes
#'
#' @return numeric ECOG score
#'
#' @export

hoursToEcog.num <- function(num.hours, vec.breaks=c(0,3,6,8,10,20), vec.labels=c(4,3,2,1,0)) {
  num.ecog = as.numeric(as.character(cut(num.hours, breaks=vec.breaks, labels=vec.labels)))
  return(num.ecog)
}


#' Correct ECOG (numeric)
#'
#' Function to correct ECOG for HR response using an angle derived from linear modeling of HR vs steps
#'
#' Default ECOG HR correction:
#'   angle > 50: 0
#'   angle 30-50: -2
#'   angle 20-30: -1
#'   angle < 20: 0
#'
#' @param num.ecog ECOG score
#' @param num.angle angle from linear model of HR vs. steps
#' @param num.cutoffLow lowest angle to use for ECOG correction. Default: 20
#' @param num.cutoffModerate lowest angle corresponding to intense activity. Default: 30
#' @param num.cutoffHigh highest angle to use for ECOG correction. Default: 50
#'
#' @return numeric corrected ECOG score
#'
#' @export

correctEcog.num <- function(num.ecog, num.angle, num.cutoffLow=20, num.cutoffModerate=30, num.cutoffHigh=50) {
  ret.ecog = num.ecog

  if(!is.na(num.angle) && (num.angle >= num.cutoffModerate && num.angle <= num.cutoffHigh)) {
    ret.ecog = ret.ecog - 2
  }
  else if (!is.na(num.angle) && (num.angle >= num.cutoffLow && num.angle < num.cutoffModerate)) {
    ret.ecog = ret.ecog - 1
  }

  if (!is.na(ret.ecog) && ret.ecog < 0) {
    ret.ecog = 0
  }

  return(ret.ecog)
}


#' Hours to ECOG (percent)
#'
#' Function to convert hours of activity to ECOG performance status
#'
#' Default active hours -> ECOG conversion
#'   ECOG 4 = 0-25%
#'   ECOG 3 = 25-50%
#'   ECOG 2 = 50-75%
#'   ECOG 1 = 75-90%
#'   ECOG 0 = > 90%
#'
#' @param num.pct percent daily activity
#' @param vec.breaks percentage activity cutoffs
#' @param vec.labels ECOG scores corresponding to percentage activity categories
#'
#' @return numeric ECOG score
#'
#' @export

hoursToEcog.pct <- function(num.pct, vec.breaks=c(0,0.25,0.50,0.75,0.90, 1), vec.labels=c(4,3,2,1,0)) {
  num.ecog = as.numeric(as.character(cut(num.pct, breaks=vec.breaks, labels=vec.labels)))
  return(num.ecog)
}


#' Correct ECOG (percentage)
#'
#' Function to correct ECOG for percentage of time spent being 'fairly' or 'very' active (fitbit variables).
#'
#' Default ECOG correction:
#'   fairly + very active < %10: 0
#'   very active >= 5%: -2
#'   fairly active >= 10%: -1
#'
#' @param num.ecog ECOG score
#' @param pct.veryActive percentage of 'very active' time
#' @param num.pctFairlyActive percentage of 'fairly active' time
#' @param num.cutoffVery lowest percentage to use 'very active' ECOG correction. Default: 0.05
#' @param num.cutoffFairly lowest percentage to use 'fairly active' ECOG correction. Default: 0.10
#'
#' @return numeric corrected ECOG score
#'
#' @export

correctEcog.pct <- function(num.ecog, pct.veryActive, pct.fairlyActive, num.cutoffVery=0.05, num.cutoffFairly=0.10) {
  ret.ecog = num.ecog

  if (!is.na(pct.veryActive) && pct.veryActive >= num.cutoffVery) {
    ret.ecog = ret.ecog - 1
  }
  if (!is.na(pct.fairlyActive) && pct.fairlyActive >= num.cutoffFairly) {
    ret.ecog = ret.ecog - 1
  }

  if (!is.na(ret.ecog) && ret.ecog < 0) {
    ret.ecog = 0
  }

  return(ret.ecog)
}
