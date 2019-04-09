#' @import data.table
NULL

#' ModelPairs
#'
#' Function to create a model with a pair of variables
#'
#' @param dt data.table with data
#' @param col.id id in data.table (ex. study day)
#' @param col.x x variable
#' @param col.y y variable (response)
#' @param fn.lstAggregate function to aggregate list data
#' @param str.variable name for id column in new table after aggregation over all col.id in vec.idRange
#' @param str.value name for value column in new table
#' @param fn.rangeAggregate function to aggregate data in new table (ex. median of days in the specified date range). Default: median
#'
#' @return data.table with aggregated data
#'
#' @export

modelPairs.dt <- function(dt, col.id, col.x, col.y, fn.model=lm) {
  f = as.formula(paste(col.y, '~', col.x))
  str.xlab = paste('coef', col.x, sep='.')
  str.ylab = paste('baseline', col.y, sep='.')

  vec.coefs = coef(fn.model(f, dt))
  names(vec.coefs) = c(str.ylab, str.xlab)
  vec.coefs['angle'] = atan(vec.coefs[str.xlab] * 60) * 180/pi

  return(as.list(vec.coefs))
}
