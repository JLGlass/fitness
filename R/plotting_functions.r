#' @import data.table
#' @import ggplot2
NULL

#' Plot Scatter
#'
#' Generic scatterplot function
#'
#' @param dt data.table to plot
#' @param col.x x column label
#' @param col.y y column label
#' @param vec.xlim vector of x limits (min, max)
#' @param vec.ylim vector of y limits (min, max)
#' @param str.title plot title
#' @param str.xlab x axis label
#' @param str.ylab y axis label
#' @param p.theme plot theme object
#' @param num.alpha alpha for dots. Default: 0.07
#' @param num.size size for dots. Defaults: 0.5
#' @param trend.method method for trendline. Default: 'lm'
#'
#' @return plot object
#'
#' @export

plotScatter.plt <- function(dt, col.x, col.y, vec.xlim, vec.ylim, str.title, str.xlab, str.ylab, p.theme, num.alpha=0.07, num.size=0.5, trend.method='lm') {
  plt = ggplot(dt, aes_string(col.x, col.y)) + geom_point(alpha=num.alpha, size=num.size) + geom_smooth(method=trend.method, se=F)

  if (!missing(vec.xlim) > 0) {
    plt = plt + xlim(vec.xlim)
  }
  if (!missing(vec.ylim) > 0) {
    plt = plt + ylim(vec.ylim)
  }
  if (!missing(str.title)) {
    plt = plt + ggtitle(str.title)
  }
  if (!missing(str.xlab)) {
    plt = plt + xlab(str.xlab)
  }
  if (!missing(str.ylab)) {
    plt = plt + ylab(str.ylab)
  }
  if (!missing(p.theme)) {
    plt = plt + p.theme
  }

  return(plt)
}
