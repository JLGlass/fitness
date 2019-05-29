#' @import data.table
#' @import ggplot2
#' @import ggrepel
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

#' A function to produce a list of PCA plots
#' @param pca.obj pca object
#' @param lbl data.table with sample labels and groups
#' @param str.title string with plot title. Default: 'PCA'
#' @param vec.colors.type color vector for sample types
#' @param p.theme ggplot2 theme variable
#' @param col.group group column in lbl. Default: 'group'
#' @param str.legend figure legend title. Default: 'Group'
#' @param str.xlabLoading x axis label in PCA loadings figure. Default: 'Ranked variables'
#'
#' @return list of plots
#'
#' @export

plotPCA.lst <- function(pca.obj, lbl, str.title='PCA', vec.colors.type, p.theme, col.group='group', str.legend='Group', str.xlabLoading='Ranked variables') {

  mlt.score = data.table(Sample=rownames(pca.obj$x), Sample.type=lbl[rownames(pca.obj$x), get(col.group)], pca.obj$x)
  mlt.load = data.table(rowNames=rownames(pca.obj$rotation), pca.obj$rotation)
  mlt.load[order(PC1), PC1.order := 1:nrow(mlt.load)]
  eigs = pca.obj$sdev^2
  var = round(100 * eigs / sum(eigs))

  str.xlab = paste('PC1: ', var[1], '% variance', sep='')
  str.ylab = paste('PC2: ', var[2], '% variance', sep='')

  p = list()

  # plot for all

  p[['All']] = ggplot(mlt.score, aes(PC1, PC2, fill=Sample.type)) + geom_point(shape=21, size=5, stroke=1, alpha=0.8) + scale_fill_manual(name=str.legend, values=vec.colors.type) + labs(title=str.title, x=str.xlab, y=str.ylab) + p.theme
  p[['All.label']] = ggplot(mlt.score, aes(PC1, PC2, fill=Sample.type, label=Sample)) + geom_point(shape=21, size=5, stroke=1, alpha=0.8) + geom_text_repel(show.legend=F) + scale_fill_manual(name=str.legend, values=vec.colors.type) + labs(title=str.title, x=str.xlab, y=str.ylab) + p.theme


  # plot load distribution

  sd.pc1 = sd(mlt.load$PC1)
  mean.pc1 = mean(mlt.load$PC1)

  p[['load']] = ggplot(mlt.load, aes(PC1.order, PC1)) + geom_line() + labs(title='PC1 loading (mean +/- 1 SD)', x=str.xlabLoading, y='PC1 loading') + geom_hline(yintercept = c(mean.pc1 + 2*sd.pc1, mean.pc1-2*sd.pc1), color='firebrick', alpha=0.2) + geom_hline(yintercept= mean.pc1, color='darkgray', alpha=0.2) + p.theme + theme(legend.position='none')


  return(p)
}

#' A function to produce a list of TSNE plots for a set of scores
#' @param pca.obj tsne object
#' @param vec.rows sample names
#' @param lbl data.table with sample labels and groups
#' @param str.title string with plot title. Default: 'TSNE'
#' @param vec.colors.type color vector for sample types
#' @param p.theme ggplot2 theme variable
#' @param col.group group column in lbl. Default: 'group'
#' @param col.key key column in lbl. Default: 'label'
#' @param str.legend figure legend title. Default: 'Group'
#'
#' @return list of plots
#'
#' @export

plotTsne.lst <- function(tsne.obj, vec.rows, lbl, str.title='TSNE', vec.colors.type, p.theme, col.group='group', col.key='label', str.legend='Group') {

  dt.tsne = data.table(label=vec.rows, tsne.obj$Y)
  dt.tsne = merge(dt.tsne, lbl[, list(label=get(col.key), get(col.group))], by='label')

  setnames(dt.tsne, c('Sample', 'x', 'y', 'Sample.type'))
  setcolorder(dt.tsne, c('Sample', 'Sample.type', 'x', 'y'))

  p.lst = list()

  # plot for all

  p.lst[['All']] = ggplot(dt.tsne, aes(x, y, fill=Sample.type)) + geom_point(shape=21, size=5, stroke=1, alpha=0.8) + scale_fill_manual(name=str.legend, values=vec.colors.type) + labs(title=str.title) + p.theme

  p.lst[['All.label']] = ggplot(dt.tsne, aes(x, y, fill=Sample.type, label=Sample)) + geom_point(shape=21, size=5, stroke=1, alpha=0.8) + geom_text_repel(show.legend=F) + scale_fill_manual(name=str.legend, values=vec.colors.type) + labs(title=str.title) + p.theme

  return(p.lst)
}
