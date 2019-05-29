#' @import data.table
#' @import ggplot2
#' @import ggrepel
NULL

#' A helper function to color dendrogram labels
#'
#' @param x dendrogram element
#' @param lbl data table of labels
#' @param vec.colors vector of colors indexed by sample group
#'
#' @return dendrogram element

labelNode <- function(x, lbl, vec.colors) {
  if (is.leaf(x)) {
    x.label = attr(x, 'label')
    labelCol = vec.colors[lbl[x.label, as.character(group)]]
    attr(x, 'nodePar') <- list(lab.col=labelCol)
  }
  return(x)
}


#' A function to generate a list of dendrograms with various sd cutoffs
#'
#' @param mat matrix of data
#' @param lbl data table with sample labels
#' @param vec.sd vector of SD cutoffs to use
#' @param dist.method distance method
#' @param hclust.method hclust method
#' @param vec.colors color vector indexed by sample group
#' @param colorFn dendrogram coloring function (ex. labelNode)
#'
#' @return a list of dendrograms
#'
#' @export

getDend.lst <- function(mat, lbl, vec.sd, dist.method, hclust.method, vec.colors, colorFn=labelNode) {

  dend.lst = list()

  dist.obj = dist(t(mat), method=dist.method)
  dend.lst[['All']] = dendrapply(as.dendrogram(hclust(dist.obj, method='ward.D')), colorFn, lbl, vec.colors)


  sd.meth = apply(mat, 1, sd)
  q.meth = quantile(sd.meth, probs=seq(0.01, 1, 0.01))

  for (i in vec.sd) {
    i.str = as.character(i)

    dist.obj = dist(t(mat[sd.meth > q.meth[i], ]), method=dist.method)
    dend.lst[[i.str]] = dendrapply(as.dendrogram(hclust(dist.obj, method=hclust.method)), colorFn, lbl, vec.colors)
  }

  return(dend.lst)
}
