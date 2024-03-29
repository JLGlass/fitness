% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting_functions.r
\name{plotTsne.lst}
\alias{plotTsne.lst}
\title{A function to produce a list of TSNE plots for a set of scores}
\usage{
plotTsne.lst(
  tsne.obj,
  vec.rows,
  lbl,
  str.title = "TSNE",
  vec.colors.type,
  p.theme,
  col.group = "group",
  col.key = "label",
  str.legend = "Group"
)
}
\arguments{
\item{vec.rows}{sample names}

\item{lbl}{data.table with sample labels and groups}

\item{str.title}{string with plot title. Default: 'TSNE'}

\item{vec.colors.type}{color vector for sample types}

\item{p.theme}{ggplot2 theme variable}

\item{col.group}{group column in lbl. Default: 'group'}

\item{col.key}{key column in lbl. Default: 'label'}

\item{str.legend}{figure legend title. Default: 'Group'}

\item{pca.obj}{tsne object}
}
\value{
list of plots
}
\description{
A function to produce a list of TSNE plots for a set of scores
}
