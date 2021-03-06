% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_batchRunWinsteps.R
\name{plot.batchRunWinsteps}
\alias{plot.batchRunWinsteps}
\title{Plot the test characteristic curve}
\usage{
\method{plot}{batchRunWinsteps}(ob, theta = seq(-4, 4, 0.1), colors = NULL,
  store = FALSE, ...)
}
\arguments{
\item{ob}{Objects of class batchRunWinsteps}

\item{theta}{Theta values from which the information function values are 
calculated from. Defaults to a sequence from -4 to 4 by 0.1. The limits of 
this range are used for the x-axes.}

\item{colors}{Line colors for the plot. Defaults to NULL, in which case the
colors are equivalent to the default ggplot colors.}

\item{store}{Optional logical argument to return data used in plotting.}

\item{...}{Additional arguments passed to \code{\link{plot}}.}
}
\description{
Plot the test characteristic curve
}
