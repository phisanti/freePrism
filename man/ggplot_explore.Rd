% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_plotting_tools.R
\name{ggplot_explore}
\alias{ggplot_explore}
\title{Generate exploration plots}
\usage{
ggplot_explore(d, input)
}
\arguments{
\item{d}{A data frame or data table containing the data to be plotted.}

\item{input}{A named list containing the input parameters for the plot. Required input parameters are:
\describe{
\item{plot_type}{A character string specifying the type of plot to be generated. Allowed values are "histogram", "QQ", and "Correation".}
\item{colpal}{A character string specifying the color palette to be used in the plot.}
\item{treatment}{A character string specifying the name of the treatment variable in the data.}
}}
}
\value{
A ggplot object containing the exploration plot.
}
\description{
This function generates exploration plots based on the input data and the selected plot type.
}
