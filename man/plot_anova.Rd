% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_plotting_tools.R
\name{plot_anova}
\alias{plot_anova}
\title{Plot ANOVA or Kruskal-Wallis test results with optional post-hoc analysis}
\usage{
plot_anova(d, input, post_hoc, ref.group = NULL)
}
\arguments{
\item{d}{A data frame containing the data to plot}

\item{input}{A list containing input options for the plot. See Details for more information.}

\item{post_hoc}{An optional list containing information on which post-hoc tests to perform and how to adjust for multiple comparisons.}

\item{ref.group}{An optional character value indicating the reference group for comparisons.}
}
\value{
A ggplot object containing the plot.
}
\description{
This function produces a plot of the results of an ANOVA or Kruskal-Wallis 
test, with optional post-hoc analysis. The function accepts a data frame 
\code{d} and an input list \code{input} that contains information on the test
to perform, the variables to use, and various plotting options. The function
also accepts an optional argument \code{post_hoc} which, if provided, is a 
list containing information on which post-hoc tests to perform and how to 
adjust for multiple comparisons. If no post-hoc analysis is desired, this 
argument can be left as the default value \code{NULL}.
}
