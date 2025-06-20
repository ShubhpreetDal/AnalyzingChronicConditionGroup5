% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualization.R
\name{generate_barplots}
\alias{generate_barplots}
\title{Generate bar plots for all categorical variables in a dataset}
\usage{
generate_barplots(df, output_dir = "Plots")
}
\arguments{
\item{df}{A data frame (categorical variables recommended)}

\item{output_dir}{Path to the folder where bar plots will be saved}
}
\description{
This function takes a data frame and generates bar plots for each column,
saving the plots in the specified output directory.
}
