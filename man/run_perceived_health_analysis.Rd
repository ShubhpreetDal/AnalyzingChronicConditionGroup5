% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/perceived_health.R
\name{run_perceived_health_analysis}
\alias{run_perceived_health_analysis}
\title{Analyze Perceived vs Actual Health for Chronic Conditions}
\usage{
run_perceived_health_analysis(
  df,
  perceived_col,
  output_dir = "perceived_health_plots"
)
}
\arguments{
\item{df}{Cleaned dataset (already transformed)}

\item{perceived_col}{Column name: either "perceived_health" or "perceived_mental_health"}

\item{output_dir}{Directory to save plots and chi-square results}
}
\description{
Performs crosstabs, chi-square tests, and bar plots showing average condition prevalence
by perceived health levels (either general or mental health).
}
