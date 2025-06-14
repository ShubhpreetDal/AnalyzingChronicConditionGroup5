% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_logreg_coefficients.R
\name{extract_logistic_coefficients}
\alias{extract_logistic_coefficients}
\title{Extract and plot coefficients from regularized logistic regression}
\usage{
extract_logistic_coefficients(
  model_path,
  output_csv,
  plot_title = NULL,
  plot_path = NULL,
  top_n = 7
)
}
\arguments{
\item{model_path}{Path to saved .rds model}

\item{output_csv}{Output path for coefficient CSV}

\item{plot_title}{Title for coefficient plot}

\item{plot_path}{Output path for coefficient plot}

\item{top_n}{Number of top coefficients to display}
}
\value{
Tibble with coefficients
}
\description{
Extract and plot coefficients from regularized logistic regression
}
