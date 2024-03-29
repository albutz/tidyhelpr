% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{subset_lines}
\alias{subset_lines}
\title{Filter relevant lines}
\usage{
subset_lines(.file, .pattern, .locale)
}
\arguments{
\item{.file}{A path to a file or a connection.}

\item{.pattern}{Pattern to look for.}

\item{.locale}{A \code{\link[readr]{locale}} object.}
}
\value{
A character vector.
}
\description{
Subset lines to the relevant part, starting (inclusive) with the line in
which \code{.pattern} is located.
}
