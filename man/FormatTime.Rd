% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReadICT.R
\name{FormatTime}
\alias{FormatTime}
\title{FormatTime}
\usage{
FormatTime(x, vars = attributes(x)$header[8])
}
\arguments{
\item{x}{table}

\item{vars}{variable names}
}
\value{
a data frame with \code{vars} columns to chron objects
}
\description{
Format time columns in ICT table
}

