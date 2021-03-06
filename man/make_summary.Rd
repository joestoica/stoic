% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_summary.R
\name{make_summary}
\alias{make_summary}
\title{Makes a summary table of specific data frame columns given certain functions.}
\usage{
make_summary(df, columns, funs = c("length", "mean", "var", "sd",
  "median", "max", "min"), range = TRUE, drop = TRUE)
}
\arguments{
\item{df}{data frame that supplies the data}

\item{columns}{A numerical or character vector indicating the columns that are to be selected.}

\item{funs}{a character vector that supplies the functions that will be used to summarize the columns}

\item{range}{boolean to determine whether range should be calculated, TRUE by default}
}
\value{
A data frame with number of rows equal to length of the column
  vector and the number of columns equal to the length of funs.
}
\description{
\code{make_summary} returns a data frame that summarizes each input column using
  the functions given in a supplied vector.
}
\examples{
make_summary(iris, 1:4)
make_summary(iris, c("Sepal.Length", "Sepal.Width"))
#' make_summary(iris, 1:4, funs = c("length", "mean", "var", "sd", "median", "min"), range = FALSE)

\dontrun{
make_summary(iris, c(Sepal.Length, Sepal.Width))
make_summary(iris, 1:4, funs = c("length", "mean", "var", "sd", "median", "min"), range = TRUE)
}
}
