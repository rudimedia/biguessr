% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/biguessdf.R
\name{biguessdf}
\alias{biguessdf}
\title{Add Gender-Coding to a Data Frame}
\usage{
biguessdf(df, name_col)
}
\arguments{
\item{df}{A data frame or tibble containing the names to be processed.}

\item{name_col}{The unquoted name of the column containing given names.}
}
\value{
The original data frame with two new columns:
\itemize{
\item \code{gender} - a factor with levels 1 (male) and 2 (female)
\item \code{method} - "KNOWN" if from predefined list, "GUESSED" if predicted by LLM
}
}
\description{
This function adds gender-coding to a data frame or tibble based on a given name column.
It uses a predefined name-gender list, and if the name is not found, it uses llama3.2 to guess.
}
\examples{
\dontrun{
  df <- tibble::tibble(id = 1:4, name = c("J\u00fcrgen", "Franzi", "Rudi", "Pepe"))
  biguessdf(df, name)
}
}
