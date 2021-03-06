% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_nm_tables.R
\name{index_table}
\alias{index_table}
\title{Index table columns}
\usage{
index_table(x, column_map = character(0))
}
\arguments{
\item{x}{A list containing the tables (`x$data`) to be combined along with
their respective names (`x$name`).}

\item{column_map}{A named character vector mapping column names either to
reserved NONMEM keywords or to itself.}
}
\value{
A tibble of the index.
}
\description{
Index table columns
}
\seealso{
\code{\link{parse_nm_input_record}}
}
\keyword{internal}
