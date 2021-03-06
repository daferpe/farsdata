% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_map_state}
\alias{fars_map_state}
\title{Map with the accidents by state for a year}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{An integer with the State Code}

\item{year}{A character string or an integer giving the year}
}
\value{
a map. If the \code{state.num} is invalid, it will return an error
}
\description{
This is a function that shows a map with the accidents by state for a year
}
\examples{
fars_map_state(31,2013)

}
\seealso{
\link{fars_read}

\link{make_filename}
}
