% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_summarize_years}
\alias{fars_summarize_years}
\title{Summarize FARS data by year and month}
\usage{
fars_summarize_years(years)
}
\arguments{
\item{years}{A vector with a list of years}
}
\value{
This function returns a dataframe with the accidents by year and month.
}
\description{
This is a function that summarizes data from FARS by year and month. That file is coming from the US
}
\examples{
fars_summarize_years(2013)
fars_summarize_years(c(2013,2014))

}
\seealso{
\link{fars_read}
}
