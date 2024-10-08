% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{get_water_year}
\alias{get_water_year}
\title{Get water year for dates}
\usage{
get_water_year(dates, wy_start_month = 10)
}
\arguments{
\item{dates}{array of Dates}

\item{wy_start_month}{Water Year start month (10 (Oct) by default)}
}
\value{
array of water years
}
\description{
Get water year for dates
}
\examples{
get_water_year(c(as.Date('1991-09-30'), as.Date('1991-10-01')))
}
