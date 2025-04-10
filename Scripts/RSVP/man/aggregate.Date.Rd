% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{aggregate.Date}
\alias{aggregate.Date}
\title{Aggregate value to Daily, Monthly, or Yearly}
\usage{
\method{aggregate}{Date}(dates, values, interval, date_col = "Date", FUN, ...)
}
\arguments{
\item{dates}{array of Date values}

\item{values}{array of values to be aggregated}

\item{interval}{aggregation interval: 'd' for daily, 'm' for monthly, 'y' for yearly}

\item{date_col}{Name of output DF column for date (default: 'Date')}

\item{FUN}{function to aggregate by}

\item{...}{Optional values passed to \code{\link{aggregate}}}
}
\value{
Data.frame of aggregated values, with a Date column (only of type 'Date' if daily)
}
\description{
Aggregate value to Daily, Monthly, or Yearly
}
\examples{
# Some made up data
dates <- as.Date(c('1988-11-30','1988-11-30','1988-11-30',
                   '1988-12-01','1988-12-01','1988-12-01'))
values <- c(12,13,14,14,15,16)
# Use function to average by day
avg <- aggregate.Date(dates, values, interval='d', FUN=mean)
}
