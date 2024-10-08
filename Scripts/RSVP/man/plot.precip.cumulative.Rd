% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{plot.precip.cumulative}
\alias{plot.precip.cumulative}
\title{Cumulative Precipitation Comparison Plot}
\usage{
\method{plot}{precip.cumulative}(dates, ..., title = NULL, unit = "mm", col = NULL)
}
\arguments{
\item{dates}{array of dates of precip events}

\item{...}{one or more arrays of precipitation at dates}

\item{title}{title of plot (optional)}

\item{unit}{character, the units the precipitation is measured in}

\item{col}{array of colors for data (optional)}
}
\value{
Plot
}
\description{
Cumulative Precipitation Comparison Plot
}
\examples{
\dontrun{
# Read
precip1 <- read.table('../../SVIHM_Input_Files/Scenario_Development/precip_regressed_orig.txt',
                      header = F, col.names = c('prcp','date'))
precip2 <- read.table('../../SVIHM_Input_Files/Scenario_Development/precip_regressed.txt',
                      header = F, col.names = c('prcp','date'))
precip1$date <- as.Date(precip1$date, format='\%d/\%m/\%Y')
precip2$date <- as.Date(precip2$date, format='\%d/\%m/\%Y')

# Subset
precip1 <- precip1[precip1$date > '2011-9-30',]
precip2 <- precip2[precip2$date > '2011-9-30',]
precip2 <- precip2[precip2$date <= max(precip1$date),]

# Plot
plot.precip.cumulative(precip1$date,
                       precip1$prcp,
                       precip2$prcp,
                       unit='m',
                       col=c('blue','red'))
}
}
