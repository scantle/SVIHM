% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/precipData.R
\name{make_daily_precip}
\alias{make_daily_precip}
\title{Make Daily Precip}
\usage{
make_daily_precip(
  weather_table,
  station_table,
  daily_precip_start_date,
  daily_precip_end_date,
  ref_data_dir
)
}
\arguments{
\item{weather_table}{DataFrame of weather data from RSVP::download_meteo_data or equivalent
long precipitation data}

\item{station_table}{Dataframe of station info (i.e., data(noaa_stations))}

\item{daily_precip_start_date}{Start of data period}

\item{daily_precip_end_date}{End of data period}

\item{ref_data_dir}{Reference data directory}
}
\value{
DataFrame with all data sources and no missing days (but may have missing data)
}
\description{
Creates daily precip dataframe out of (1) downloaded noaa data (weather_table) and (2) the
"original" model 1991-2011 precipitation data. Each datasource is in a different column.
}
\author{
Claire Kouba, Leland Scantlebury
}
