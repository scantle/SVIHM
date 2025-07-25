% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/riverRegression.R
\name{write_trib_file}
\alias{write_trib_file}
\title{Write Tributary Flow Record File}
\usage{
write_trib_file(
  gauges,
  output_dir,
  start_date,
  end_date,
  old_tribs_df = NA,
  monthly = F,
  filename = "daily_tributary_streamflow.txt",
  verbose = TRUE
)
}
\arguments{
\item{gauges}{List of gauge data from \code{\link{get_tributary_flows}}}

\item{output_dir}{Directory to write file}

\item{start_date}{Simulation start date}

\item{end_date}{Simulation end date}

\item{filename}{Filename (optional, default: daily_tributary_streamflow.txt)}

\item{verbose}{T/F write status info to console (default: TRUE)}
}
\value{
None
}
\description{
Write Tributary Flow Record File
}
