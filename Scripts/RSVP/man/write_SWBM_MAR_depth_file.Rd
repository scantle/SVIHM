% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/writeSWBMInputs.R
\name{write_SWBM_MAR_depth_file}
\alias{write_SWBM_MAR_depth_file}
\title{Write SWBM MAR Depth Table to SWBM Input File}
\usage{
write_SWBM_MAR_depth_file(
  mar_df,
  output_dir,
  filename = "MAR_depth.txt",
  verbose = TRUE
)
}
\arguments{
\item{mar_df}{A \code{data.frame} as returned by \code{create_MAR_depth_df()},
with \code{Stress_Period} and \code{ID_<fieldID>} columns.}

\item{output_dir}{Character. Directory in which to write the file.}

\item{filename}{Character. Name of the output file. Default: \code{"MAR_depth.txt"}.}

\item{verbose}{Logical. If \code{TRUE}, prints a message before writing. Default: \code{TRUE}.}
}
\value{
Invisibly returns \code{NULL}.  Side effect: writes \code{mar_df} to
\code{file.path(output_dir, filename)} using space-delimited format.
}
\description{
Writes a managed-aquifer recharge (MAR) depth (m) table to disk in SWBM input format.
}
\examples{
\dontrun{
df <- create_MAR_depth_df(
  start_date   = as.Date("2023-10-01"),
  end_date     = as.Date("2024-09-30"),
  mar_scenario = "max24"
)
write_SWBM_MAR_depth_file(df, output_dir = "SWBM_inputs")
}
}
\seealso{
\code{\link{create_MAR_depth_df}}
}
