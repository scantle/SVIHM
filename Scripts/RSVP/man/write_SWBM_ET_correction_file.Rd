% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/writeSWBMInputs.R
\name{write_SWBM_ET_correction_file}
\alias{write_SWBM_ET_correction_file}
\title{Write SWBM ET Correction File}
\usage{
write_SWBM_ET_correction_file(
  et_cor_output,
  output_dir,
  filename = "field_et_corrections.txt",
  verbose = TRUE
)
}
\arguments{
\item{output_dir}{Character. Directory where the file will be written.}

\item{filename}{Character. Name of the output file. Default:
\code{"ETcorrection_factors.txt"}.}

\item{verbose}{Logical. If \code{TRUE}, prints a message before writing.
Default: \code{TRUE}.}

\item{et_cor_df}{A \code{data.frame} as returned by
\code{\link{create_SWBM_ET_correction_df}()}, with \code{Stress_Period}
and \code{ID_<fieldID>} columns.}
}
\value{
Invisibly returns \code{NULL}.  Side effect: writes
\code{et_cor_df} to \code{file.path(output_dir, filename)}.
}
\description{
Writes an ET correction factors table to a space-delimited SWBM input file.
}
\examples{
\dontrun{
df <- create_SWBM_ET_correction_df(
  start_date  = as.Date("2023-10-01"),
  end_date    = as.Date("2024-09-30"),
  scenario_id = "basecase"
)
write_SWBM_ET_correction_file(df, output_dir = "SWBM_inputs")
}
}
\seealso{
\code{\link{create_SWBM_ET_correction_df}}, \code{\link{write_SWBM_file}}
}
