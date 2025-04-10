% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/writeSWBMInputs.R
\name{write_SWBM_ET_correction_file}
\alias{write_SWBM_ET_correction_file}
\title{Write ET Correction File for SWBM}
\usage{
write_SWBM_ET_correction_file(output_dir, start_date, end_date)
}
\arguments{
\item{output_dir}{Directory where the ET correction file will be saved.}

\item{start_date}{Start date of the simulation (as Date object).}

\item{end_date}{End date of the simulation (as Date object).}
}
\value{
None. Saves the ET correction data as \code{field_et_corrections.txt} in \code{output_dir}.
}
\description{
Generates and saves a file specifying evapotranspiration (ET) correction factors
for each agricultural field for each stress period in the simulation.
Fields without specific corrections default to a value of 1.0 (no change to ET).
}
\details{
The correction file is used in the SWBM model to adjust evapotranspiration
based on the implementation of best management practices (BMPs) in 2024.
}
\examples{
write_SWBM_ET_correction_file(output_dir = "model_outputs",
                              start_date = as.Date("1990-10-01"),
                              end_date = as.Date("2024-12-31"))
}
