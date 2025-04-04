% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/writeSWBMInputs.R
\name{write_SWBM_landcover_file}
\alias{write_SWBM_landcover_file}
\title{Write file specifying land use types for each field, for each stress period}
\usage{
write_SWBM_landcover_file(
  scenario_id = "basecase",
  output_dir,
  start_date,
  end_date
)
}
\arguments{
\item{scenario_name}{Name of  management scenario. Default is historical basecase or "basecase".}
}
\value{
landcover_tab
}
\description{
Write file specifying land use types for each field, for each stress period
}
