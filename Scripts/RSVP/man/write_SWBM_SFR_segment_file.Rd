% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SWBM_SFR_files.R
\name{write_SWBM_SFR_segment_file}
\alias{write_SWBM_SFR_segment_file}
\title{Write SWBM SFR Inflow Segments file}
\usage{
write_SWBM_SFR_segment_file(
  output_dir,
  filename = "SFR_inflow_segments.txt",
  only_mf_tribs = T,
  verbose = T
)
}
\arguments{
\item{output_dir}{directory to write the files to}

\item{filename}{input filename, SFR_inflow_segments.txt by default}

\item{only_mf_tribs}{Only include the tributaries explicitly modeled in the MF file (no PRMS additions)}

\item{verbose}{Logical. Whether to print status information to the console. Default is TRUE.}
}
\value{
None. The function writes the input file to the specified directory.
}
\description{
Writes file to establish connection between tributaries, subwatersheds, and MODFLOW SFR package
}
\examples{

write_SWBM_SFR_segment_file(output_dir=out_dir)

}
