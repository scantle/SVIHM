% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/writeSWBMInputs.R
\name{write_SWBM_landcover_desc_file}
\alias{write_SWBM_landcover_desc_file}
\title{Write SWBM Polygon Landcover Description Table File}
\usage{
write_SWBM_landcover_desc_file(
  landcover_desc,
  output_dir,
  filename = "landcover_table.txt",
  verbose = TRUE
)
}
\arguments{
\item{landcover_desc}{A \code{data.frame} whose columns describe polygon landcover properties,
for example:
\itemize{
\item \code{id} - Numeric or character landcover identifier.
\item \code{Landcover_Name} - Landcover type name.
\item \code{IrrFlag}, \code{ET_Flag} - Logical flags for irrigation and ET.
\item \code{IrrSWC}, \code{RootDepth}, \code{RD_Mult}, \code{Kc_Mult} - Numeric parameters.
\item \code{IrrMonStart}, \code{IrrDayStart}, \code{IrrMonEnd}, \code{IrrDayEnd} - Integer dates.
\item \code{IrrEff_Flood}, \code{IrrEff_WL}, \code{IrrEff_CP} - Irrigation efficiencies.
}
All columns will be formatted to a fixed width based on their longest entry.}

\item{output_dir}{Character. Directory in which to write the output file.}

\item{filename}{Character. Name of the output file. Defaults to \code{"landcover_table.txt"}.}

\item{verbose}{Logical. If \code{TRUE}, prints a message indicating the full path of the file written.}
}
\value{
Invisibly returns \code{NULL}. The aligned landcover table is written
to \code{file.path(output_dir, filename)}.
}
\description{
Outputs the polygon landcover description table with fixed-width, column-aligned formatting,
ready for ingestion by the Soil Water Budget Model (SWBM). Each column is padded
to the maximum width needed (based on the header and all data rows), ensuring
human-readable alignment in the text file.
}
\details{
Unlike standard space-delimited writes, this function:
\enumerate{
\item Computes, for each column, the maximum number of characters needed (header vs. data).
\item Formats every cell in that column to that width (padding with spaces).
\item Joins columns with a single space, preserving neat vertical alignment.
}
}
\examples{
\dontrun{
write_SWBM_landcover_desc_file(
  landcover_desc = lc_desc,
  output_dir     = "SWBM_inputs",
  filename       = "landcover_table_aligned.txt",
  verbose        = TRUE
)
}
}
