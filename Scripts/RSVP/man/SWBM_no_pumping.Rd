% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/writeSWBMInputs.R
\name{SWBM_no_pumping}
\alias{SWBM_no_pumping}
\title{Recode Polygon Water Sources to Remove Pumping}
\usage{
SWBM_no_pumping(polygon_df, include_unknown = TRUE)
}
\arguments{
\item{polygon_df}{A \code{data.frame} containing at least the column
\code{WATERSOURC} (numeric codes).}

\item{include_unknown}{Logical; if \code{TRUE} (default), recode
unknown sources (code 999) to dry-farmed (code 5).  If \code{FALSE},
leave code 999 unchanged.}
}
\value{
A modified copy of \code{polygon_df} with updated
\code{WATERSOURC} codes (and their text labels).
}
\description{
Converts groundwater and mixed water-source codes to non-pumping equivalents,
and optionally recodes unknown sources to dry-farmed.  After recoding, updates
any associated text labels via \code{\link{translate_SWBM_codes}}.
}
\details{
SWBM water-source codes:
\tabular{rl}{
1   \tab Surface water\cr
2   \tab Groundwater\cr
3   \tab Mixed surface/groundwater\cr
4   \tab Sub-irrigated\cr
5   \tab Dry-farmed (no pumping)\cr
999 \tab Unknown (assumed groundwater)\cr
}

This function applies the following recoding to \code{WATERSOURC}:
\itemize{
\item Groundwater (2) to Dry-farmed (5)
\item Mixed (3)      to Surface water (1)
\item Unknown (999)  to Dry-farmed (5), if \code{include_unknown = TRUE}
}

After recoding the numeric codes, the function calls
\code{\link{translate_SWBM_codes}} to regenerate any associated
\code{*_txt} label columns.
}
\examples{
\dontrun{
polygon_fields <- read_SWBM_polygon_file(scen$polygon_file, landcover_desc, tributary_desc)
polygon_fields <- SWBM_no_pumping(polygon_fields)
}
}
