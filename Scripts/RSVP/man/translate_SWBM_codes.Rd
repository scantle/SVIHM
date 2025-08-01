% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{translate_SWBM_codes}
\alias{translate_SWBM_codes}
\title{Translate SWBM Code Columns into Text Labels (In-Place)}
\usage{
translate_SWBM_codes(df, landcover_desc = NULL, tributary_desc = NULL)
}
\arguments{
\item{df}{A \code{data.frame} that may include SWBM code columns.}

\item{landcover_desc}{Optional \code{data.frame} with columns \code{id} and \code{Landcover_Name},
required to translate \code{SWBM_LU}.}

\item{tributary_desc}{Optional \code{data.frame} with columns \code{subws_ID} and \code{subws_name},
required to translate \code{subws_ID}.}
}
\value{
The same \code{df}, but with any of the following new columns added and placed
immediately after their source columns:
\describe{
\item{\code{LU_txt}}{Character labels for \code{SWBM_LU}.}
\item{\code{IRR_txt}}{Character labels for \code{SWBM_IRR}.}
\item{\code{WS_txt}}{Character labels for \code{WATERSOURC}.}
\item{\code{subws_txt}}{Character labels for \code{subws_ID}.}
}
Columns are only added if the corresponding code column is present in \code{df}
and the needed lookup (\code{landcover_desc} or \code{tributary_desc}) is supplied.
}
\description{
Given a data.frame that may contain SWBM code columns, adds corresponding
"_txt" columns with human-readable labels, and inserts each new text column
immediately after its source code column.  The function handles these mappings:
\itemize{
\item \code{SWBM_LU}      to \code{LU_txt}, using \code{landcover_desc$id} to \code{landcover_desc$Landcover_Name}
\item \code{SWBM_IRR}     to \code{IRR_txt}, using internal \code{swbm_irr_key}
\item \code{WATERSOURC}   to \code{WS_txt}, using internal \code{watersource_key}
\item \code{subws_ID}     to \code{subws_txt}, using \code{tributary_desc$subws_ID} to \code{tributary_desc$subws_name}
}
}
\examples{
\dontrun{
data(swbm_irr_key); data(watersource_key)
landcov <- read.table("landcover_table.txt", header=TRUE)
trib    <- read.table("trib_desc.txt", header=TRUE)
poly    <- read.table("polygons.txt", header=TRUE)
poly2   <- translate_SWBM_codes(poly, landcov, trib)
head(poly2[c("SWBM_LU","LU_txt","SWBM_IRR","IRR_txt")])
}
}
