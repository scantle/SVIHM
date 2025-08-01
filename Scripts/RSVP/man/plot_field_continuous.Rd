% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{plot_field_continuous}
\alias{plot_field_continuous}
\title{Plot a Continuous Field Attribute on a Map}
\usage{
plot_field_continuous(
  data_df,
  stress_period,
  field_shp_file = file.path(data_dir["ref_data_dir", "loc"], "Landuse_20190219.shp"),
  prefix = "ID_",
  origin_date = as.Date("1990-09-30"),
  plot_title = NA,
  value_label = "Value",
  viridis_option = "viridis",
  direction = 1,
  na_color = "grey40"
)
}
\arguments{
\item{data_df}{A data.frame with a \code{Stress_Period} Date column and one \code{<prefix><fieldID>}
column per field.}

\item{stress_period}{Integer, Date, or character.  Which period to plot:
\describe{
\item{\code{numeric}}{treated as a stress-period index (months since \code{origin_date}, 1-based) and converted to Date via \code{mftime2date()}.}
\item{\code{character}}{coerced to Date with \code{as.Date()}.}
\item{\code{Date}}{used directly.}
}}

\item{field_shp_file}{Character. Path to the SWBM field shapefile. Defaults to
\code{file.path(data_dir["ref_data_dir","loc"], "Landuse_20190219.shp")}.}

\item{prefix}{Character. Column-name prefix in \code{data_df} (default \code{"ID_"}).}

\item{origin_date}{Date. Reference date for numeric stress periods (default \code{as.Date("1990-09-30")}).}

\item{plot_title}{Character. Plot title (default NA)}

\item{value_label}{Character. Legend title for the fill scale (default \code{"Value"}).}

\item{viridis_option}{Character. One of \code{"viridis","magma","plasma","cividis"} (default \code{"viridis"}).}

\item{direction}{Integer 1 or -1.  Scale direction (default 1).}

\item{na_color}{Character. Color for \code{NA} fields (default \code{"grey40"}).}
}
\value{
A \code{ggplot} object with \code{geom_sf(aes(fill = value))}.
}
\description{
Reads a field-polygon shapefile, merges in a single-period continuous attribute
(e.g. curtailment fraction) from a wide data.frame, and plots it with a Viridis color scale.
}
\examples{
\dontrun{
# Assuming `curtail_df` from create_SWBM_curtailment_df()
p <- plot_field_continuous(
  data_df      = curtail_df,
  stress_period= 5,
  value_label  = "Curtailment Fraction"
)
print(p)
}
}
