% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modelEval.R
\name{print.split_sample_stats}
\alias{print.split_sample_stats}
\title{Print Split Sample Statistics as Formatted Line}
\usage{
\method{print}{split_sample_stats}(sampledf, group = NA, side)
}
\arguments{
\item{sampledf}{DF of statistics computed by \code{\link{calc_split_sample_stats}} or
\code{\link{calc_split_sample_stats.grouped}}}

\item{group}{Group name (optional, default NA)}

\item{side}{'Pre' or 'Post'}
}
\value{
String of statistical results
}
\description{
Print Split Sample Statistics as Formatted Line
}
\examples{
\dontrun{
# Read in head observation data
#-- HOB data
hob_locs <- read.csv('hob_wells.csv', row.names=1, stringsAsFactors = F)
hob <- import_HOB(hob_input = 'SVIHM.hob',
                  hob_output = 'HobData_SVIHM.dat',
                  origin_date = origin_date)
hob <- hob[order(hob$row, hob$column),]

# Date to split data (i.e. pre-post calibration)
cal_split <- as.Date('2012-10-01')

hob_compare <- calc_split_sample_stats(dates = hob$date,
                                       sim = hob$sim,
                                       obs = hob$hobs,
                                       split_date = cal_split,
                                       FUNs = list(NSE, rmse, KGE),
                                       FUN_names = c('NSE', 'RMSE', 'KGE'))
# Finally ready
statstr <- print.split_sample_stats(hob_compare, 'Post')
print(statstr)
}
}
\seealso{
\code{\link{calc_split_sample_stats}}, \code{\link{calc_split_sample_stats.grouped}}
}
\author{
Leland Scantlebury
}
