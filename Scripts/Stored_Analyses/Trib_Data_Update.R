library(RSVP)

#-------------------------------------------------------------------------------------------------#
# Setup

update_streams <- c('East_Fork', 'South_Fork', 'Sugar', 'French', 'Shackleford')
update_files <- c('East_Fork_F26050_Stream_Flow_Rate_Daily_Means_Ending_Midnight_09292025.csv',
                  'South_Fork_F28100_Stream_Flow_Rate_Daily_Means_Ending_Midnight_09292025.csv',
                  'SGN_F25890_Stream_Flow_Rate_Daily_Means_Ending_Midnight_09292025.csv',
                  'FCC_F25650_Stream_Flow_Rate_Daily_Means_Ending_Midnight_09292025.csv',
                  'SCK_F25484_Stream_Flow_Rate_Daily_Means_Ending_Midnight_09292025.csv')

# folder where new streamflow data is located on your computer
new_dir <- file.path('C:/Users/lelan/Box/Research/Scott Valley/Data/Streamflow')
str_dir <- data_dir['sf_reg_dir','loc']

for (i in 1:length(update_streams)) {
  nm <- update_streams[i]
  # Read in previous data
  prev <- read.table(file.path(str_dir,stream_metadata[stream_metadata$name==nm, 'daily_file']),
                     header = T,
                     stringsAsFactors = F,
                     sep = '\t')

  # Read in new data
  new <- read.csv(file.path(new_dir, update_files[i]), skip = 9, header=F,
                  col.names = c('Date', 'mean_cfs', 'quality'))

  # Parse dates
  new$Date <- as.Date(strptime(new$Date, format = "%m/%d/%Y %H:%M:%S", tz = "UTC"))
  # prev = "M/D/YYYY" (tab file) -> Date
  prev$Date <- as.Date(prev$Date, format = "%m/%d/%Y")

  # Extract numeric quality code before ":" and align columns
  # e.g., "1: Good data" -> 1
  new$Qual_Flag <- as.integer(sub(":.*$", "", trimws(new$quality)))
  new$Data_Source <- 'WDL'

  # Keep only the columns that match the old file
  new_keep <- new[, c("Date", "mean_cfs", "Qual_Flag", "Data_Source")]

  # Trim old data to strictly before the earliest new date
  cutoff <- min(new_keep$Date, na.rm = TRUE)
  prev_trim <- prev[prev$Date < cutoff, c("Date", "mean_cfs", "Qual_Flag", "Data_Source")]

  # Append and sort
  updated <- rbind(prev_trim, new_keep)
  updated <- updated[order(updated$Date), ]

  # In case of accidental duplicates, keep the *last* (i.e., new) record per date
  # (Should be unnecessary because of the < cutoff filter, but safe.)
  updated <- updated[!duplicated(updated$Date, fromLast = TRUE), ]

  # Convert back to legacy "%m/%d/%Y" string format
  updated$Date <- format(updated$Date, "%m/%d/%Y")

  # Write back to the same tab-delimited file (no quotes)
  out_path <- file.path(str_dir, stream_metadata[stream_metadata$name == nm, "daily_file"])
  write.table(updated, file = out_path, sep = "\t", row.names = FALSE, quote = FALSE)

  message(sprintf("[%s] Updated %s rows written to: %s",
                  nm, nrow(updated), out_path))

}
