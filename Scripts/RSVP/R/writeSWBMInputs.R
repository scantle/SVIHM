
#-------------------------------------------------------------------------------------------------#

#' Write SWBM Drains Input Files (Drain_m3day.txt, Drains_initial_m3day.txt)
#'
#' @param num_stress_periods Number of model stress periods, determining the length of values array
#' to be written to the drains file
#' @param output_dir directory to write the files in
#' @param values Values to write to drain files, by default (NULL) will write zeroes
#' @param drains_filename Optional, default "Drains_m3day.txt"
#' @param drains_iniital_filename Optional, default "Drains_initial_m3day.txt"
#' @param verbose T/F write status info to console (default: TRUE)
#'
#' @return None
#' @export
#'
#' @examples write_SWBM_drain_files(num_stress_periods=336,
#' @examples output_dir = getwd(),
#' @examples values = NULL,
#' @examples drains_filename = "Drains_m3day.txt",
#' @examples init_filename = "Drains_initial_m3day.txt",
#' @examples verbose = TRUE)
#'
write_SWBM_drain_files <- function(num_stress_periods,
                                   output_dir,
                                   values=NULL,
                                   drains_filename="Drains_m3day.txt",
                                   init_filename="Drains_initial_m3day.txt",
                                   verbose=TRUE) {
  if (is.null(values)) {
    values <- rep(0, num_stress_periods)
  } else if (length(values) != num_stress_periods) {
    stop('values array too short - need one value for each stress period.')
  }
  drains_vector = c("#Initial Drain Flow", values)

  if (verbose) {message(paste('Writing file: ', drains_filename))}

  write.table(drains_vector, file = file.path(output_dir, drains_filename),
              sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)

  if (verbose) {message(paste('Writing SWBM file: ', init_filename))}
  write.table(drains_vector, file = file.path(output_dir, init_filename),
              sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)
}

#-------------------------------------------------------------------------------------------------#

#' Write SWBM General Inputs File
#'
#' Writes all basecase values by default
#'
#' @param num_stress_periods Number of model stress periods
#' @param output_dir directory to write the files to
#' @param filename general input filename, general_inputs.txt by default
#' @param recharge_scenario character
#' @param flow_scenario character
#' @param alf_irr_stop_mo integer Calendar month
#' @param alf_irr_stop_day integer
#' @param early_cutoff_flag character
#' @param curtailment_scenario character
#' @param curtail_start_mo integer
#' @param curtail_start_day integer
#' @param landuse_scenario character
#' @param verbose T/F write status info to console (default: TRUE)
#'
#' @return None
#' @export
#'
#' @examples
write_SWBM_gen_inputs_file <- function(num_stress_periods,
                                       output_dir,
                                       filename="general_inputs.txt",
                                       recharge_scenario="Basecase",
                                       flow_scenario="Basecase",
                                       alf_irr_stop_mo=8,
                                       alf_irr_stop_day=31,
                                       early_cutoff_flag="AllYears",
                                       curtailment_scenario="NoCurtail",
                                       curtail_start_mo=8,
                                       curtail_start_day=15,
                                       landuse_scenario="basecase",
                                       verbose=TRUE) {

  # Convert months from calendar months to WY months
  if(alf_irr_stop_mo<9){alf_irr_stop_mo = alf_irr_stop_mo + 3
  }else{alf_irr_stop_mo = alf_irr_stop_mo - 9}

  if(curtail_start_mo<9){curtail_start_mo = curtail_start_mo + 3
  }else{curtail_start_mo = curtail_start_mo - 9}

  gen_inputs = c(
    paste("2119  167", num_stress_periods,
          "440  210  1.4 UCODE",
          "! num_fields, num_irr_wells, num_stress_periods, nrow, ncol, RD_Mult, UCODE/PEST",
          sep = "  "),
    paste(recharge_scenario, flow_scenario,
          "! Basecase/MAR/ILR/MAR_ILR, Basecase/Flow_Lims",
          sep = "  "),
    paste(alf_irr_stop_mo, alf_irr_stop_day, early_cutoff_flag,
          "! alf_irr_stop_mo  alf_irr_stop_day early_alf_cutoff_scenario",
          sep = "  "),
    paste(curtailment_scenario, curtail_start_mo, curtail_start_day,
          "! curtailment_scenario curtail_start_mo curtail_start_day",
          sep = "  "),
    paste(landuse_scenario, "! Basecase/Major_NatVeg")
  )

  if (verbose) {message(paste('Writing SWBM file: ', filename))}
  write.table(gen_inputs, file = file.path(output_dir, filename),
              sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)
}

#-------------------------------------------------------------------------------------------------#

#' Write SWBM Instream Flow Available Ratio File
#'
#' @param avail_monthly Dataframe of ratio of instream flow available over the model period, as
#' generated by \code{\link{build_cdfw_instream_flow_cal}}
#' @param output_dir directory to write the files to
#' @param filename general input filename, instream_flow_available_ratio.txt by default
#' @param verbose T/F write status info to console (default: TRUE)
#'
#' @return
#' @export
#'
#' @examples
write_SWBM_instream_available_file <- function(avail_monthly, output_dir,
                                               filename="instream_flow_available_ratio.txt",
                                               verbose=TRUE) {

  if (verbose) {message(paste('Writing SWBM file: ', filename))}
  write.table(avail_monthly, file = file.path(output_dir, filename),
              sep = " ", quote = FALSE, col.names = F, row.names = FALSE)
}

#-------------------------------------------------------------------------------------------------#

#' Write SWBM Crop Coefficient (Kc) File
#'
#' @param kc_df Dataframe of days and coefficients (see
#'   \code{\link{gen_daily_binary_crop_coefficients}} and
#'   \code{\link{gen_daily_curve_crop_coefficients}})
#' @param output_dir directory to write the files to
#' @param filename general input filename, instream_flow_available_ratio.txt by default
#' @param verbose T/F write status info to console (default: TRUE)
#'
#' @return
#' @export
#'
#' @examples
write_SWBM_crop_coefficient_file <- function(kc_df, output_dir, filename, verbose=TRUE) {
  if (verbose) {message(paste('Writing SWBM Crop Coefficient file: ', filename))}

  kc_df$day <- format(kc_df$Date, '%d/%m/%Y')
  kc_df$kc = round(kc_df$kc, 4)

  write.table(kc_df[,c('kc','day')],
              file = file.path(output_dir, filename),
              sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)
}

#-------------------------------------------------------------------------------------------------#

#' Build Field-value Data Frame
#'
#' Empty dataframe of field-level values, for every month between the start and end dates. This
#' format is common to many temporal SWBM inputs
#'
#' @param nfields number of fields in the Soil Water Balance Model simulation
#' @param model_start_date Start date of simulation
#' @param model_end_date End date of simulation
#' @param default_values Default values, given by field (optional, defaults to NA)
#'
#' @return Dataframe of stress periods (rows) for each field (columns)
#' @export
#'
#' @examples
#' # Dates
#' start_date <- get_model_start(1991)
#' end_date <- as.Date(floor_date(Sys.Date(), 'month')-1)
#' # Fields
#' nfields <- 100
#' # For example: land use
#' default_lu <- rep(swbm_lutype['Alfalfa','Code'], nfields)
#'
#' lu_df <- swbm_build_field_value_df(nfields, start_date, end_date, default_lu)
#'
swbm_build_field_value_df <- function(nfields, model_start_date, model_end_date, default_values=NA) {

  # Define Stress Periods & IDs
  vect_sp <- seq.Date(model_start_date, model_end_date, by='month')
  id_list <- paste0("ID_", 1:nfields)

  # Error check
  if (!is.na(default_values) & (!length(default_values) == length(id_list))) {
    stop('Default value list must be same length as # of fields in poly_table')
  }

  # Create dataframe of irrigation types for each field (columns) for each stress period (rows)
  df <- data.frame(matrix(default_values,
                          nrow=length(vect_sp),
                          ncol=length(id_list),
                          byrow = T))
  colnames(df) <- id_list # assign column names
  df$Stress_Period <- vect_sp

  #-- Stress period column first
  df <- df[c('Stress_Period', names(df)[names(df) != 'Stress_Period'])]

  return(df)
}

# ------------------------------------------------------------------------------------------------#

#' Update Land Use Table With New Data
#'
#' @param lu_df Dataframe of field land uses for each stress period, possibly created by
#'              [swbm_build_field_value_df()]
#' @param update_table Table of land use updates, where each column has a name of the form
#'                     'landuse_XX' where XX is year 20XX, each row is a field, in order, and the
#'                     cells contain valid land use codes (see [swbm_lutype])
#' @param verbose T/F write # of yearly changed to console (default: TRUE)
#'
#' @return Updated lu_df
#' @export
#'
#' @examples
#' # Dates
#' start_date <- get_model_start(1991)
#' end_date <- as.Date(floor_date(Sys.Date(), 'month')-1)
#' # Fields
#' nfields <- 100
#' # For example: land use
#' default_lu <- rep(swbm_lutype['Alfalfa','Code'], nfields)
#'
#' lu_df <- swbm_build_field_value_df(nfields, start_date, end_date, default_lu)
#'
#' # Make update table
#' updates <- data.frame('ID'=1:nfields, 'landuse_11'=swbm_lutype['Pasture','Code'])
#'
#' # Do updates
#' lu_df <- swbm_landuse_update(lu_df, updates)
#'
swbm_landuse_update <- function(lu_df, update_table, verbose=TRUE) {

  # update_table is assumed to have columns of 'landuse_XX' where XX is year 20XX
  for (col in names(update_table)[2:length(names(update_table))]) {
    col_yr <- 2000 + as.numeric(strsplit(col, '_')[[1]][2])
    sps_after_change <- nrow(lu_df[year(lu_df$Stress_Period) >= col_yr,])

    # Get changes
    change_df <- update_table[update_table[,col]>0,c('ID',col)]

    # Create change matrix
    diff <- matrix(change_df[,col],
                   nrow=sps_after_change,
                   ncol=length(change_df[,col]),
                   byrow = T)

    # Report
    if (verbose) {
      message(paste('Column:', col, ' Year:', col_yr, '- ', length(change_df[,col]), 'Fields Updated'))
    }

    # Update
    lu_df[year(lu_df$Stress_Period) >= col_yr, names(lu_df) %in% paste0('ID_',change_df$ID)] <- diff
  }
  return(lu_df)
}

# ------------------------------------------------------------------------------------------------#

#' Update Irrigation Type Table with New Data
#'
#' @param irrtype_df Dataframe of field irrigation types for each stress period, possibly created by
#'                   [swbm_build_field_value_df()]
#' @param update_table Dataframe with columns 'ID' of field IDs and 'Year' of the year (integer XXXX)
#'                     when the field switched to center pivot. Zeros can be used to denote that it
#'                     has never switched.
#' @param verbose T/F write # of yearly changed to console (default: TRUE)
#'
#' @return Updated irrtype_df
#' @export
#'
#' @examples
#' # Dates
#' start_date <- get_model_start(1991)
#' end_date <- as.Date(floor_date(Sys.Date(), 'month')-1)
#' # Fields
#' nfields <- 100
#' # For example: land use
#' default_irr <- rep(swbm_irrtype['Wheel Line','Code'], nfields)
#'
#' lu_df <- swbm_build_field_value_df(nfields, start_date, end_date, default_irr)
#'
#' # Make update table - magically all fields updated to center pivot in 2005!
#' updates <- data.frame('ID'=1:nfields, 'Year'=2005)
#'
#' # Do updates
#' lu_df <- swbm_irrtype_cp_update(lu_df, updates)
#'
swbm_irrtype_cp_update <- function(irrtype_df, update_table, verbose=TRUE) {

  # Switch is to center pivot
  new_code <- swbm_irrtype['Center Pivot', 'Code']

  #-- Loop over years where changes occur
  for (yr in unique(update_table[update_table$Year>0,]$Year)) {  # when year==0 no data/change
    id_cols <- update_table$ID[update_table$Year == yr]

    if (verbose) {
      message(paste('Year:', yr, '- ', length(id_cols), 'fields switched to center pivot (code =',new_code,')'))
    }

    irrtype_df[year(irrtype_df$Stress_Period) >= yr, paste0('ID_', id_cols)] <- new_code
  }

  return(irrtype_df)
}
