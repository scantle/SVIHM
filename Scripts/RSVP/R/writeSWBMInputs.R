
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
write_SWBM_gen_inputs_file_orig <- function(num_stress_periods,
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
write_SWBM_gen_inputs_file <- function(output_dir,
                                       num_stress_periods,
                                       filename="general_inputs.txt",
                                       modelName = "SVIHM",
                                       WYstart = 1991,
                                       npoly = 2119,
                                       nlandcover = 6,
                                       nAgWells = 167,
                                       nMuniWells = 0,
                                       nSubws = 9,
                                       inflow_is_vol = TRUE,
                                       nSFR_inflow_segs = 12,
                                       nrows = 440,
                                       ncols = 210,
                                       RD_Mult = 1.4,
                                       calib_software = "UCODE",
                                       verbose=TRUE) {


  gen_inputs = c(
    paste(modelName, WYstart, npoly, nlandcover, nAgWells, nMuniWells, nSubws,
          "! modelName, WYstart, npoly, nlandcover, nAgWells, nMuniWells, nSubws",
          sep = "  "),
    paste(inflow_is_vol, nSFR_inflow_segs, num_stress_periods, nrows, ncols,
          "! inflow_is_vol, nSFR_inflow_segs, nmonths, nrows, ncols",
          sep = "  "),
    paste(RD_Mult, calib_software, "! RD_Mult, UCODE/PEST",
          sep = "  ") )

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
#' @param filename input filename, instream_flow_available_ratio.txt by default
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
#' @param filename input filename, instream_flow_available_ratio.txt by default
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

#' Write SWBM File Partitioning Subwatershed Surface Flows to Stream Inflows
#'
#' @param sfr_component Dataframe of months and either: 1) subwatershed inflow partition
#' fractions (see \code{\link{gen_monthly_sfr_flow_partition}}), or 2) inflows available
#' for irrigation, or 3) inflows NOT available for irrigation (e.g. reserved for environmental
#' flows) (for items 2 and 3 see \code{\link{process_monthly_sfr_inflows}}).
#' @param output_dir directory to write the files to
#' @param filename Writes to this filename
#' @param verbose T/F write status info to console (default: TRUE)
#'
#' @return
#' @export
#'
#' @examples
#'
write_SWBM_SFR_inflow_files <- function(sfr_component, output_dir, filename, verbose=TRUE) {
  if (verbose) {message(paste('Writing SWBM SFR Handling file: ', filename))}

  # if(filename=="SFR_subws_flow_partitioning.txt"){
    sfr_component$modelMonth <- as.character(format(x = sfr_component$modelMonth, format= '%b-%Y'))

    write.table(sfr_component,
                file = file.path(output_dir, filename),
                sep = " ", quote = FALSE, col.names = TRUE, row.names = FALSE)

  # }


}

#-------------------------------------------------------------------------------------------------#

#' Write SFR Diversions File
#'
#' Generates text file specifying the number, stream segment, priority handling (in MODFLOW), and
#' unchanging flowrate of each diversion point in the stream network.
#' Default is currently (Nov 2022) configured to specify diversion points for the SVID ditch and
#' Farmers ditch, with 0 flow in them (as ditch diversions are currently  handled in a
#' separate module).
#'
#' @param num_divs Number of diversions in the stream network
#' @param iseg_for_divs The stream segment associated with each diversion
#' @param iprior_for_divs Specifies priority handling for diversion in MODFLOW.
#' @param flow_for_divs Uniform flowrate for the diversion.
#'
#' @return
#' @author Claire Kouba, Leland Scantlebury
#' @export
#'
#' @examples

write_SWBM_SFR_diversions_file <- function(filename = "SFR_diversions.txt",
                                           output_dir,
                                           num_divs = 2,
                                           iseg_for_divs = c(3,10), # Stream segment of diversion
                                           iprior_for_divs = c(0,0), # Modflow param. for calc.
                                           flow_for_divs = c(0,0),    # Flowrate (units? cfs?)
                                           verbose = T) {

  sfr_divs = c(
    paste(num_divs, "    ! Number of diversions", sep = "  "),
    paste(paste(iseg_for_divs, collapse = "  "), "    ! ISEG for diversions",
          sep = "  "),
    paste(paste(iprior_for_divs, collapse = "  "), "    ! IPRIOR for diversions",
          sep = "  "),
    paste(paste(flow_for_divs, collapse = "  "), "    ! FLOW for diversions",
          sep = "  "))

  if (verbose) {message(paste('Writing SWBM file: ', filename))}
  write.table(sfr_divs, file = file.path(output_dir, filename),
              sep = " ", quote = FALSE, col.names = FALSE, row.names = FALSE)

}


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


# ------------------------------------------------------------------------------------------------#

#' Write file specifying pumping volumes in agricultural wells
#'
#' @param scenario_name Name of  management scenario. Default is historical basecase or "basecase".
#'
#' @return none; writes input file
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
write_ag_pumping_file <- function(start_date, n_stress, output_dir,
                                  ag_pumping_data = NA,
                                  filename = "ag_well_specified_volume.txt") {

  stress_period_vector = format(seq.Date(from=start_date, length.out=n_stress, by="month"),
                                format = "%b%Y")

  # get vector of well IDs
  ag_wells = read.table(file.path(data_dir['time_indep_dir','loc'],"ag_well_summary.txt"),
                        header = T)
  well_ids = ag_wells$well_id


  if(is.na(ag_pumping_data)){
    #If no ag pumping data provided, set all stress periods to FALSE (no specified pumping volumes)
    # and pump volumes to 0
    specify_pumping = rep(FALSE, length(stress_period_vector))
    pumping_volumes = data.frame(matrix(data = 0, nrow = n_stress, ncol = length(well_ids)))

    ag_pumping_tab = cbind(stress_period_vector, specify_pumping, pumping_volumes)
    colnames(ag_pumping_tab) = c("well_id", "specify_pumping", well_ids)
  } else {
    # placeholder - if we receive specified ag pumping data (or want to specifically dictate it for
    # a scenario), file design can go here.
  }

  write.table(ag_pumping_tab, file = file.path(output_dir, filename), sep = "  ", quote = F,
              col.names = TRUE, row.names = FALSE)
}


# ------------------------------------------------------------------------------------------------#

#' Write file specifying pumping volumes in municipal wells
#'
#' @param scenario_name Name of  management scenario. Default is historical basecase or "basecase".
#'
#' @return none; writes input file
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
write_muni_pumping_file <- function(start_date, n_stress, output_dir,
                                  muni_pumping_data = NA,
                                  filename = "muni_well_specified_volume.txt") {

  stress_period_vector = format(seq.Date(from=start_date, length.out=n_stress, by="month"),
                                format = "%b%Y")

  # get vector of well IDs
  muni_wells = read.table(file.path(data_dir['time_indep_dir','loc'],"muni_well_summary.txt"),
                        header = T)
  well_ids = muni_wells$well_id


  if(is.na(muni_pumping_data)){
    #If no muni pumping data provided, set all stress periods to FALSE (no specified pumping volumes)
    # and pump volumes to 0
    specify_pumping = rep(FALSE, length(stress_period_vector))
    pumping_volumes = data.frame(matrix(data = 0, nrow = n_stress, ncol = length(well_ids)))

    muni_pumping_tab = cbind(stress_period_vector, specify_pumping, pumping_volumes)
    colnames(muni_pumping_tab) = c("well_id", "specify_pumping", well_ids)
  } else {
    # placeholder - if we receive specified ag pumping data (or want to specifically dictate it for
    # a scenario), file design can go here.
  }

  write.table(muni_pumping_tab, file = file.path(output_dir, filename), sep = "  ", quote = F,
              col.names = TRUE, row.names = FALSE)
}


# ------------------------------------------------------------------------------------------------#

#' Write file specifying irrigation curtailment regulations/practices
#'
#' @param scenario_name Name of  management scenario. Default is historical basecase or "basecase".
#'
#' @return curtail_tab
#' @export
#'
#' @examples
#'
#'
#'
write_SWBM_curtailment_file <- function(scenario_name = "basecase",
                                        start_date,
                                        n_stress) {

  # reference build_field_value_df



  # # Switch is to center pivot
  # new_code <- swbm_irrtype['Center Pivot', 'Code']
  #
  # #-- Loop over years where changes occur
  # for (yr in unique(update_table[update_table$Year>0,]$Year)) {  # when year==0 no data/change
  #   id_cols <- update_table$ID[update_table$Year == yr]
  #
  #   if (verbose) {
  #     message(paste('Year:', yr, '- ', length(id_cols), 'fields switched to center pivot (code =',new_code,')'))
  #   }
  #
  #   irrtype_df[year(irrtype_df$Stress_Period) >= yr, paste0('ID_', id_cols)] <- new_code
  # }
  #
  # return(irrtype_df)
}



# ------------------------------------------------------------------------------------------------#

#' Write file specifying land use types for each field, for each stress period
#'
#' @param scenario_name Name of  management scenario. Default is historical basecase or "basecase".
#'
#' @return landcover_tab
#' @export
#'
#' @examples
#'
#'
#'
write_SWBM_landcover_file <- function(scenario_id = "basecase",
                                      output_dir, start_date, end_date) {
  recognized_scenarios = c("basecase")
  output_filename = "polygon_landcover_ids.txt"

  # pull reference land cover
  poly_tab = read.table(file.path(data_dir["time_indep_dir","loc"],"polygons_table.txt"),
                      header = T)
  num_unique_fields = length(unique(poly_tab$SWBM_id))
  # Check for polygon ID number continuity
  if(nrow(poly_tab) != num_unique_fields){
    print("Caution: polygons (fields) info table contains replicated or missing ID numbers")
  }
  # generate a stress-period-by-field table (wide format)
  field_df = swbm_build_field_value_df(nfields = num_unique_fields,
                                       model_start_date = start_date,
                                       model_end_date = end_date)

  # Build default (time-invarying) land use table
  for(i in 1:num_unique_fields){
    field_id = poly_tab$SWBM_id[i]
    # set entire model period to the baseline land use from reference poly_tab file
    field_df[,paste0("ID_",field_id)] = poly_tab$SWBM_LU[poly_tab$SWBM_id==field_id]
  }

  if(tolower(scenario_id) == "basecase"){
    landcover_output = field_df
  }

  # if(tolower(scenario_id=="natveg")){} # Placeholder :
  # Replace default with new landcover file, and update recognized_scenarios
  # possible steps: read in fields and potentially adjudicated zone
  # potentially read in schedule of land use updates

  if(!(tolower(scenario_id) %in% recognized_scenarios)){
    print("Warning: specified landuse scenario not recognized in current codebase. Using default land cover file")
    landcover_output = field_df
  }

  write.table(landcover_output, file = file.path(output_dir, output_filename),
              sep = "  ", quote = FALSE, col.names = TRUE, row.names = FALSE)

}


# ------------------------------------------------------------------------------------------------#

#' Write file specifying Managed Aquifer Recharge (applied irrigation) volumes for each field, for each stress period
#'
#' @param scenario_name Name of  management scenario. Default is historical basecase or "basecase".
#'
#' @return none, saves MAR_volumes.txt
#' @export
#'
#' @examples
#'
#'
#'
write_SWBM_MAR_vol_file <- function(scenario_id = "basecase",
                                      output_dir, start_date, end_date) {
  recognized_scenarios = c("basecase")
  output_filename = "MAR_volumes.txt"

  # pull reference land cover
  poly_tab = read.table(file.path(data_dir["time_indep_dir","loc"],"polygons_table.txt"),
                        header = T)
  num_unique_fields = length(unique(poly_tab$SWBM_id))
  # Check for polygon ID number continuity
  if(nrow(poly_tab) != num_unique_fields){
    print("Caution: polygons (fields) info table contains replicated or missing ID numbers")
  }

  # generate a stress-period-by-field table (wide format)
  field_df = swbm_build_field_value_df(nfields = num_unique_fields,
                                       model_start_date = start_date,
                                       model_end_date = end_date)

  # Build default MAR volumes table (no MAR applications)
  field_column_selector = grepl(pattern = "ID", x = colnames(field_df))
  field_df[,field_column_selector] = 0

  if(tolower(scenario_id) == "basecase"){
    mar_vol_output = field_df
  }
  # if(tolower(scenario_id=="natveg")){} # Placeholder :
  # Replace default with new MAR vol file, and update recognized_scenarios
  # possible steps: read in fields and potentially adjudicated zone
  # potentially read in schedule of land use updates

  if(!(tolower(scenario_id) %in% recognized_scenarios)){
    print("Warning: specified MAR scenario not recognized in current codebase. Using basecase MAR file (no irrigation applied for MAR)")
    mar_vol_output = field_df
  }
  write.table(mar_vol_output, file = file.path(output_dir, output_filename),
              sep = "  ", quote = FALSE, col.names = TRUE, row.names = FALSE)

}


# ------------------------------------------------------------------------------------------------#

#' Write file specifying Managed Aquifer Recharge (applied irrigation) volumes for each field, for each stress period
#'
#' @param scenario_name Name of  management scenario. Default is historical basecase or "basecase".
#'
#' @return none, saves MAR_volumes.txt
#' @export
#'
#' @examples
#'
#'
#'
write_SWBM_curtailment_file <- function(scenario_id = "basecase",
                                    output_dir, start_date, end_date) {
  library(plyr) # for "count" function
  library(sf)
  m2_to_acres = 1/4046.856

  recognized_scenarios = c("basecase")
  output_filename = "curtailment_fractions.txt"
  print(paste("Writing SWBM file:", output_filename))

  # function to extract all APN values from LCS data as a vector
  get_all_apns = function(apn_tab){
    apn_col_nums = grep(pattern = "APN", x = colnames(apn_tab))
    apn_vals_raw = as.vector(as.matrix(apn_tab[,apn_col_nums]))
    take_out_vals = c("", "0", " 0", "0 ", NA)
    apn_vals = apn_vals_raw[!(apn_vals_raw) %in% take_out_vals]
    apn_vals = trimws(apn_vals)
    return(apn_vals)
  }

  # pull reference land cover
  poly_tab = read.table(file.path(data_dir["time_indep_dir","loc"],"polygons_table.txt"),
                        header = T)
  num_unique_fields = length(unique(poly_tab$SWBM_id))
  # Check for polygon ID number continuity
  if(nrow(poly_tab) != num_unique_fields){
    print("Caution: polygons (fields) info table contains replicated or missing ID numbers")
  }

  # generate a stress-period-by-field table (wide format)
  field_df = swbm_build_field_value_df(nfields = num_unique_fields,
                                       model_start_date = start_date,
                                       model_end_date = end_date)

  # Build 0-curtailment fraction table
  field_column_selector = grepl(pattern = "ID", x = colnames(field_df))
  field_df[,field_column_selector] = 0

  ## Basecase curtailment scenario -----------------------------------------
  # build historical curtailments if scenario is Basecase or unrecognized
  if(tolower(scenario_id) == "basecase" |
     !(tolower(scenario_id) %in% recognized_scenarios) ){
    ref_dir = data_dir["ref_data_dir","loc"]

    # read and process 2021 and 2022 curtailment input file


    # PLACEHOLDER: need 2021 curtailment input


    # Read in 2022 curtailment input
    curtail_frac = read.table(file.path(ref_dir,
                                        "LCS Tracking_2022 growing season_anonymized for SVIHM.txt"),
                               header = T, sep = "\t")
    # Assign application #
    curtail_frac$app_id = 1:nrow(curtail_frac)
    # trim whitespace from APN numbers
    apn_col_nums = grep(pattern = "APN", x = colnames(curtail_frac))
    apn_mat = as.matrix(curtail_frac[,apn_col_nums])
    curtail_frac[,apn_col_nums] = trimws(apn_mat)
    #reassign apn_mat to be the cleaned apn values
    apn_mat = as.matrix(curtail_frac[,apn_col_nums])
    # add leading 0 if it is missing
    lead_char = substr(x = apn_mat, start = 1, stop = 1)
    lead_0_missing = !(lead_char %in% c("0", NA, ""))
    apn_mat[lead_0_missing] = paste0("0",apn_mat[lead_0_missing])
    curtail_frac[,apn_col_nums] = apn_mat
    # extract all APN values from LCS curtailment data
    all_apns = get_all_apns(apn_tab = curtail_frac)

    # check to see if there are any repeat parcels
    apn_freq = count(all_apns); apn_freq[order(apn_freq$freq),]
    apn_repeats = apn_freq$x[apn_freq$freq>1]

    # check to see if any parcels in curtailment data are not present in the spatial data
    # read in parcels spatail data
    parcels = st_read(dsn = ref_dir, layer = "Scott_HUC8_Parcels_Public" )
    apns_not_in_spatial = sort(setdiff(all_apns, parcels$APN)) # 27 not
    #intersect(apns_not_in_spatial, apn_repeats) # 4 repeat parcels not in spatial
    # ok. 27 parcels in LCS data not present in spatial data.
    # which LCS reports are they in?
    lcs_problem_indices = data.frame(apn = apns_not_in_spatial,
                                     lcs_app_index = NA,
                                     lcs_app_index2 = NA)

    for(i in 1:length(apns_not_in_spatial)){
      apn = apns_not_in_spatial[i]
      # print(which(apn_mat==apn))
      if(length(which(apn_mat==apn)) == 1){
        lcs_problem_index = which(apn_mat==apn) %% nrow(apn_mat)
        lcs_problem_indices$lcs_app_index[i] = lcs_problem_index
      }
      if(length(which(apn_mat==apn)) == 2){
        lcs_problem_index = which(apn_mat==apn) %% nrow(apn_mat)
        lcs_problem_indices$lcs_app_index[i] = lcs_problem_index[1]
        lcs_problem_indices$lcs_app_index2[i] = lcs_problem_index[2]
      }
    }
    # sort(unique(as.vector(as.matrix(lcs_problem_indices[,c("lcs_app_index","lcs_app_index2")]))))
    # count(lcs_problem_indices)

    # appears to be problem APNs in 12 out of 44 LCS applications.


    # What number of curtailment % reduced numbers are missing? 15 for april, 13 for other months
    # curtail_frac$app_id[is.na(curtail_frac$April_pct_conserved)]
    # sum(is.na(curtail_frac$May_pct_conserved))
    # sum(is.na(curtail_frac$June_pct_conserved))
    # sum(is.na(curtail_frac$July_pct_conserved))
    # sum(is.na(curtail_frac$Aug_pct_conserved))

    # What number of Acres are missing? none
    # sum(is.na(curtail_frac$Acres))
    # which(is.na(curtail_frac$Acres))
    # setdiff(curtail_frac$app_id[is.na(curtail_frac$Acres)], curtail_frac$app_id[is.na(curtail_frac$April_pct_conserved)])

    # Compare spatial area of APNs to reported acreage
    curtail_frac$acres_to_apn_ratio = NA
    apps_w_acres = curtail_frac$app_id[!is.na(curtail_frac$Acres)]
    for(i in apps_w_acres){
      apn_list_30 = curtail_frac[curtail_frac$app_id==i, apn_col_nums]
      apn_list = apn_list_30[!(apn_list_30 %in% c("0","",NA))]
      apn_list_sp = parcels[parcels$APN %in% apn_list,]
      if(nrow(apn_list_sp)>0){
        apns = st_union(apn_list_sp)

        apn_acres = st_area(apns) * m2_to_acres
        app_area_frac = curtail_frac$Acres[i] / apn_acres
        curtail_frac$acres_to_apn_ratio[i] = app_area_frac
      }
    }

    # Table: month, parcel id, fraction curtailed

    # Simplified curtailment rules
    # For missing % conserved volumes, apply the average of all LCS apps for that month
    conserv_cols = grep(pattern = "conserv", x = colnames(curtail_frac))
    # calculate monthly conservation averages
    month_avg_cons =apply(X = curtail_frac[,conserv_cols], MARGIN = 2,
                          FUN = mean, na.rm=T)
    # add new columns to the end of curtail_frac for building model input (to avoid altering original columns with NAs)
    conserv_for_model = paste0(month.abb[4:10], "_pct_cons_model")
    # Assign values to model-input columns
    curtail_frac[,conserv_for_model] = curtail_frac[,conserv_cols]
    # fill NAs in each month with monthly average
    for(i in 1:length(conserv_cols)){
      month_cons = curtail_frac[,conserv_for_model[i]]
      month_cons[is.na(month_cons)] = month_avg_cons[i]
      # reset curtail_frac model input column for that month
      curtail_frac[,conserv_for_model[i]] = month_cons
    }

    # OK, now each application has:
    ## A curtailment conservation % for each month.
    ## A reported acreage-to-APN ratio.
    ## For each month, we can multiply the conservation % by the acre-to-apn ratio
    # to get a value to apply to each field within the APN footprint (minus unirrigated fields)
    # And we can area-overlap weight each of those.

    # spatially relate parcel numbers to fields
    # Read in fields spatial file for spatial relation
    svihm_fields = read_sf(dsn = file.path(data_dir["ref_data_dir","loc"],"Landuse_20190219.shp"))
    svihm_fields = st_transform(svihm_fields, st_crs(parcels))

    # Initialize tab to store area weighted info for each field
    area_wt_tab= data.frame(swbm_id = svihm_fields$Polynmbr)
    area_wt_tab$curtail_app_id = NA
    area_wt_tab$frac_area_overlap = 0

    plot_overlaps=F
    if(plot_overlaps==T){pdf("fields vs parcels in LCS data.pdf", width = 7, height = 7)}
    for(i in 1:nrow(curtail_frac)){
      apn_list_30 = curtail_frac[i,apn_col_nums]
      apn_list = apn_list_30[!(apn_list_30 %in% c("0","",NA))]

      apn_list_sp = parcels[parcels$APN %in% apn_list,]
      apns = st_union(apn_list_sp)
      if(sum(parcels$APN %in% apn_list)==0){print(paste("LCS app",i,"had no useable apns"))}
      if(sum(parcels$APN %in% apn_list)>0){

        fields_intersecting = svihm_fields[apns,] # find overlapping fields
        # plot overlapping polygons
        if(plot_overlaps == T){
          plot(apn_list_sp$geometry, col = "dodgerblue", border = "midnightblue")
          plot(fields_intersecting$geometry, add=T, col = rgb(.6,.1,.1,.5),border="firebrick")
          legend(x = "topright", legend = c("APNs", "SVIHM fields", "Overlap"),
                 fill = c("cornflowerblue", rgb(1,0,0,.5),rgb(.95,.5,.98,.5)),
                 border = c("midnightblue", "firebrick",NA))
        }

        # calculate area-weighted
        for(j in 1:nrow(fields_intersecting)){
          field = fields_intersecting[j,] # subset field for analysis
          # locate place to put result in output tab
          area_wt_tab_index = which(area_wt_tab$swbm_id==field$Polynmbr)
          # assign LCS application number to area_wt_tab
          area_wt_tab$curtail_app_id[area_wt_tab_index] = i
          # calculate spatial intersection of field and APN footprint
          overlap = st_intersection(x = field, y = apns)
          # assign the overlap fraction to the field
          area_wt_tab$frac_area_overlap[area_wt_tab_index] =
            st_area(overlap)/ st_area(field)
        }

      }


    }
    if(plot_overlaps==T){dev.off()}

    # merge relevant curtail_frac columns onto field-columns
    # acres to apn ratio
    area_wt_tab$acres_to_apn_ratio = NA
    area_wt_tab$acres_to_apn_ratio = curtail_frac$acres_to_apn_ratio[match(area_wt_tab$curtail_app_id,
                                                                          curtail_frac$app_id)]

    monthly_conserv = data.frame(matrix(data = NA, nrow = nrow(area_wt_tab),
                                        ncol = length(conserv_for_model)))
    colnames(monthly_conserv) = conserv_for_model
    # Apr-Oct conserv percentages from gap-filled LCS data
    area_wt_tab = cbind(area_wt_tab, monthly_conserv)
    # fill monthly conservation %s
    for(month_abb in month.abb[4:10]){
      month_cons_colname = paste0(month_abb,"_pct_cons_model")
      area_wt_tab[,month_cons_colname] = curtail_frac[match(area_wt_tab$curtail_app_id,
                                                            curtail_frac$app_id), month_cons_colname]
    }
    # Initialize columns for Apr-Oct conserv %ages after final weighting
    # final weighting =
    ## reported curtail app % conserved for a given month, times
    ## the acre-to-apn ratio, times
    ## the area of each field overlapping the apn-polygon

    monthly_conserv_2wt = data.frame(matrix(data = NA, nrow = nrow(area_wt_tab),
                                            ncol = length(conserv_for_model)))
    colnames(monthly_conserv_2wt) = paste0(conserv_for_model,"_acre_wt")
    area_wt_tab = cbind(area_wt_tab, monthly_conserv_2wt)
    area_wt_tab[, colnames(monthly_conserv_2wt)] =
      area_wt_tab[,conserv_for_model] * 1/100 * #convert to percent
      area_wt_tab$frac_area_overlap * area_wt_tab$acres_to_apn_ratio

    # Produce: month-by-field curtailment fraction tables
    # Initialize model input file (curtail_output of this process, input for model run)
    curtail_output = field_df
    swbm_ids = 1:num_unique_fields
    # match months to stress periods
    conserv_2022_month_dates = as.Date(paste0("2022-",4:10,"-01"))

    # Make the final input file. Make it run in fortran.

    date_matcher_tab = data.frame(stress_per_date = conserv_2022_month_dates,
                                  curtail_colname = colnames(monthly_conserv_2wt))
    for(i in 1:nrow(date_matcher_tab)){
      sp = date_matcher_tab$stress_per_date[i]
      cname = date_matcher_tab$curtail_colname[i]
      # assign overlap- and acre-ratio- weighted curtailment fractions to each stress period
      curtail_output[curtail_output$Stress_Period==sp, 2:ncol(curtail_output)] =
        area_wt_tab[match(swbm_ids, area_wt_tab$swbm_id),cname]
    }
    # Relace any NAs with 0s
    curtail_output[is.na(curtail_output)] = 0
    field_cols = grep(pattern = "ID", x=colnames(curtail_output))
    # PLACEHOLDER UNTIL BETTER DATA QUALITY
    # Identify fields with curtailment fractions > 1. Reset to 1.
    # sum(curtail_output[,field_cols] > 1) # 95 of them
    # sum(curtail_output[,field_cols] < 1 & curtail_output[,field_cols] > 0) #

    curtail_output[,field_cols][curtail_output[,field_cols] > 1] = 1
  }

  ## Non-basecase scenarios ------------------------------------------------
  # if(tolower(scenario_id=="natveg")){} # Placeholder :
  # Replace default with new MAR vol file, and update recognized_scenarios
  # possible steps: read in fields and potentially adjudicated zone
  # potentially read in schedule of land use updates

  ## Write curtailment output file -----------------------------------------
  if(!(tolower(scenario_id) %in% recognized_scenarios)){
    print("Warning: specified curtailment scenario not recognized in current codebase. Using basecase curtailment file")
  }
  write.table(curtail_output, file = file.path(output_dir, output_filename),
              sep = "  ", quote = FALSE, col.names = TRUE, row.names = FALSE)

}


# ------------------------------------------------------------------------------------------------#

#' Write file specifying crop coefficients for each crop type, for each daily timestep
#'
#' @param model_start_date Start date of simulation
#' @param model_end_date End date of simulation
#' @param output_dir directory to write the files in
#'
#' @return none; writes file
#' @export
#'
#' @examples
#'
#'
#'
write_daily_crop_coeff_values_file = function(model_start_date, model_end_date, output_dir){

  #TO DO: Read max kc values from the landcover_table. Use to update scenarios

  # Generate new kc records based on model period
  kc_alfalfa <- gen_daily_binary_crop_coefficients(model_start_date, model_end_date)
  kc_pasture <- gen_daily_binary_crop_coefficients(model_start_date, model_end_date)
  kc_grain   <- gen_daily_curve_crop_coefficients(model_start_date, model_end_date)
  kc_natveg <- gen_daily_binary_crop_coefficients(model_start_date, model_end_date,
                                                  kc_growing = 0.6,
                                                  growing_season_start_day = 1, growing_season_start_month = 1,
                                                  growing_season_end_day = 31, growing_season_end_month = 12)
  kc_water <- gen_daily_binary_crop_coefficients(model_start_date, model_end_date,
                                                 kc_growing = 0,
                                                 growing_season_start_day = 1, growing_season_start_month = 1,
                                                 growing_season_end_day = 31, growing_season_end_month = 12)
  kc_urban <- gen_daily_binary_crop_coefficients(model_start_date, model_end_date,
                                                 kc_growing = 0,
                                                 growing_season_start_day = 1, growing_season_start_month = 1,
                                                 growing_season_end_day = 31, growing_season_end_month = 12)
  # Combine daily kc records into one table
  kc_values = data.frame(Date = format(kc_alfalfa$Date, "%Y-%m-%d"),
                         Alfalfa_Kc = kc_alfalfa$kc, Grain_Kc = kc_grain$kc,
                         Pasture_Kc = kc_pasture$kc, Native_Veg_Kc = kc_natveg$kc,
                         Barren_Urban_Kc = kc_urban$kc, Water_Kc = kc_water$kc)
  # Write combined kc values file
  write.table(kc_values, file = file.path(output_dir, "kc_values.txt"),
              sep = "  ", quote = FALSE, col.names = TRUE, row.names = FALSE)
  }


