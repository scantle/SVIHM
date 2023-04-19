# 02_BaseUpdate.R
#
#
library(RSVP)

# ------------------------------------------------------------------------------------------------#
# Settings ----------------------------------------------------------------------------------------
# Dates
start_year <- 1991 # WY 1991; do not change
end_year   <- as.numeric(format(Sys.Date(), "%Y"))  # Assumes current year

# Directory (Created in SVIHM_Input_Files/Updates) - Grabs latest version
update_dir <- latest_dir(data_dir['update_dir','loc'])

# Scenario selection
current_scenario = "basecase" # default is "basecase". Affects a variety of input files.

# ------------------------------------------------------------------------------------------------#

# Temporal discretization -------------------------------------------------------------------------

model_start_date <- get_model_start(start_year)
model_end_date <- as.Date(basename(update_dir)) #as.Date('2022-03-31')

num_stress_periods <- calc_num_stress_periods(model_start_date, model_end_date)
num_days <- days_in_month_diff(model_start_date, model_end_date)  # current setup: days = time steps

# ------------------------------------------------------------------------------------------------#


# Create Time-Varying SWBM Inputs -----------------------------------------

# Requires Flow file to have been downloaded to update_dir
fjd <- read.csv(file.path(update_dir, list.files(update_dir, pattern = 'FJ (USGS 11519500)*')),
                          stringsAsFactors = F)
fjd$Date <- as.Date(fjd$Date)


# avail_monthly <- build_cdfw_instream_flow_cal(model_start_date = model_start_date,
#                                               model_end_date = model_end_date,
#                                               fort_jones_flows = fjd)


sfr_subws_flow_partitioning <- gen_monthly_sfr_flow_partition(model_start_date, model_end_date, update_dir)
subws_inflow_filename = file.path(update_dir,"streamflow_input.txt")
subws_irr_inflows <- process_monthly_sfr_inflows(model_start_date, model_end_date,
                                             stream_inflow_filename = subws_inflow_filename,
                                             avail_for_irr = T,
                                             scenario_id = current_scenario) # Possibly divide flow into avail and unavail for irr based on flow regime
subws_nonirr_inflows <- process_monthly_sfr_inflows(model_start_date, model_end_date,
                                                    stream_inflow_filename = subws_inflow_filename,
                                                    avail_for_irr = F,
                                                    scenario_id = current_scenario) # Possibly divide flow into avail and unavail for irr based on flow regime

# Write SWBM Inputs -------------------------------------------------------

# Main input file
write_SWBM_gen_inputs_file(num_stress_periods = num_stress_periods, output_dir = update_dir)

# Stress Period Days file
# Include stress period # and column names in input file
num_days_tab = data.frame("stress_period" = 1:num_stress_periods, ndays = num_days)
write.table(num_days_tab, file = file.path(update_dir, "stress_period_days.txt"),
            sep = " ", quote = FALSE, col.names = TRUE, row.names = FALSE)

# Drains
write_SWBM_drain_files(num_stress_periods = num_stress_periods, output_dir = update_dir)

# Instream available flow ratio
# write_SWBM_instream_available_file(avail_monthly, output_dir = update_dir)

# Crop coefficients
# write_SWBM_crop_coefficient_file(kc_alfalfa, update_dir, 'kc_alfalfa.txt')
# write_SWBM_crop_coefficient_file(kc_pasture, update_dir, 'kc_pasture.txt')
# write_SWBM_crop_coefficient_file(kc_grain,   update_dir, 'kc_grain.txt')

write_daily_crop_coeff_values_file(model_start_date, model_end_date, update_dir)

# Surface flow / SFR files
write_SWBM_SFR_inflow_files(sfr_subws_flow_partitioning, update_dir, "SFR_subws_flow_partitioning.txt")
write_SWBM_SFR_inflow_files(subws_irr_inflows, update_dir, "subwatershed_irrigation_inflows.txt")
write_SWBM_SFR_inflow_files(subws_nonirr_inflows, update_dir, "subwatershed_nonirrigation_inflows.txt")
write_SWBM_SFR_diversions_file(output_dir = update_dir)

# Specified pumping data (if available)
write_ag_pumping_file(start_date = model_start_date, n_stress = num_stress_periods,
                      output_dir = update_dir, ag_pumping_data = NA)
write_muni_pumping_file(start_date = model_start_date, n_stress = num_stress_periods,
                      output_dir = update_dir, muni_pumping_data = NA)

# Land use by field by month
write_SWBM_landcover_file(scenario_id = current_scenario, output_dir = update_dir,
                          start_date = model_start_date, end_date = model_end_date)
# MAR applications by field by month
write_SWBM_MAR_depth_file(scenario_id = current_scenario, output_dir = update_dir,
                        start_date = model_start_date, end_date = model_end_date)
# Irrigation curtailment fractions (as fraction of calculated demand) by field by month
write_SWBM_curtailment_file(scenario_id = current_scenario,
                            output_dir = update_dir,
                            start_date = model_start_date,
                            end_date = model_end_date)


# ------------------------------------------------------------------------------------------------#

# Write MODFLOW Inputs ----------------------------------------------------

# Discretization (DIS)
update_DIS_stress_periods(num_days, num_stress_periods, output_dir = update_dir)

# Drain (DRN)
update_DRN_stress_periods(num_stress_periods, output_dir = update_dir)

# Head Observations (HOB)
write_SVIHM_head_obs_file(model_start_date, model_end_date, output_dir = update_dir)

# Output Control (OC)
update_OC_stress_periods(num_days, num_stress_periods, output_dir = update_dir)

# ------------------------------------------------------------------------------------------------#


# Batch File --------------------------------------------------------------

# If necessary, create new batchfile for
batch_file_name = paste0("Prepare_",current_scenario,"_Run.bat")
if(!file.exists(file.path(data_dir["svihm_dir","loc"], batch_file_name))){
  write_scenario_prep_batchfile(scenario_name = current_scenario)
  }

# Create new batchfile for assembling the updated model
write_update_prep_batchfile(update_dir)


