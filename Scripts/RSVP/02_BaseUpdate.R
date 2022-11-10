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

kc_alfalfa <- gen_daily_binary_crop_coefficients(model_start_date, model_end_date)
kc_pasture <- gen_daily_binary_crop_coefficients(model_start_date, model_end_date)
kc_grain   <- gen_daily_curve_crop_coefficients(model_start_date, model_end_date)

sfr_subws_flow_partitioning <- gen_monthly_sfr_flow_partition(model_start_date, model_end_date)
subws_inflow_filename = file.path(update_dir,"streamflow_input.txt")
subws_irr_inflows <- process_monthly_sfr_inflows(model_start_date, model_end_date,
                                             stream_inflow_filename = subws_inflow_filename,
                                             avail_for_irr = T)
subws_nonirr_inflows <- process_monthly_sfr_inflows(model_start_date, model_end_date,
                                                    stream_inflow_filename = subws_inflow_filename,
                                                    avail_for_irr = F)


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
write_SWBM_crop_coefficient_file(kc_alfalfa, update_dir, 'kc_alfalfa.txt')
write_SWBM_crop_coefficient_file(kc_pasture, update_dir, 'kc_pasture.txt')
write_SWBM_crop_coefficient_file(kc_grain,   update_dir, 'kc_grain.txt')

# Surface flow / SFR files
write_SWBM_SFR_inflow_files(sfr_subws_flow_partitioning, update_dir, "SFR_subws_flow_partitioning.txt")
write_SWBM_SFR_inflow_files(subws_irr_inflows, update_dir, "subwatershed_irrigation_inflows.txt")
write_SWBM_SFR_inflow_files(subws_nonirr_inflows, update_dir, "subwatershed_nonirrigation_inflows.txt")
write_SWBM_SFR_diversions_file(output_dir = update_dir)

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

# Create new batchfile for assembling the updated model
write_update_prep_batchfile(update_dir)
