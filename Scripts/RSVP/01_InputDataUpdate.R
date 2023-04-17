# 01_InputDataDownload.R
# Downloads and processes new hydrologic data:
# 1. Precip
# 2. ET
# 3. Streamflow
# 4. Tributary Inflow (calculated from FJ Streamflow)
#
library(RSVP)
library(cimir)
library(readr)

# ------------------------------------------------------------------------------------------------#
# Settings ----------------------------------------------------------------------------------------

# Dates
start_year <- 1991 # WY 1991; do not change
end_year   <- as.numeric(format(Sys.Date(), "%Y"))  # Assumes current year

# Rainfall data requires NOAA CDO token
# (free online: https://www.ncdc.noaa.gov/cdo-web/webservices/v2)
noaa_token <- read_lines(file='_noaa_cdo_token.txt', n_max = 1)
options(noaakey = noaa_token)

# ET data requires CIMIS Token
# (free online: https://cimis.water.ca.gov/)
cimis_key <- read_lines(file='_CIMIS_API_key.txt')
cimir::set_key(cimis_key)
# ------------------------------------------------------------------------------------------------#

# ------------------------------------------------------------------------------------------------#

# Temporal discretization -------------------------------------------------------------------------

model_start_date <- get_model_start(start_year)
model_end_date <- as.Date(floor_date(Sys.Date(), 'month')-1)

num_stress_periods <- calc_num_stress_periods(model_start_date, model_end_date)
num_days <- days_in_month_diff(model_start_date, model_end_date)  # current setup: days = time steps

# Directory (Created in SVIHM_Input_Files/Updates)
update_dir <- create_update_dir(end_date = model_end_date)

# Weather Data ------------------------------------------------------------------------------------

# Precip
prcp <- get_daily_precip_table(model_start_date, model_end_date)
write_swbm_precip_input_file(p_record = prcp, output_dir = update_dir, filename = 'precip.txt')

# ET
et <- build_daily_et_df(model_start_date, model_end_date)
# In case of CIMIS rejecting your query (try several times to be sure), download data from
# CIMIS station 225 (daily data, csv file, metric units, 2015-04-19 through present),
# save in update_dir, and use this workaround

if(!exists("et")){
  et <- build_daily_et_df(start_date = model_start_date, end_date = model_end_date,
                          api_download = F, local_file = T, update_dir = update_dir)

}
write_swbm_et_input_file(et_record = et, output_dir = update_dir, filename = 'ref_et.txt')


# River Flows -------------------------------------------------------------------------------------

# Update observed FJ flows
fjd_model <- download_fort_jones_flow(model_start_date,
                                    model_end_date,
                                    output_dir = update_dir,
                                    save_csv = TRUE)

tribs <- get_tributary_flows(end_date = model_end_date, fj_update = fjd_model)

# Combine East Fork and South Fork tributary flows,
# since they are represented at a single inflow point at the confluence
pred_or_obs = rep("Observed", nrow(tribs$East_Fork))
pred_or_obs[tribs$East_Fork$source == "Predicted" | tribs$South_Fork$source == "Predicted"] = "Predicted"

e_and_s_forks = data.frame(Date = tribs$East_Fork$Date,
                          normLogAF_trib = NA,
                          normlogAF_FJ = NA,
                          stream_name = "East_and_South_Forks",
                          pred = NA,
                          source = pred_or_obs,
                          pred_AF = tribs$East_Fork$pred_AF+tribs$South_Fork$pred_AF,
                          obs_AF = tribs$East_Fork$obs_AF+tribs$South_Fork$obs_AF) # NA for observed if either is NA

# Remove East and South Fork data tables and add the combined data table
# As the first stream in the list of tribs
tribs$East_Fork=NULL; tribs$South_Fork= NULL
tribs_2 = list(East_and_South_Forks = e_and_s_forks)
tribs_2 = append(tribs_2, tribs)

write_tributary_input_file(gauges = tribs_2, output_dir = update_dir,
                           start_date=model_start_date, end_date=model_end_date)

