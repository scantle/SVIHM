library(usethis)
library(sf)

#-------------------------------------------------------------------------------------------------#

#-- Create data_dir internal dataset (a directory of directories)

# Assumed path from RSVP package default dir (located in Scripts/RSVP) to SVIHM dir
svihm_dir <- file.path('../../')

data_dir <- read.table(text=
                         'key              rel_loc
                    svihm_dir        .
                    input_files_dir  SVIHM_Input_Files
                    time_indep_dir   SVIHM_Input_Files/time_independent_input_files
                    ref_data_dir     SVIHM_Input_Files/reference_data
                    ref_plot_dir     SVIHM_Input_Files/reference_data_for_plots
                    scenario_dev_dir SVIHM_Input_Files/Scenario_Development
                    update_dir       SVIHM_Input_Files/Updates
                    sf_reg_dir       Streamflow_Regression_Model/
                   ', header=T, row.names=1)

# Add relative path to create "exact" location column
data_dir$loc <- file.path(svihm_dir, data_dir$rel_loc)

# Check (it's important the directories exist)
if (!all(dir.exists(data_dir$loc))) {
  stop(paste('One or more directories do not exist:', data_dir[!dir.exists(data_dir$loc),'loc']))
}

#-------------------------------------------------------------------------------------------------#

#-- Create stream_metadata.R, an internal dataframe full of information about stream/creek data files

streams = c('FJ','East_Fork','South_Fork','Sugar','Etna','French','Patterson','Kidder','Moffett','Mill','Shackleford','Johnson','Crystal')
prms_seg = c(139, 22, 31, 37, 67, 53, 86, 93, 109, 132, 135, 80, 81)
gauge_daily_mean_files <- c('USGS_11519500_WY_1942_2018.txt',  # Can (should) be replaced with latest version
                            'East_Fork_Scott_River_R_input.txt',
                            'South_Fork_Scott_River_R_input.txt',
                            'Sugar_Creek_R_input.txt',
                            'Etna_Creek_R_input.txt',
                            'French_Creek_R_input.txt',
                            'Patterson_Creek_R_input.txt',
                            'Kidder_Creek_R_input.txt',
                            'Moffett_Creek_R_input.txt',
                            'Mill_Creek_R_input.txt',
                            'Shackleford_Creek_R_input.txt',
                            NA,
                            NA)

stream_metadata <- data.frame(name=streams,
                              daily_mean_file=gauge_daily_mean_files,
                              PRMS_seg=prms_seg)

#-------------------------------------------------------------------------------------------------#

#-- Soil Water Balance Model Tables

# Irrigation
swbm_irrtype <- data.frame('Name'=c('Flood', 'Wheel Line', 'Center Pivot', 'No Source', 'Unknown'),
                           'Code'=c(      1,            2,              3,         555,       999),
                           row.names=1)
# Land Use
swbm_lutype <- data.frame('Name'=c('Alfalfa', 'Pasture', 'ET_NoIrr', 'NoET_NoIrr', 'Water'),
                          'Code'=c(       25,         2,          3,            4,       6),
                          row.names=1)

#-- Add to internal data
usethis::use_data(data_dir,
                  stream_metadata,
                  swbm_irrtype,
                  swbm_lutype,
                  internal = T, overwrite = T)
