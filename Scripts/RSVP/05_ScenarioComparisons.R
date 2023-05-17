library(RSVP)
library(viridis)
library(hydroGOF)
library(RMODFLOW)
library(ggplot2)
library(reshape2)
library(sf)
library(colorspace)

## Graphs promised in 5/3 SWRC presentation
# Flow differences: time series at FJ gauge (zoomed in for specific water year)
# Map of flow differences in Oct 2021 and Oct 2022, between BAU (0 curtail) and historical basecase
# Drawdown maps between historical and BAU scenarios, Fall 2021 and Fall 2022
# Overall groundwater storage time series: 2 lines on same graph (historical and BAU)


#/////////////////-
# V I S U A L S

#-------------------------------------------------------------------------------------------------#
# Settings ----------------------------------------------------------------

origin_date <- as.Date('1990-09-30')

create_sp_charts = FALSE  # Many SPs, very slow

# Directories
# run_dir <- file.path('../../Run/')
# swbm_dir = file.path(run_dir, 'SWBM')
# mf_dir <- file.path(run_dir, 'MODFLOW')
# Scenario 1
# s1_dir <- file.path('../../Scenarios/no_curtail_2023.05.08') # RUN THIS
s1 = "curtail_30_pct_2022"
s1_dir <- file.path('../../Scenarios', s1)
swbm1_dir = file.path(s1_dir, 'SWBM')
mf1_dir <- file.path(s1_dir, 'MODFLOW')
s2 = "curtail_50_pct_2022"
s2_dir <- file.path('../../Scenarios',s2)
swbm2_dir = file.path(s2_dir, 'SWBM')
mf2_dir <- file.path(s2_dir, 'MODFLOW')

# TODO automate finding latest version
update_dir <- latest_dir(data_dir['update_dir','loc'])  #file.path('../../SVIHM_Input_Files/Updates/2022-04-13/')
plot_data_dir = file.path('../../SVIHM_Input_Files/reference_data_for_plots/')


plots1_dir <- file.path(s1_dir, 'Plots')
plots2_dir <- file.path(s2_dir, 'Plots')
out_dir = file.path('../../Scenarios', "_Comparison_Plots")


if (!dir.exists(plots1_dir)) {dir.create(plots1_dir, recursive = T)}
if (!dir.exists(plots2_dir)) {dir.create(plots2_dir, recursive = T)}

# info from general_inputs.txt
gen_inputs1 = strsplit(readLines(file.path(swbm1_dir, "general_inputs.txt")), "  ")
gen_inputs2 = strsplit(readLines(file.path(swbm1_dir, "general_inputs.txt")), "  ")
#assumes same number of stress periods in 1 and 2
wy_start = as.numeric(gen_inputs1[[1]][2])
start_date = as.Date(paste0(wy_start-1,"-10-01"))
n_stress = as.numeric(gen_inputs1[[2]][3])


#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# Read in Data ------------------------------------------------------------

#-- Observed

#-- FJ
fj_obs <- read.csv(file.path(update_dir, list.files(update_dir, pattern = 'FJ (USGS 11519500)*')),
                   stringsAsFactors = F)
fj_obs$Date <- as.Date(fj_obs$Date)

#-- Serpa Lane (Not in GITHUB - stored locally #TODO permissions)
sl_obs <- read.table('c:/Users/lelan/Box/Research/Scott Valley/Data/Streamflow/Scott River Above Serpa Lane.txt',
                     header=T)
sl_obs$Date <- as.Date(sl_obs$Date, format = '%m/%d/%Y')
sl_obs$Flow <- sl_obs$Streamflow_cfs

#-- Below Youngs Dam (Not in GITHUB - stored locally #TODO permissions)
by_obs <- read.table('c:/Users/lelan/Box/Research/Scott Valley/Data/Streamflow/Scott River Below Youngs Dam.txt',
                     header=T)
by_obs$Date <- as.Date(by_obs$Date, format = '%m/%d/%Y')
by_obs$Flow <- by_obs$Streamflow_cfs

#-- Group surface water
streams <- list(fj_obs, sl_obs, by_obs)
stream_names <- c('Fort Jones', 'Serpa Lane', 'Below Youngs Dam')
stream_short <- c('FJ', 'AS', 'BY')

#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
#-- Modeled

#-- HOB data
hob_locs <- read.csv(file.path(data_dir['ref_data_dir','loc'], 'hob_wells.csv'),
                     row.names=1, stringsAsFactors = F)
hob1 <- import_HOB(hob_input = file.path(mf1_dir, 'SVIHM.hob'),
                  hob_output = file.path(mf1_dir, 'HobData_SVIHM.dat'),
                  origin_date = origin_date)
hob1 <- hob1[order(hob1$row, hob1$column),]

hob2 <- import_HOB(hob_input = file.path(mf2_dir, 'SVIHM.hob'),
                   hob_output = file.path(mf2_dir, 'HobData_SVIHM.dat'),
                   origin_date = origin_date)
hob2 <- hob1[order(hob2$row, hob2$column),]


#-- SFR Data (Turn into function?)
sfr_locs <- read.csv(file.path(data_dir['ref_data_dir','loc'], 'sfr_gages.csv'),
                     row.names=1, stringsAsFactors = F)
streams_sim1 <- list(import_sfr_gauge(file.path(mf1_dir, 'Streamflow_FJ_SVIHM.dat'), origin_date = origin_date),
                    import_sfr_gauge(file.path(mf1_dir, 'Streamflow_AS_SVIHM.dat'), origin_date = origin_date),
                    import_sfr_gauge(file.path(mf1_dir, 'Streamflow_BY_SVIHM.dat'), origin_date = origin_date))
streams_sim2 <- list(import_sfr_gauge(file.path(mf2_dir, 'Streamflow_FJ_SVIHM.dat'), origin_date = origin_date),
                     import_sfr_gauge(file.path(mf2_dir, 'Streamflow_AS_SVIHM.dat'), origin_date = origin_date),
                     import_sfr_gauge(file.path(mf2_dir, 'Streamflow_BY_SVIHM.dat'), origin_date = origin_date))


#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# Streamflow Comparison Maps ------------------------------------------------------


# Read and process streamflow data - output from modflow
save_sfr_array = function(scen_dir){
  plots_dir <- file.path(scen_dir, 'Plots')
  mf_dir_i <- file.path(scen_dir, 'MODFLOW')


  sfr_glob_text = readLines(file.path(mf_dir_i, "Streamflow_Global.dat"))
  start_rows = grep("STREAM LISTING", sfr_glob_text) + 5 #one start for each stress period
  n_reach = start_rows[2]-start_rows[1]-8  # 8 extra header rows at each timestep

  colname_list = c("LAYER", "ROW","COL", "STREAM_SEG_NO", "RCH_NO", "FLOW_INTO_STRM_RCH",
                   "FLOW_TO_AQUIFER", "FLOW_OUT_OF_STRM_RCH","OVRLND_RUNOFF","DIRECT PRECIP",
                   "STREAM_ET","STREAM_HEAD", "STREAM_DEPTH", "STREAM_WIDTH",
                   "STREAMBED_CONDCTNC","STREAMBED_GRADIENT")

  # Initialize array
  reach_array = array(data=NA, dim = c(length(start_rows), n_reach, 16))
  # Process SFR values into an array of row, column, and stress period
  for(i in 1:length(start_rows)){
    start_row = start_rows[i];
    sfr_stress = sfr_glob_text[start_row:(start_row+n_reach-1)]
    for(j in 1:n_reach){
      sfr_reach = unlist(strsplit(trimws(sfr_stress[j]), " ")) #split on space character
      sfr_reach = sfr_reach[nchar(sfr_reach)>0] #this produces a lot of blank strings; get rid of those
      reach_array[i,j,] = sfr_reach
    }
    # Save giant reach file as .Rdata
    # saveRDS(object=reach_array,file=file.path(plot_data_dir,"sfr_reach_array.Rdata"))
    saveRDS(object=reach_array,file=file.path(plots_dir,"sfr_reach_array.rds"))
  }
}

# Read in reach arrays
# Reading SFR data takes ~5 mins. Save to an .RDS file for convenience
if(!file.exists(file.path(plots1_dir, "sfr_reach_array.RDS"))){
  save_sfr_array(scen_dir = s1_dir)
}
if(!file.exists(file.path(plots2_dir, "sfr_reach_array.RDS"))){
  save_sfr_array(scen_dir = s2_dir)
}

reach_array1 = readRDS(file.path(s1_dir,"Plots","sfr_reach_array.RDS"))
reach_array2 = readRDS(file.path(s2_dir,"Plots","sfr_reach_array.RDS"))

# Set up for SFR stream network maps

# Check flow max
max(as.numeric(as.character(reach_array1[,,8]))) # max flow out
max(as.numeric(as.character(reach_array2[,,8]))) # max flow out
# Breaks for flow
flow_breaks_manual = c(0, 2.5, 20, 50, 100, 300, 700, 6400)*1000
#Set color palette
n_classes = 7
pal = rev(sequential_hcl(n_classes, palette = "ag_GrnYl"))
# Read in GIS data
seg = st_read(dsn = plot_data_dir, layer = "SFR_segments_sugar_pts")
seg = st_transform(seg, crs = st_crs(3310))
# make identifier for each seg point
seg$row_col = paste(seg$row, seg$column, sep="_")
seg$flow_out = NA; seg$depth = NA; seg$color =NA

#read in Bulletin 118 groundwater basin boundary shapefile
basin = st_read(dsn = plot_data_dir, layer ="SGMA_B118_SV")
basin = st_transform(basin, crs = st_crs(3310))
#generate background color polygon
bg_poly = st_buffer(x = basin, dist=1e5)


#### Plot streamflow

#make a pdf appendix of each timestep of dry or wet

#make table of months and years for each stress period
stress_period_table = data.frame(stress_period=1:n_stress); sp_tab = stress_period_table
sp_tab$date = seq.Date(from = start_date, by = "month", length.out=n_stress)
sp_tab$month = month(sp_tab$date)
sp_tab$water_year = year(sp_tab$date); sp_tab$water_year[sp_tab$month>9] = year(sp_tab$date[sp_tab$month>9])+1

#to make a pdf appendix with each stress period plotted:
pdf_name = paste0("sfr_diff", s1, "minus",s2, ".pdf")
pdf(file.path(plots1_dir, pdf_name), width=8.5, height=11)
for(i in 1:length(start_rows)){

  #to make a png figure with manually selected stress periods plotted
  # png(file.path(out_dir, "wet_dry_stream_4yrs.png"),
  #     width=7.5, height=16, units = "in", res=300)
  # par(mfrow=c(4,1), mar = c(1,1,1,1))
  # for(i in c(287, 323, 239, 299)){ #Aug of 2014 (wet), 2017 (dry), 2010, and 2015 (avg, spread and conc)

  stress_period_array = data.frame(reach_array[i,,])
  spa = stress_period_array
  title_text = paste(month.abb[sp_tab$month[i]],"of water year",sp_tab$water_year[i])
  #process matrix a bit
  colnames(spa)=colname_list
  spa$row_col = paste(spa$ROW, spa$COL, sep="_")
  spa$FLOW_OUT_OF_STRM_RCH = as.numeric(as.character(spa$FLOW_OUT_OF_STRM_RCH))
  spa$STREAM_DEPTH = as.numeric(as.character(spa$STREAM_DEPTH))

  # par(mfrow = c(1,2)) #if plotting both flow and depth

  # Flowrate
  seg$flow_out = spa$FLOW_OUT_OF_STRM_RCH[match(seg$row_col, spa$row_col)]
  # seg$color = "dodgerblue"
  #all flow segments with flow out of < 1 cfs are considered dry
  # seg$color[seg$flow_out/2446.6 < 1] = "salmon" #convert m^3/day to cfs for threshold comparison
  seg$color[is.na(seg$row_col)]="black"
  # plot(basin, border="darkgray", main=title_text,
  #      sub=paste("stress period",sp_tab$stress_period[i]))
  # plot(seg, col=seg$color, pch=19, cex=0.2, add=T)
  #plot basin polygon as background
  plot(basin$geometry,main=title_text, sub=paste("stress period",sp_tab$stress_period[i]))
  plot(bg_poly$geometry, col="burlywood1",add=T)
  plot(basin$geometry, border="black", add=T
       #col = "gray20",
       # main=title_text, sub=paste("stress period",sp_tab$stress_period[i]))
  )
  #plot river reach centroids, colored according to depth
  plot(seg$geometry,pch=19, cex=1, add=T,
       col=pal[cut(na.omit(seg$flow_out), include.lowest=T,
                   breaks = flow_breaks_manual)])
  # generate legend labels and add legend
  legend_labels = paste(flow_breaks_manual[1:n_classes]/1000,
                        flow_breaks_manual[2:(n_classes+1)]/1000, sep="-")
  legend(x="bottomleft", fill = pal, title="Flow (1000 m3/day)",
         legend = legend_labels)#, cex=2.5)

  #legend in CFS
  # legend_labels = paste(round(flow_breaks_manual[1:n_classes]/2446.6),
  #                       round(flow_breaks_manual[2:(n_classes+1)]/2446.6), sep="-")
  # legend(x="bottomleft", fill = pal, title="Flow (cfs)",
  #        legend = legend_labels)


  # # Flow depth
  # seg$depth = spa$STREAM_DEPTH[match(seg$row_col, spa$row_col)]
  # # seg$color = "dodgerblue"
  # #all flow segments with flow out of < 1 cfs are considered dry
  # # seg$color[seg$depth*39.3701 < 3] = "salmon" #convert m to inches for threshold comparison
  # seg$color[is.na(seg$row_col)]="black"
  # #plot basin polygon as background
  # plot(basin, border="darkgray", main=title_text, col = "darkgray",
  #      sub=paste("stress period",sp_tab$stress_period[i]))
  # #plot river reach centroids, colored according to depth
  # plot(seg, col=pal[cut(na.omit(seg$depth), breaks = depth_breaks_manual_7, include.lowest=T)],
  #      pch=19, cex=0.2, add=T)
  # #generate legend labels and add legend
  # legend_labels = paste(depth_breaks_manual_7[1:n_classes], depth_breaks_manual_7[2:(n_classes+1)], sep="-")
  # legend(x="bottomleft", fill = pal, title="Flow depth (m)",
  #        legend = legend_labels)

}
dev.off()



# -------------------------------------------------------------------------#
# -------------------------------------------------------------------------#
# FJ Flow Comparison ------------------------------------------------------


fjsim1 =
fjsim2

# -------------------------------------------------------------------------#
# -------------------------------------------------------------------------#
# Drawdown maps ------------------------------------------------------



# -------------------------------------------------------------------------#
# -------------------------------------------------------------------------#
# Groundwater change in storage ------------------------------------------------------



# -------------------------------------------------------------------------#
# -------------------------------------------------------------------------#
# Water Budget Comparison ------------------------------------------------------


SWBM_Terms = c('Precipitation', 'SW Irrigation', 'GW Irrigation', 'ET', 'Recharge', 'Runoff', 'Storage','Error')
SWBM_colors = c('lightblue1',  'darkcyan', 'midnightblue', 'goldenrod','green4', 'mistyrose','black','gray')

convert_monthly_to_annual = function(SWBM_Monthly_m3){
  SWBM_Annual_m3 = aggregate(.~WY,SWBM_Monthly_m3[,!names(SWBM_Monthly_m3)%in% c('Month',"Stress_Period")], FUN = sum)
  return(SWBM_Annual_m3)
}


read_process_swbm_monthly_budget = function(swbm_dir){
  SWBM_Monthly_m3 = read.table(file.path(swbm_dir,'monthly_water_budget.dat'), header = T)
  names(SWBM_Monthly_m3_s1) = c('Month',SWBM_Terms)
  n_stress = nrow(SWBM_Monthly_m3)
  SWBM_Monthly_m3$Month = seq.Date(from = origin_date+1, by = "month", length.out = n_stress)
  SWBM_Monthly_m3$WY = year(SWBM_Monthly_m3$Month)
  SWBM_Monthly_m3$WY[month(SWBM_Monthly_m3$Month)>9] = year(SWBM_Monthly_m3$Month[month(SWBM_Monthly_m3$Month)>9]) +1
  # SWBM_Monthly_m3$Month = format(seq(origin_date, by = "month", length.out = n_stress),'%b-%Y')

  return(SWBM_Monthly_m3)
}

SWBM_Monthly_m3_s1 = read_process_swbm_monthly_budget(swbm_dir = swbm1_dir)
SWBM_Monthly_m3_s2 = read_process_swbm_monthly_budget(swbm_dir = swbm2_dir)

SWBM_Annual_m3_s1 = convert_monthly_to_annual(SWBM_Monthly_m3 = SWBM_Monthly_m3_s1)
SWBM_Annual_m3_s2 = convert_monthly_to_annual (SWBM_Monthly_m3 = SWBM_Monthly_m3_s2)

swbm_annual_m3_diff = SWBM_Annual_m3_s1
swbm_annual_m3_diff[,2:9] = SWBM_Annual_m3_s1[,2:9] - SWBM_Annual_m3_s2[,2:9]
swbm_annual_m3_rel_diff = swbm_annual_m3_diff
swbm_annual_m3_rel_diff[,2:9] = swbm_annual_m3_diff[,2:9] / SWBM_Annual_m3_s1[,2:9]

swbm_monthly_m3_diff = SWBM_Monthly_m3_s1
swbm_monthly_m3_diff[,2:9] = SWBM_Monthly_m3_s1[,2:9] - SWBM_Monthly_m3_s2[,2:9]
swbm_monthly_m3_rel_diff = swbm_monthly_m3_diff
swbm_monthly_m3_rel_diff[,2:9] = swbm_monthly_m3_diff[,2:9] / SWBM_Monthly_m3_s1[,2:9]

sm_dates = SWBM_Monthly_m3_s1[SWBM_Monthly_m3_s1$Month >= as.Date("2022-03-01"),]
smd_dates = swbm_monthly_m3_diff[swbm_monthly_m3_diff$Month >= as.Date("2022-03-01"),]

sw_tot_rel_diff = sum(smd_dates$SW_Irr, na.rm=T) / sum(sm_dates$SW_Irr, na.rm=T)
gw_tot_rel_diff = sum(smd_dates$GW_Irr, na.rm=T) / sum(sm_dates$GW_Irr, na.rm=T)

smrd_dates = swbm_monthly_m3_rel_diff[swbm_monthly_m3_rel_diff$Month >= as.Date("2022-03-01"),]
mean(smrd_dates$SW_Irr*100, na.rm=T)
mean(smrd_dates$GW_Irr*100, na.rm=T)

#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# Volumetric Budget - SWBM - Monthly bar graph ------------------------------------------------------

# make data long-format
keep_cols = !(colnames(SWBM_Monthly_m3_s1) %in% c("WY", "Stress_Period"))
SWBM_Monthly_m3_s1_melt =  melt(SWBM_Monthly_m3_s1[,keep_cols],
                                id.vars = 'Month')
SWBM_Monthly_m3_s2_melt =  melt(SWBM_Monthly_m3_s2[,keep_cols], id.vars = 'Month')

plot_one_wy_monthly = function(SWBM_Monthly_m3_melt, plot_wy = 2022, save_as_pdf = F){
  # Set water year
  if(save_as_pdf==T){
    pdf(file = file.path(out_dir, "Monthly Budget Plots.pdf"), width = 8.5, height = 11/2)
  }
  for(wy in plot_wy){#1991:2021){ # wy = 1995
    this_wy = wy
    datelims = as.Date(paste0(c(this_wy-1, this_wy), "-10-01"))
    # Make monthly line plot
    # SWBM_Monthly_Mm3_Plot =
    print(ggplot(SWBM_Monthly_m3_melt, aes(x = Month, y = value/1E6, color=variable)) +
            geom_line(size=2) +
            scale_color_manual(values = SWBM_colors) +
            scale_x_continuous(limits = datelims) +
            xlab('') +
            ylab(bquote('Volume ('*Mm^3*')')) +
            ggtitle(paste('Soil Zone Monthly Budget, WY', this_wy)) +
            theme(legend.title=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_rect(color = 'black', fill = NA),
                  plot.background = element_rect(color = NA, fill = NA),
                  axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.7),
                  axis.text = element_text(size = 8),
                  plot.title = element_text(hjust = 0.5),
                  legend.position = c(0.25, 0.95),
                  legend.key = element_rect(fill = NA, color = NA),
                  legend.background = element_rect(fill = NA, color = NA),
                  legend.direction = 'horizontal',
                  legend.text = element_text(size = 6, margin = margin(r=1,l=1, unit = 'pt')),
                  legend.key.height = unit(10,'pt'))
    )
  }
  if(save_as_pdf==TRUE){dev.off()}
}

plot_one_wy_monthly(SWBM_Monthly_m3_melt = SWBM_Monthly_m3_s1_melt, plot_wy = 2022)
