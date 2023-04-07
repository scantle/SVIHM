library(RSVP)
library(viridis)
library(hydroGOF)
library(RMODFLOW)
library(ggplot2)
library(reshape2)

#/////////////////-
# V I S U A L S

#-------------------------------------------------------------------------------------------------#
# Settings ----------------------------------------------------------------

origin_date <- as.Date('1990-09-30')

create_sp_charts = FALSE  # Many SPs, very slow

# Directories
run_dir <- file.path('../../Run/') # file.path("C:/Users/Claire/Documents/GitHub/SVIHM_SWBM_dev/SWBM_Test/SWBM")
swbm_dir = file.path(run_dir, 'SWBM')
mf_dir <- file.path(run_dir, 'MODFLOW')
# TODO automate finding latest version
update_dir <- latest_dir(data_dir['update_dir','loc'])  #file.path('../../SVIHM_Input_Files/Updates/2022-04-13/')

out_dir <- file.path(run_dir, 'Plots')

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = T)
}
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
hob <- import_HOB(hob_input = file.path(mf_dir, 'SVIHM.hob'),
                  hob_output = file.path(mf_dir, 'HobData_SVIHM.dat'),
                  origin_date = origin_date)
hob <- hob[order(hob$row, hob$column),]

#-- SFR Data (Turn into function?)
sfr_locs <- read.csv(file.path(data_dir['ref_data_dir','loc'], 'sfr_gages.csv'),
                     row.names=1, stringsAsFactors = F)
streams_sim <- list(import_sfr_gauge(file.path(mf_dir, 'Streamflow_FJ_SVIHM.dat'), origin_date = origin_date),
                    import_sfr_gauge(file.path(mf_dir, 'Streamflow_AS_SVIHM.dat'), origin_date = origin_date),
                    import_sfr_gauge(file.path(mf_dir, 'Streamflow_BY_SVIHM.dat'), origin_date = origin_date))

#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# Statistics --------------------------------------------------------------

#-------------------------------------------------------------------------------------------------#
#-- Processing

stream_combined <- ts_obs_sim_combine(streams, streams_sim, stream_names, val_cols = c('Flow','Flow_cfs'))
#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
#-- Evaluation
cal_split <- as.Date('2012-10-01')
hob_compare <- calc_split_sample_stats(hob$date, hob$sim, hob$hobs,
                                       cal_split,
                                       FUNs = list(NSE, rmse, KGE),
                                       FUN_names = c('NSE', 'RMSE', 'KGE'))
sfrcompare <- calc_split_sample_stats.grouped(stream_combined$Date, stream_combined$sim,
                                              stream_combined$obs, stream_combined$group,
                                              cal_split,
                                              FUNs = list(NSE, rmse, KGE),
                                              FUN_names = c('NSE', 'RMSE', 'KGE'))
#-- Write Out
write.csv(hob_compare, file.path(out_dir, 'hob_compare.csv'))
write.csv(sfrcompare, file.path(out_dir, 'sfr_compare.csv'))
#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# Groundwater Level Plots -------------------------------------------------

#-- Loop over wells & Plot
pdf(file.path(out_dir,'hobs_plots.pdf'), width=11, height=8.5)
for (well in unique(hob$wellnam)) {
  # Report
  message('Plotting ', well)
  # Set up
  w <- gsub('_','',well)
  wsub <- hob[hob$wellnam == well,]
  well_title <- paste('Well: ', w,
                      '\nRow: ', wsub$row[1],
                      '  |  Col: ', wsub$column[1],
                      '  |  Lay: ', wsub$layer[1])
  # Plot
  plot.gw.hydrograph_wMap(wsub$date,
                       wsub$hobs,
                       wsub$sim,
                       xloc = hob_locs[w,'x'],
                       yloc = hob_locs[w,'y'],
                       map_xs = hob_locs[,'x'],
                       map_ys = hob_locs[,'x'],
                       ylabel = 'Groundwater Elevation (m)',
                       title = well_title, map_x_offset = 0, map_y_offset = 0)
}
# Scatterplots
plot.scatterbox(obs = hob$hobs, sim = hob$sim, groups = rep('Wells', nrow(hob)),
                xlab = 'Observed GW Elevation (m)', ylab = 'Simulated GW Elevation (m)', log = F)

dev.off()
#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# Streamflow Plots --------------------------------------------------------

#-- Loop over wells & Plot
pdf(file.path(out_dir,'sfr_plots.pdf'), width=11, height=8.5)
i <- 1
lapply(streams_sim, function(x) {
  # Report
  message('Plotting ', stream_names[i])
  # Set up
  gag_title <- paste0('Gage: ',stream_names[i], '\n', attr(x,'info'))
  # Plot
  plot.stream.hydrograph_wMap(list(streams[[i]]$Date, x$Date),
                       streams[[i]]$Flow,
                       x$Flow_cfs,
                       xloc = sfr_locs[stream_short[i],'x'],
                       yloc = sfr_locs[stream_short[i],'y'],
                       map_xs = sfr_locs$x,
                       map_ys = sfr_locs$y,
                       ylabel = 'Streamflow (cfs)',
                       title = gag_title, map_x_offset=0, map_y_offset=0)
  plot.stream.hydrograph_wMap(list(streams[[i]]$Date, x$Date),
                       streams[[i]]$Flow,
                       x$Flow_cfs,
                       log='y',
                       xloc = sfr_locs[stream_short[i],'x'],
                       yloc = sfr_locs[stream_short[i],'y'],
                       map_xs = sfr_locs$x,
                       map_ys = sfr_locs$y,
                       ylabel = 'Streamflow, log10 (cfs)',
                       title = gag_title, map_x_offset=0, map_y_offset=0)
  i <<- i + 1
})
# Scatterplots
plot.scatterbox(obs = stream_combined$obs, sim = stream_combined$sim, groups = stream_combined$group,
                xlab = 'Observed Streamflow (cfs)', ylab = 'Simulated Streamflow (cfs)', log = F)
plot.scatterbox(obs = stream_combined$obs, sim = stream_combined$sim, groups = stream_combined$group,
                xlab = 'Observed Streamflow (cfs)', ylab = 'Simulated Streamflow (cfs)', log = T)
dev.off()

#-------------------------------------------------------------------------------------------------#

# Pre-post Plots
pdf(file.path(out_dir,'FJ_pre_post.pdf'), width=11, height=8.5)
pre_title <- print.split_sample_stats(sfrcompare, 'Fort Jones', 'Pre')
post_title <- print.split_sample_stats(sfrcompare, 'Fort Jones', 'Post')
plot.pre_post_compare(dates = stream_combined[stream_combined$group == 'Fort Jones','Date'],
                      sim = stream_combined[stream_combined$group == 'Fort Jones','sim'],
                      obs = stream_combined[stream_combined$group == 'Fort Jones', 'obs'],
                      split_date = cal_split,
                      ylabel = 'Streamflow (cfs)',
                      log = 'y',
                      pre_title = pre_title,
                      post_title = post_title)
dev.off()


#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# Volumetric Budget - SWBM ------------------------------------------------------
# swbm_dir = run_dir
# swbm_dir = "C:/Users/Claire/Documents/GitHub/SVIHM/Scenarios/basecase"
SWBM_Terms = c('Precipitation', 'SW Irrigation', 'GW Irrigation', 'ET', 'Recharge', 'Runoff', 'Storage','Error')
SWBM_colors = c('lightblue1',  'darkcyan', 'midnightblue', 'goldenrod','green4', 'mistyrose','black','gray' )

SWBM_Monthly_m3 = read.table(file.path(swbm_dir,'monthly_water_budget.dat'), header = T)
names(SWBM_Monthly_m3) = c('Month',SWBM_Terms)
n_stress = nrow(SWBM_Monthly_m3)

# process dates for plotting
SWBM_Monthly_m3$Month = seq.Date(from = origin_date, by = "month", length.out = n_stress)
SWBM_Monthly_m3$WY = year(SWBM_Monthly_m3$Month)
SWBM_Monthly_m3$WY[month(SWBM_Monthly_m3$Month)>9] = year(SWBM_Monthly_m3$Month[month(SWBM_Monthly_m3$Month)>9]) +1
SWBM_Monthly_m3$Month = format(seq(origin_date, by = "month", length.out = n_stress),'%b-%Y')

SWBM_Annual_m3 = aggregate(.~WY,SWBM_Monthly_m3[,!names(SWBM_Monthly_m3)%in%'Month'], FUN = sum)

# melt data for plot
SWBM_Annual_m3_melt = melt(SWBM_Annual_m3, id.vars = 'WY')
SWBM_Annual_m3_melt$variable = factor(SWBM_Annual_m3_melt$variable, levels = SWBM_Terms)
SWBM_Annual_m3_melt = SWBM_Annual_m3_melt[order(SWBM_Annual_m3_melt$variable),]

# make plots in metric and TAF units
SWBM_Annual_Mm3_Plot = ggplot(SWBM_Annual_m3_melt, aes(x = WY, y = value/1E6)) +
  geom_bar(aes(fill = variable), position = "stack",
           stat = 'identity', color = 'black', width = 0.95, size = 0.1) +
  scale_x_continuous(limits = range(SWBM_Annual_m3$WY),
                     #breaks = seq(1991,2011,by = 2),
                     expand = c(0,0))  +
  scale_y_continuous(limits = c(-300,300), breaks = seq(-300,300,by = 100), expand = c(0,0)) +
  xlab('') +
  ylab(bquote('Volume ('*Mm^3*')')) +
  ggtitle('Soil Zone Annual Budget') +
  scale_fill_manual(values = SWBM_colors)+
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

SWBM_Annual_TAF_Plot = ggplot(SWBM_Annual_m3_melt, aes(x = WY, y = value*0.000810714/1000)) +
  geom_bar(aes(fill = variable), position = "stack", stat = 'identity', color = 'black', width = 0.95, size = 0.1) +
  scale_x_continuous(limits = c(1990.4,2011.6), breaks = seq(1991,2011,by = 2),expand = c(0,0))  +
  scale_y_continuous(limits = c(-300,300), breaks = seq(-300,300,by = 100), expand = c(0,0)) +
  xlab('') +
  ylab('Volume (TAF)') +
  ggtitle('Soil Zone Annual Budget') +
  scale_fill_manual(values = SWBM_colors)+
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


# SWBM Budget troubleshooting 4/6/2023

old_swbm_dir = "C:/Users/Claire/Documents/GitHub/SVIHM/Scenarios/basecase"
new_swbm_dir = swbm_dir

old_poly = read.csv(file.path(data_dir["ref_data_dir","loc"],"polygons_table_ref.csv"),
                    header = T)
new_poly = read.table(file.path(new_swbm_dir, "polygons_table.txt"), header = T)

table(old_poly$SWBM_IRR)
table(new_poly$SWBM_IRR)

old_wb=read.table(file.path(old_swbm_dir, "monthly_water_budget.dat"), header = T)
new_wb = read.table(file.path(new_swbm_dir, "monthly_water_budget.dat"), header = T)
new_wb = new_wb[1:336,]


stream_new_over_old = (new_wb$SW_Irr-old_wb$SW_Irr)/old_wb$SW_Irr
gw_new_over_old = new_wb$GW_Irr/old_wb$GW_Irr
summary(new_wb$GW_Irr); summary(old_wb$GW_Irr)
summary(new_wb$SW_Irr); summary(old_wb$SW_Irr)
summary(new_wb$Precip); summary(old_wb$Precip)


#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# Volumetric Budget - MF -------------------------------------------------------

mfnam <- rmf_read_nam(file.path(mf_dir,'SVIHM.nam'))
mfdis <- rmf_read_dis(file.path(mf_dir,'SVIHM.dis'), nam=mfnam)

bud <- rmf_read_budget(file.path(mf_dir,'SVIHM.lst'))

# Plot!
pdf(file.path(out_dir,'VBudget.pdf'), width=11, height=8.5)
# Over time
p <- rmf_plot(bud, dis = mfdis, type='bar')  # Hacky fake plot
pdates <- mftime2date(sp=1,ts=ggplot_build(p)$layout$panel_params[[1]]$x$breaks, origin_date)
p <- rmf_plot(bud, dis = mfdis, type='bar') + scale_x_continuous("nstp", labels = pdates)
print(p)
p <- rmf_plot(bud, dis = mfdis, type='bar', net=T) + scale_x_continuous("nstp", labels = pdates)
print(p)
# Error
p <- rmf_plot(bud, dis=mfdis, what='difference') + scale_x_continuous("nstp", labels = pdates)
print(p)
p <- rmf_plot(bud, dis=mfdis, what='discrepancy') + scale_x_continuous("nstp", labels = pdates)
print(p)
# Cumulative, gross and net
p <- rmf_plot(bud, dis = mfdis, timesteps=-1, what='cumulative')
print(p)
p <- rmf_plot(bud, dis = mfdis, timesteps=-1, what='cumulative', net=T) +
  geom_text(aes(label = value), color='black', vjust=-1.0)
print(p)
p <- rmf_plot(bud, dis = mfdis, type='bar', what='cumulative', fluxes=c('storage'), net=T) +
     scale_x_continuous("nstp", labels = pdates)
print(p)
# Done
dev.off()
#-------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------#
# Heads/DTW ---------------------------------------------------------------

# Read in MF Files

if (create_sp_charts) {

  mfbas <- RMODFLOW::rmf_read_bas(file.path(mf_dir,'SVIHM.bas'), nam=mfnam, dis=mfdis)
  mfhds <- RMODFLOW::rmf_read_hed(file.path(mf_dir,'SVIHM.hds'), dis=mfdis)

  #-- Loop over SP plotting
  # Heads
  pdf(file.path(out_dir,'Head_Maps.pdf'), width=8.5, height=11)
  for (sp in 1:length(mfdis$perlen)) { #length(mfdis$perlen)) {
    ts <- mfdis$perlen[sp]
    message(paste('Heads - Plotting: SP:'), sp, '| TS:',ts)

    # Find in time
    outdate <- mftime2date(sp, ts, origin_date)

    p <- rmf_plot(mfhds, dis=mfdis, bas=mfbas, k=1, kper=sp, kstp=ts, colour_palette = viridis, legend='Heads') +
         rmf_plot(mfhds, dis=mfdis, bas=mfbas, k=1, kper=sp, kstp=ts, type='contour', label=F, add=T) +
         ggplot2::ggtitle(paste('SVIHM |', outdate,'| SP: ', sp, '- TS:', ts))
    print(p)

  }
  dev.off()

  # Depth to Water (DTW)
  pdf(file.path(out_dir,'DTW_Maps.pdf'), width=8.5, height=11, onefile = T)
  for (sp in 1:length(mfdis$perlen)) {
    ts <- mfdis$perlen[sp]
    message(paste('DTW - Plotting: SP:'), sp, '| TS:',ts)

    # Find in time
    outdate <- mftime2date(sp, ts, origin_date)

    # Prepare
    l <- ifelse(sp == 1, 0, cumsum(mfdis$nstp)[sp-1]) + ifelse(sp < 0, dis$nstp[sp], ts)
    wt <- rmf_convert_hed_to_water_table(mfhds, l = l)
    dtw <- mfdis$top - wt

    p <- rmf_plot(dtw, dis=mfdis, bas=mfbas, k=1, kper=sp, kstp=ts, colour_palette = viridis, legend='Depth to Water') +
         rmf_plot(dtw, dis=mfdis, bas=mfbas, k=1, kper=sp, kstp=ts, type='contour', label=F, add=T) +
         ggplot2::ggtitle(paste('SVIHM |', outdate,'| SP: ', sp, '- TS:', ts))
    print(p)
  }
  dev.off()

  # Flooded Cells
  pdf(file.path(out_dir,'FloodedCells_Maps.pdf'), width=8.5, height=11, onefile = T)
  for (sp in 1:length(mfdis$perlen)) {
    ts <- mfdis$perlen[sp]
    message(paste('FC - Plotting: SP:'), sp, '| TS:',ts)

    # Find in time
    outdate <- mftime2date(sp, ts, origin_date)

    # Prepare
    l <- ifelse(sp == 1, 0, cumsum(mfdis$nstp)[sp-1]) + ifelse(sp < 0, dis$nstp[sp], ts)
    wt <- rmf_convert_hed_to_water_table(mfhds, l = l)
    fc <- wt - mfdis$top
    fc[fc < 0] <- 0

    p <- rmf_plot(fc, dis=mfdis, bas=mfbas, k=1, kper=sp, kstp=ts, colour_palette = inferno, legend='Water Depth Above Surface') +
      ggplot2::ggtitle(paste('SVIHM |', outdate,'| SP: ', sp, '- TS:', ts))

    print(p)
  }
  dev.off()

}
#-------------------------------------------------------------------------------------------------#
