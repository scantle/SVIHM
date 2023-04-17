#-------------------------------------------------------------------------------------------------#

#' Generate Monthly SFR Inflow Partition
#'
#' Inflow to the model domain is often measured downstream of the domain boundary.
#' The upstream inflow may enter the domain on multiple forks/inflow points.
#' This partitioning assigns a fraction of the flow in each subwatershed to
#' different inflow points. Inflow points are assigned to subwatersheds in file
#' SFR_inflow_segments.txt.
#'
#' @param model_start_date Start date of model
#' @param model_end_date End date of model
#' @param unchanging_partition Are the inflow partitioning values unchanging over time? True or false
#'
#' @return
#' @author Claire Kouba, Leland Scantlebury
#' @export
#'
#' @examples
gen_monthly_sfr_flow_partition <- function(model_start_date,
                                     model_end_date,
                                     unchanging_partition = T) {
  model_months = seq(from = model_start_date, to = model_end_date, by = "months")

  if(unchanging_partition){
    # Hard-coded SFR partition. Uses average overall values.
    # Built from SVIHM (2018 version) streamflow records (regressed inputs)
    # and hard-coded SWBM (2018 version) choices (in the read_streamflow subroutine,
    # irrigation.f90 file) that divide measured/estimated tributary flow
    # among upstream inflow segments.

    sfr_unchang_part =  data.frame(inflow_seg_name = c('Scott River',
                                                       'Sugar Creek',
                                                       'Miners Creek',
                                                       'French Creek',
                                                       'Etna Creek',
                                                       'Johnson Creek',
                                                       'Crystal Creek',
                                                       'Patterson Creek',
                                                       'Kidder Creek',
                                                       'Moffett',
                                                       'Mill Creek',
                                                       'Shackleford Creek'),
                                   partition = c(0.875, 0.125, 0.5, 0.5, 1, 0.148,
                                                 0.111, 0.741, 1,   1,   1, 1))
                                   # partition = c(1, 1, 0.5, 0.5, 1, 1, # if we made each record a subwatershed
                                                 # 1, 1, 1,   1,   1, 1))
    # generate SFR_subws_flow_partitioning.txt
    sfr_part_tab = data.frame(modelMonth = model_months)
    for(i in 1:nrow(sfr_unchang_part)){
      # Make a new column named for the tributary.
      # Assign the uniform partition values for the full model period
      sfr_part_tab[,sfr_unchang_part$inflow_seg_name[i]] = sfr_unchang_part$partition[i]
    }
  } else {

    # Placeholder - CK will populate after confirm from GT what data/
    # method is necessary to calculate the monthly-changing
    # values for SFR_subws_flow_partitioning.txt.

  }

  return(sfr_part_tab)
}

#-------------------------------------------------------------------------------------------------#

#' Process Monthly SFR Irrigation Inflows
#'
#' Generates table of monthly stream inflow volumes that are either available for irrigation
#' purposes or are unavailable for irrigation (i.e., flows reserved for environmental uses).
#'
#' @param model_start_date Start date of model
#' @param model_end_date End date of model
#' @param stream_inflow_filename Filename of the gap-filled tributary flow records for each
#' subwatershed (see \code{\link{write_tributary_input_file}}).
#' @param avail_for_irr Boolean. Writes the file "subwatershed_irrigation_inflows.txt" if TRUE
#' and the file "subwatershed_nonirrigation_inflows.txt" if FALSE.
#' @param instream_flow_regime Date frame of start- and end-dates and flow values
#' describing an annual instream flow regime. If NA, produces a 0-flow nonirrigation inflows
#' table or passes 100% of inflow to the irrigation inflows table.
#' @param instream_flow_units Handles "cfs", "cms" or "m3day". Defaults to NA.
#'
#' @return
#' @author Claire Kouba, Leland Scantlebury
#' @export
#'
#' @examples
process_monthly_sfr_inflows <- function(model_start_date,
                                    model_end_date,
                                    scenario_id,
                                    stream_inflow_filename,
                                    avail_for_irr = T,
                                    instream_flow_regime = NA) {

  model_months = seq(from = model_start_date, to = model_end_date, by = "months")

  # Read tributary streamflow inputs from file
  subws_inflow = read.table(stream_inflow_filename, header = T)
  colnames(subws_inflow)[colnames(subws_inflow) == "Month"] = "modelMonth"

  if(avail_for_irr){ # If writing the subwatershed_irrigation_inflows.txt file
    # If no instream flow regime available, make all inflows available for irrigation
    if(is.na(instream_flow_regime)){
      sfr_inflow_tab = subws_inflow
    } else {
      # Process sfr_inflow_tab by subtracting reserved flows from sfr_inflows

    }

  } else if(avail_for_irr == FALSE) { # If writing the subwatershed_nonirrigation_inflows.txt file
    if(is.na(instream_flow_regime)){
      # If no instream flow regime available, reserve no inflows
      sfr_inflow_tab = subws_inflow
      flow_columns = grepl(pattern = "Flow", x = colnames(sfr_inflow_tab), ignore.case = T)
      # Set all monthly reserved flow volumes to 0
      sfr_inflow_tab[,flow_columns] = 0
    } else {
      # process the inflow regime to produce monthly flow volumes for each tributary

    }

  }

  return(sfr_inflow_tab)
}


