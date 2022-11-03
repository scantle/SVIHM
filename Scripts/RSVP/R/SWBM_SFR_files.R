#-------------------------------------------------------------------------------------------------#

#' Generate Daily Binary Crop Coefficients (Kc)
#'
#' Currently used for alfalfa and pasture. Binary coefficients for growing/dormant.
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

