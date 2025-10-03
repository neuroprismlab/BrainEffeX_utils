# Simplified version of plotting functions to create single effect size plots


#' Single effect size plot with simultaneous confidence intervals
#'
#' This function plots a single effect size plot with simultaneous confidence intervals for a specified study.
#'
#' @param study_name The name of the study to plot (string).
#' @param v_data A data list containing effect size data (download data from OSF: https://doi.org/10.17605/OSF.IO/CWNJD, or format your data to the same format)
#' @param estimate The effect size measure to use ('d' or 'r_sq'). Default is 'd'.
#' @param motion_method The motion correction method used ('none', 'regression', 'threshold' for 0.1mm threshold, or 'threshold2' for 0.2mm threshold). Default is 'none'.
#' @param pooling_method The pooling method used ('none', or 'net' for network-based pooling). Default is 'none'.
#' @param add_description Logical indicating whether to add a description to the plot. Default is TRUE.
#' @param do_minimal_title Logical indicating whether to use a minimal title. Default is FALSE.
#' 
#' @return A ggplot object representing the effect size plot with simultaneous confidence intervals.
#' @export
#'
#' @examples
#' # Example usage
#' \dontrun{
#' plot_simci("abcd_fc_r_rest_bmi", v_data = v) # v downloaded from OSF: https://doi.org/10.17605/OSF.IO/CWNJD
#' }
#' 
#' @import ggplot2

plot_one_simci <- function(study_name, v_data, estimate = 'd', motion_method = 'none', pooling_method = 'none', add_description = TRUE, do_minimal_title = FALSE) {

  plot_info <- list(grouping_var = 'category', group_level = v_data$study[v_data$study$name == study_name,"category"], ref = v_data$study[v_data$study$name == study_name,"ref"])

  combo_name = paste0('pooling.', pooling_method, '.motion.', motion_method, '.mv.none')

  prepped <- prep_data_for_plot(data = v_data$data[[study_name]], study_details = v_data$study[v_data$study$name == study_name, ], 
                               combo_name = paste0('pooling.', pooling_method, '.motion.', motion_method, '.mv.none'), 
                               mv_combo_name = paste0('pooling.', pooling_method, '.motion.', motion_method, '.mv.multi'),
                               estimate = estimate, plot_info = plot_info, prep_spatial = FALSE, brain_masks = NA)

  plot_data_list <- list(prepped)

  plot_data_list[[1]]$extra_study_details$grouping_var <- 'none'

  pp <- list()
  pp$effect_size_measure <- estimate
  if (estimate == 'r_sq') {
    pp$effect_size_measure <- 'R^2' # rename to shorten
    pp$effect_size_thresh <- 0.1
    pp$effect_size_limits_big <- c(-0.25, 0.5)
    pp$effect_size_limits_small <- c(-0.02, 0.1)
    pp$effect_size_limits_smaller <- c(-0.01, 0.05)
    pp$reference_xlimits <- c(0, 0.005)
  } else { # using d limits for all other effect types
    pp$effect_size_thresh <- 0.5
    pp$effect_size_limits_big <- c(-1.2, 1.2)
    pp$effect_size_limits_small <- c(-0.5, 0.5)
    pp$effect_size_limits_smaller <- c(-0.15, 0.15)
    pp$reference_xlimits <- c(-0.1, 0.1)
  }

  pp$axis_title_size = element_text(size = 16) 
  pp$axis_text_size = element_text(size = 16)

  p <- plot_simci_panel(pp, plot_data_list)

  summary_info <- get_summary_info(plot_data_list[[1]]$study_details, plot_data_list[[1]]$extra_study_details)

  p <- add_plot_description(p, pp, summary_info, add_description, FALSE, title_size = 12)
  
  return(p)
}