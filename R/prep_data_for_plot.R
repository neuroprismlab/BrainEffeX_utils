#' Prepare Effect Size Data for Plot
#'
#' This function prepares a given study for effect size plots.
#'
#' @param data A list containing effect size data (e.g., `v$d_clean[[i]]`).
#' @param name A string specifying the study name (e.g., `names(v$d_clean[i])`).
#' @param study_details A list of study details (e.g., `v$study[i, ]`).
#' @param combo_name A string specifying the combo to plot.
#' @param mv_combo_name A string specifying the multivariate combo to plot.
#' @param group_by A string to specify grouping: "none", "orig_stat_type", or "category".
#' @param estimate A string to specify the effect size estimate: "d" or "r_sq"
#'
#' @return A plot visualizing effect sizes and simulated CIs.
#' @export
#'
#' @examples
#' # Example usage
#' # plot_sim_ci(data = data, name = "abcd_fc_r_rest_bmi_z", study_details = study, 
#' #            combo_name = "pooling.none.motion.none.mv.none", mv_combo_name = "pooling.none.motion.none.mv.multi")
#' pd <- prep_data_for_plot(data = v$data[[i]], name = names(v$data[i]), study_details = v$study[i, ], group_by='none', 
#'              combo_name = "pooling.none.motion.none.mv.none", mv_combo_name = "pooling.none.motion.none.mv.multi")
prep_data_for_plot <- function(data, name, study_details, combo_name, mv_combo_name, group_by = 'none', estimate = 'd') {
  
  if (estimate == "d") {
    ci_lb <- "sim_ci_lb"
    ci_ub <- "sim_ci_ub"
  } else if (estimate == "r_sq") {
    ci_lb <- "r_sq_sim_ci_lb"
    ci_ub <- "r_sq_sim_ci_ub"
  }
  
  group_by_title <- switch(group_by,
                           "none" = "None",
                           "orig_stat_type" = "Statistic",
                           "category" = "Phenotype Category")
  
  # find the full combo name for this multivariate test #TODO: fix the code that creates the data to assign the rest test statistic to the combos
  if (group_by == "none") {
    full_mv_combo_name <- names(data)[grepl(mv_combo_name, names(data))] # TODO: SN: why does this happen? in case the whole name isn't provided?
  } else {
    full_mv_combo_name <- mv_combo_name
  }

  # remove na
  na_idx <- is.na(data[[combo_name]][[estimate]]) | is.na(data[[combo_name]][[ci_lb]]) | is.na(data[[combo_name]][[ci_ub]])
  data[[combo_name]][[estimate]] <- data[[combo_name]][[estimate]][!na_idx]
  data[[combo_name]][[ci_lb]] <- data[[combo_name]][[ci_lb]][!na_idx]
  data[[combo_name]][[ci_ub]] <- data[[combo_name]][[ci_ub]][!na_idx]

  # unlist sim CIs if list
  if (is.list(data[[combo_name]][[ci_lb]])) {
    data[[combo_name]][[ci_lb]] <- unlist(data[[combo_name]][[ci_lb]])
    data[[full_mv_combo_name]][[ci_lb]] <- unlist(data[[full_mv_combo_name]][[ci_lb]])
  }
  if (is.list(data[[combo_name]][[ci_ub]])) {
    data[[combo_name]][[ci_ub]] <- unlist(data[[combo_name]][[ci_ub]])
    data[[full_mv_combo_name]][[ci_ub]] <- unlist(data[[full_mv_combo_name]][[ci_ub]])
  }

  # sort data from smallest to largest effect size
  sorted_indices <- order(data[[combo_name]][[estimate]])
  sorted_estimate_whole <- data[[combo_name]][[estimate]][sorted_indices]
  
  # sort confidence intervals by the same order
  sorted_upper_bounds_whole <- data[[combo_name]][[ci_ub]][sorted_indices]
  sorted_lower_bounds_whole <- data[[combo_name]][[ci_lb]][sorted_indices]
  
  # downsample data for plotting
  downsample <- length(sorted_indices) %/% 100
  
  if (downsample < 1) {
    downsample = 1
  }
  sorted_estimate <- sorted_estimate_whole[seq(1, length(sorted_estimate_whole), by = downsample)]
  # to include the last element of the sorted data, check if the last element of sorted_estimate is the same as the last element of sorted_estimate_whole
  if (sorted_estimate[length(sorted_estimate)] != sorted_estimate_whole[length(sorted_estimate_whole)]) {
    sorted_estimate <- c(sorted_estimate, sorted_estimate_whole[length(sorted_estimate_whole)])
  }
  sorted_upper_bounds <- sorted_upper_bounds_whole[seq(1, length(sorted_upper_bounds_whole), by = downsample)]
  # check if the last element of sorted_upper_bounds is the same as the last element of sorted_upper_bounds_whole
  if (sorted_upper_bounds[length(sorted_upper_bounds)] != sorted_upper_bounds_whole[length(sorted_upper_bounds_whole)]) {
    sorted_upper_bounds <- c(sorted_upper_bounds, sorted_upper_bounds_whole[length(sorted_upper_bounds_whole)])
  }
  sorted_lower_bounds <- sorted_lower_bounds_whole[seq(1, length(sorted_lower_bounds_whole), by = downsample)]
  # check if the last element of sorted_lower_bounds is the same as the last element of sorted_lower_bounds_whole
  if (sorted_lower_bounds[length(sorted_lower_bounds)] != sorted_lower_bounds_whole[length(sorted_lower_bounds_whole)]) {
    sorted_lower_bounds <- c(sorted_lower_bounds, sorted_lower_bounds_whole[length(sorted_lower_bounds_whole)])
  }
  
  # for coloring of confidence intervals:
  below_zero <- sorted_upper_bounds < 0
  below_cross_idx <- which(diff(below_zero) == -1) + 1# the last TRUE before switch
  
  above_zero <- sorted_lower_bounds > 0
  above_cross_idx <- (which(diff(above_zero) == 1)) + 1 # the last FALSE before switch to true
  
  # if there are no values below zero, set the index to 1
  if (length(below_cross_idx) == 0) {
    below_cross_idx = 1
  } 
  
  # if there are no values above zero, set the index to the end
  if (length(above_cross_idx) == 0) {
    above_cross_idx = length(above_zero)
  } 
  
  # calculate conservative effect sizes
  # conservative effect size = elementwise min of abs of upper and lower bound, but keep sign
  # sorted_cons_estimate_whole <- ifelse((abs(data[[combo_name]][[ci_lb]]) > abs(data[[combo_name]][[ci_ub]])), 
  #                                      ifelse((data[[combo_name]][[ci_lb]] > 0),
  #                                             0, round(data[[combo_name]][[ci_lb]], 2), 0),
  #                                      ifelse((data[[combo_name]][[ci_ub]] < 0), round(data[[combo_name]][[ci_ub]], 2), 0))
  # sorted_cons_estimate_whole <- sorted_cons_estimate_whole[sorted_indices]
  
  sorted_cons_estimate <- ifelse((abs(sorted_lower_bounds) > abs(sorted_upper_bounds)), 
                                 ifelse((sorted_upper_bounds < 0),
                                        round(sorted_upper_bounds, 2), 0),
                                 ifelse((sorted_lower_bounds > 0), 
                                        round(sorted_lower_bounds, 2), 0))
  
  max_cons_estimate <- sorted_cons_estimate[which.max(abs(sorted_cons_estimate))] # should be either first or last of sorted entry
  
  
  
  # calculate the percent of edges/voxels with confidence intervals that don't overlap with zero:
  percent_below_zero <- sum(sorted_upper_bounds < 0) / length(sorted_upper_bounds)
  percent_above_zero <- sum(sorted_lower_bounds > 0) / length(sorted_lower_bounds)
  percent_not_zero = percent_below_zero + percent_above_zero
  

  
  # Return ready-to-plot structure
  
  plot_data <- list(
    data = list(
      estimate = sorted_estimate,
      ub = sorted_upper_bounds,
      lb = sorted_lower_bounds,
      cons_estimate = sorted_cons_estimate,
      below_cross_idx = below_cross_idx,
      above_cross_idx = above_cross_idx
    ),
    extra_study_details = list(
      percent_not_zero = percent_not_zero,
      max_cons_estimate = max_cons_estimate,
      n_title = paste0("n = ", data[[combo_name]]$n), # TODO: this should not be defined if group_type != "none",
      group_by_title = group_by_title,
      mv_estimate = data[[full_mv_combo_name]][[estimate]],
      mv_ci = c(data[[full_mv_combo_name]][[ci_lb]], data[[full_mv_combo_name]][[ci_ub]])
    ),
    study_details = study_details
  )
  
  return(plot_data)
  
  
}
