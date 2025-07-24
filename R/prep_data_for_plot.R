#' Prepare Effect Size Data for Plot
#'
#' This function prepares a given study for effect size plots.
#'
#' @param data A list containing effect size data (e.g., `v$d_clean[[i]]`).
#' @param study_details A list of study details (e.g., `v$study[i, ]`).
#' @param combo_name A string specifying the combo to plot.
#' @param mv_combo_name A string specifying the multivariate combo to plot.
#' @param estimate A string to specify the effect size estimate: "d" or "r_sq"
#' @param plot_info A list containing extra plot information (group_var, level, and reference atlas)
#' @param prep_spatial Logical; whether to prepare data for spatial plotting. Default is FALSE.
#' @param brain_masks A list containing brain mask information for spatial plotting
#'
#' @return A plot visualizing effect sizes and simulated CIs.
#' @export
#'
#' @examples
#' # Example usage
#' \dontrun{
#' plot_info <- list(grouping_var = 'none', group_level = 'test', ref = 'atlas')
#' pd <- prep_data_for_plot(data = v$data[[i]], study_details = v$study[i, ], 
#' plot_info = plot_info, grouping_var='none', 
#' combo_name = "pooling.none.motion.none.mv.none", 
#' mv_combo_name = "pooling.none.motion.none.mv.multi")
#' }
prep_data_for_plot <- function(data, study_details, combo_name, mv_combo_name, estimate = 'd', plot_info = 'NA', prep_spatial = FALSE, brain_masks = NA) {


  # 1. Clean, prep, and downsample data (clean = remove NA's, prep = sort)

  if (estimate == "d") {
    ci_lb <- "sim_ci_lb"
    ci_ub <- "sim_ci_ub"
  } else if (estimate == "r_sq") {
    ci_lb <- "r_sq_sim_ci_lb"
    ci_ub <- "r_sq_sim_ci_ub"
  }


if (!any(is.nan(data[[combo_name]][[estimate]]))) { # check if data exists

  na_idx <- is.na(data[[combo_name]][[estimate]]) | is.na(data[[combo_name]][[ci_lb]]) | is.na(data[[combo_name]][[ci_ub]])
  data[[combo_name]][[estimate]] <- data[[combo_name]][[estimate]][!na_idx]
  data[[combo_name]][[ci_lb]] <- data[[combo_name]][[ci_lb]][!na_idx]
  data[[combo_name]][[ci_ub]] <- data[[combo_name]][[ci_ub]][!na_idx]

  # unlist sim CIs if list
  if (is.list(data[[combo_name]][[ci_lb]])) {
    # data[[combo_name]][[estimate]] <- unlist(data[[combo_name]][[estimate]])
    data[[combo_name]][[ci_lb]] <- unlist(data[[combo_name]][[ci_lb]])
    data[[combo_name]][[ci_ub]] <- unlist(data[[combo_name]][[ci_ub]])
  }


  # sort data from smallest to largest effect size
  sorted_indices <- order(data[[combo_name]][[estimate]])
  sorted_estimate_whole <- data[[combo_name]][[estimate]][sorted_indices]

  # sort confidence intervals by the same order
  sorted_upper_bounds_whole <- data[[combo_name]][[ci_ub]][sorted_indices]
  sorted_lower_bounds_whole <- data[[combo_name]][[ci_lb]][sorted_indices]

  # downsample data for plotting
  if (prep_spatial) { # don't downsample
    downsample <- 1
  } else {
    downsample <- length(sorted_indices) %/% 100
  }

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



  # 2. Calculate conservative effect sizes


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



  # 3. Get additional summary and plot information

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

  # TEMP: quick plot sorted estimate with sorted upper bounds and sorted lower bounds as confidence interval
  # plot(1:length(sorted_estimate), sorted_estimate, type = "n", xlab = "Effect Size", ylab = "Confidence Interval", main = "Effect Size with Confidence Interval")
  # lines(1:length(sorted_estimate), sorted_upper_bounds, col = "red")
  # lines(1:length(sorted_estimate), sorted_lower_bounds, col = "blue")
  # lines(1:length(sorted_estimate), rep(0, length(sorted_estimate)), col = "black")

  # calculate the percent of edges/voxels with confidence intervals that don't overlap with zero:

  percent_below_zero <- sum(sorted_upper_bounds < 0) / length(sorted_upper_bounds)
  percent_above_zero <- sum(sorted_lower_bounds > 0) / length(sorted_lower_bounds)
  percent_not_zero = percent_below_zero + percent_above_zero


  # get extra multivariate info

  # find the full combo name for this multivariate test #TODO: fix the code that creates the data to assign the rest test statistic to the combos
  # if (plot_info$grouping_var == "none") {
  full_mv_combo_name <- names(data)[grepl(mv_combo_name, names(data))]
  # } else {
  #   full_mv_combo_name <- mv_combo_name  # TODO: SN: we can prob always just use the above for now
  # }

  # get MV estimates - TODO: we're not currently getting mv alongside regular
  if (length(full_mv_combo_name) != 0) {
    data[[full_mv_combo_name]][[estimate]] <- data[[full_mv_combo_name]][[estimate]]
    data[[full_mv_combo_name]][[ci_lb]] <- data[[full_mv_combo_name]][[ci_lb]]
    data[[full_mv_combo_name]][[ci_ub]] <- data[[full_mv_combo_name]][[ci_ub]]
  } else { # create a little placeholder so things will still run
    full_mv_combo_name <- mv_combo_name
    data[[full_mv_combo_name]] <- list()
    data[[full_mv_combo_name]][[estimate]] <- NULL
    data[[full_mv_combo_name]][[ci_lb]] <- NULL
    data[[full_mv_combo_name]][[ci_ub]] <- NULL
  }

  if ((full_mv_combo_name %in% names(data))) {
    if (is.list(data[[full_mv_combo_name]][[ci_lb]]) ) {
      data[[full_mv_combo_name]][[ci_lb]] <- unlist(data[[full_mv_combo_name]][[ci_lb]])
      data[[full_mv_combo_name]][[ci_ub]] <- unlist(data[[full_mv_combo_name]][[ci_ub]])
    }
  }

  
  # 3.5 If prep spatial, return to original order
  if (prep_spatial) {
    sorted_estimate <- sorted_estimate[order(sorted_indices)]
    sorted_upper_bounds <- sorted_upper_bounds[order(sorted_indices)]
    sorted_lower_bounds <- sorted_lower_bounds[order(sorted_indices)]
    sorted_cons_estimate <- sorted_cons_estimate[order(sorted_indices)]
  }


  # 4. Return ready-to-plot structure

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
      mv_estimate = data[[full_mv_combo_name]][[estimate]],
      mv_ci = c(data[[full_mv_combo_name]][[ci_lb]], data[[full_mv_combo_name]][[ci_ub]]),
      grouping_var = plot_info$grouping_var,
      group_level = plot_info$group_level,
      ref = plot_info$ref
    ),
    study_details = study_details
  )
  
  # add masks if spatial
  if (prep_spatial) {
    if (length(brain_masks) == 1 && is.na(brain_masks)) {
      stop("prep_spatial is TRUE but brain_masks is NA. Please provide a valid brain_masks data frame.")
    }
    plot_data$extra_study_details$brain_masks <- brain_masks
  }

  return(plot_data)

} else {
  return(NULL)
}

}
