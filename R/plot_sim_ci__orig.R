#' Plot Simulated Confidence Intervals
#'
#' This function plots effect sizes (Cohen's d) and simulated confidence intervals (CIs) 
#' for a given dataset. It allows optional grouping, visualization, and file saving.
#'
#' @param data A list containing effect size data (e.g., `v$d_clean[[i]]`).
#' @param name A string specifying the study name (e.g., `names(v$d_clean[i])`).
#' @param study_details A list of study details (e.g., `v$study[i, ]`).
#' @param combo_name A string specifying the combo to plot.
#' @param mv_combo_name A string specifying the multivariate combo to plot.
#' @param group_by A string to specify grouping: "none", "orig_stat_type", or "category".
#' @param estimate A string to specify the effect size estimate: "d" or "r_sq"
#' @param save Logical; whether to save the plot as a PNG file. Default is `FALSE`.
#' @param out_path A string specifying the output directory for saving plots. Default is "output".
#' @param file_name A string for the saved file name. Default is "plot".
#'
#' @return A plot visualizing effect sizes and simulated CIs.
#' @export
#'
#' @examples
#' # Example usage
#' # plot_sim_ci(data = data, name = "abcd_fc_r_rest_bmi_z", study_details = study, 
#' #            combo_name = "pooling.none.motion.none.mv.none", mv_combo_name = "pooling.none.motion.none.mv.multi")
plot_sim_ci <- function(data, name, study_details, combo_name, mv_combo_name, group_by = 'none', estimate = 'd', save = FALSE, out_path = 'output', file_name = 'plot') {
  if (save) {
    out_name = paste0(out_path, '/', file_name)
    png(out_name)
  }
  
  if (estimate == "d") {
    ci_lb <- "sim_ci_lb"
    ci_ub <- "sim_ci_ub"
  } else if (estimate == "r_sq") {
    ci_lb <- "r_sq_sim_ci_lb"
    ci_ub <- "r_sq_sim_ci_ub"
  }
  
  #TODO: change variable names from d to estimate within this function to
  # make less confusing. Not urgent.
  
  # find the full combo name for this multivariate test #TODO: fix the code that creates the data to assign the rest test statistic to the combos
  full_mv_combo_name <- names(data)[grepl(mv_combo_name, names(data))]

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

  # sort data from smallest to largest d
  sorted_indices <- order(data[[combo_name]][[estimate]])
  sorted_d_whole <- data[[combo_name]][[estimate]][sorted_indices]
  # sort confidence intervals by the same order
  sorted_upper_bounds_whole <- data[[combo_name]][[ci_ub]][sorted_indices]
  sorted_lower_bounds_whole <- data[[combo_name]][[ci_lb]][sorted_indices]
  
  # downsample data for plotting
  downsample <- length(sorted_indices) %/% 100
  if (downsample < 1) {
    downsample = 1
  }
  sorted_d <- sorted_d_whole[seq(1, length(sorted_d_whole), by = downsample)]
  # to include the last element of the sorted data, check if the last element of sorted_d is the same as the last element of sorted_d_whole
  if (sorted_d[length(sorted_d)] != sorted_d_whole[length(sorted_d_whole)]) {
    sorted_d <- c(sorted_d, sorted_d_whole[length(sorted_d_whole)])
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
  
  if (group_by == 'none') {
    n_title <- paste0("n = ", data[[combo_name]]$n)
  }
  
  # calculate the percent of edges/voxels with confidence intervals that don't overlap with zero:
  percent_below_zero <- sum(sorted_upper_bounds < 0) / length(sorted_upper_bounds)
  percent_above_zero <- sum(sorted_lower_bounds > 0) / length(sorted_lower_bounds)
  percent_not_zero = percent_below_zero + percent_above_zero
  
  # if there are no values below zero, set the index to 1
  if (length(below_cross_idx) == 0) {
    below_cross_idx = 1
  } 
  
  # if there are no values above zero, set the index to the end
  if (length(above_cross_idx) == 0) {
    above_cross_idx = length(above_zero)
  } 
  
  # plot a line for d
  par(mar=c(3, 4, 5, 2))
  plot(sorted_d, type = "l", ylim = c(min(sorted_lower_bounds, na.rm = TRUE), max(sorted_upper_bounds, na.rm = TRUE)),
       xlab = "Edges/Voxels", ylab = "Cohen's d", axes = FALSE)
  # add a horizontal line at y = 0
  abline(h = 0, col = "#ba2d25", lty = 3)
  axis(2, las = 1)  # Add left axis with labels parallel to the axis (las = 1)
  if (group_by == 'none') {

          legend("topleft", inset = c(-0.1, -0.5), 
            legend = c(bquote(bold("Test: ") ~ .(study_details$orig_stat_type) ~ ": " ~ .(study_details$test_component_1) ~ ", " ~ .(study_details$test_component_2)),
                      bquote(bold("Dataset: ") ~ .(study_details$dataset)), 
                      "",
                      bquote(bold("Map: ") ~ .(study_details$map_type)),
                      "",
                      bquote(bold("Sample Size: ") ~ .(n_title))),
                      col = 2, bty = "n", cex = 1, text.width = 25, xpd = TRUE, ncol = 3)

  } else if (group_by == "orig_stat_type") {
    legend("topleft", inset = c(-0.1, -0.5),
       legend = c(
         bquote(bold("Statistic:")), 
         paste(study_details$group, "  "),
         bquote(bold("Reference Space:")),
          paste(study_details$ref, "  ")
       ), 
       bty = "n", ncol = 2, cex = 1, x.intersp = 0.0, xpd = TRUE)
  } else if (group_by == "category") {
    legend("topleft", inset = c(-0.1, -0.5),
       legend = c(
         bquote(bold("Phenotype Category:")), 
         paste(study_details$group, "  "),
         bquote(bold("Reference Space:")),
          paste(study_details$ref, "  ")
       ), 
       bty = "n", ncol = 2, cex = 1, x.intersp = 0.0, xpd = TRUE)
  }
  max_cons_effect = ifelse((abs(max(data[[combo_name]][[ci_lb]], na.rm = TRUE)) > abs(min(data[[combo_name]][[ci_ub]], na.rm = TRUE))), 
                                                              ifelse((max(data[[combo_name]][[ci_lb]], na.rm = TRUE) > 0),
                                                                     round(abs(max(data[[combo_name]][[ci_lb]], na.rm = TRUE)), 2), 0),
                                                              ifelse((min(data[[combo_name]][[ci_ub]], na.rm = TRUE) < 0), round(abs(min(data[[combo_name]][[ci_ub]], na.rm = TRUE)), 2), 0))

  if (group_by == 'none') {
    legend("bottomleft", inset = c(0, -0.5), legend = c(bquote(bold("Max conservative effect size: ") ~ .(max_cons_effect)), 
                                                          bquote(bold("Percent not overlapping zero: ") ~.(round(percent_not_zero * 100, 1)) ~ "%"),
                                                          bquote(bold("Multivariate effect size: ") ~.(round(data[[full_mv_combo_name]][[estimate]], 2)) ~ "  [" ~.(round(data[[full_mv_combo_name]][[ci_lb]], 2)) ~ ", " ~.(round(data[[full_mv_combo_name]][[ci_ub]], 2)) ~ "]")), col = 1, bty = "n", cex = 1, x.intersp = 0, xpd = TRUE)
  } else {
    legend("bottomleft", inset = c(0, -0.5), legend = c(bquote(bold("Max conservative effect size: ") ~ .(max_cons_effect)), 
                                                          bquote(bold("Percent not overlapping zero: ") ~.(round(percent_not_zero * 100, 1)) ~ "%")),
                                                          col = 1, bty = "n", cex = 1, x.intersp = 0, xpd = TRUE)
  }
 
  
  # plot and shade the cofidence intervals:
  # green for intervals that are entirely below zero
  polygon(c(1:below_cross_idx, rev(1:below_cross_idx)), 
          c(sorted_upper_bounds[1:below_cross_idx], rev(sorted_lower_bounds[1:below_cross_idx])), 
          col = rgb(177/255, 207/255, 192/255, alpha = 0.5), border = NA)
  
  # red for intervals that include zero
  polygon(c(below_cross_idx:above_cross_idx, rev(below_cross_idx:above_cross_idx)), 
          c(sorted_upper_bounds[below_cross_idx:above_cross_idx], rev(sorted_lower_bounds[below_cross_idx:above_cross_idx])), 
          col = rgb(237/255, 185/255, 185/255, alpha = 0.5), border = NA)
  
  # green for intervals that are entirely above zero
  polygon(c(above_cross_idx:length(above_zero), rev(above_cross_idx:length(above_zero))), 
          c(sorted_upper_bounds[above_cross_idx:length(above_zero)], rev(sorted_lower_bounds[above_cross_idx:length(above_zero)])), 
          col = rgb(177/255, 207/255, 192/255, alpha = 0.5), border = NA)
  
  if (save) {
    dev.off()
  }
}
