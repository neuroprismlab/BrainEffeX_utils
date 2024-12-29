#' Plot Simultaneous Confidence Intervals
#'
#' This function plots effect sizes (Cohen's d or R-squared) and simulated confidence intervals (CIs) 
#' for a given dataset. It allows optional grouping, visualization, and file saving.
#'
#'
#' @param plot_data A list containing effect size data for plotting:
#'  - `data`: A list containing sorted & downsampled effect size data & helpers: estimate, cons_estimate, lb, ub, below_cross_idx, and above_cross_idx
#'  - `summary_info`: A list containing extra descriptive info: percent_not_zero, max_cons_effect, group_by_title, and n_title
#'  - `mv_data`: A list containing multivariate effect size data: estimate, lb, and ub
#'  - `study_details`: A list of original study details: orig_stat_type, test_component_1, test_component_2, dataset, map_type, group, and ref
#' @param save Logical; whether to save the plot as a PNG file. Default is `FALSE`.
#' @param out_path A string specifying the output directory for saving plots. Default is "output".
#' @param file_name A string for the saved file name. Default is "plot".
#'
#' @return A plot visualizing effect sizes and simulated CIs.
#' @export
#'
#' @examples
#' # Example usage
#' # plot_sim_ci2(pd)
plot_sim_ci2 <- function(plot_data, save = FALSE, out_path = 'output', file_name = 'plot') {
  
  # extract data
  data = plot_data$data
  summary_info = plot_data$summary_info
  mv_data = plot_data$mv_data
  study_details = plot_data$study_details
  
  
  if (save) {
    out_name = paste0(out_path, '/', file_name)
    png(out_name)
  }
  
  
  # plot a line for d
  par(mar=c(3, 4, 5, 2))
  plot(data$estimate, type = "l", ylim = c(min(data$lb, na.rm = TRUE), max(data$ub, na.rm = TRUE)),
       xlab = "Edges/Voxels", ylab = "Cohen's d", axes = FALSE)
  
  # plot and shade the confidence intervals:
  #   green for intervals that are entirely below zero
  polygon(c(1:data$below_cross_idx, rev(1:data$below_cross_idx)), 
          c(data$ub[1:data$below_cross_idx], rev(data$lb[1:data$below_cross_idx])), 
          col = rgb(177/255, 207/255, 192/255, alpha = 0.5), border = NA)
  
  #   red for intervals that include zero
  polygon(c(data$below_cross_idx:data$above_cross_idx, rev(data$below_cross_idx:data$above_cross_idx)), 
          c(data$ub[data$below_cross_idx:data$above_cross_idx], rev(data$lb[data$below_cross_idx:data$above_cross_idx])), 
          col = rgb(237/255, 185/255, 185/255, alpha = 0.5), border = NA)
  
  #   green for intervals that are entirely above zero
  polygon(c(data$above_cross_idx:length(data$ub), rev(data$above_cross_idx:length(data$ub))), 
          c(data$ub[data$above_cross_idx:length(data$ub)], rev(data$lb[data$above_cross_idx:length(data$ub)])), 
          col = rgb(177/255, 207/255, 192/255, alpha = 0.5), border = NA)
  
  # add a horizontal line at y = 0
  abline(h = 0, col = "#ba2d25", lty = 3)
  
  
  # add labels and legends 
  
  axis(2, las = 1)  # Add left axis with labels parallel to the axis (las = 1)
  
  if (summary_info$group_by_title == 'None') {

          legend("topleft", inset = c(-0.1, -0.5), 
            legend = c(bquote(bold("Test: ") ~ .(study_details$orig_stat_type) ~ ": " ~ .(study_details$test_component_1) ~ ", " ~ .(study_details$test_component_2)),
                      bquote(bold("Dataset: ") ~ .(study_details$dataset)), 
                      "",
                      bquote(bold("Map: ") ~ .(study_details$map_type)),
                      "",
                      bquote(bold("Sample Size: ") ~ .(summary_info$n_title))),
                      col = 2, bty = "n", cex = 1, text.width = 25, xpd = TRUE, ncol = 3)

  } else {
    legend("topleft", inset = c(-0.1, -0.5),
       legend = c(
         bquote(bold(paste(summary_info$group_by_title, ": "))),
         paste(study_details$group, "  "),
         bquote(bold("Reference Space:")),
          paste(study_details$ref, "  ")
       ), 
       bty = "n", ncol = 2, cex = 1, x.intersp = 0.0, xpd = TRUE)
  }

  if (summary_info$group_by_title == 'None') {
    legend("bottomleft", inset = c(0, -0.5), legend = c(bquote(bold("Max conservative effect size: ") ~ .(summary_info$max_cons_effect)), 
                                                          bquote(bold("Percent not overlapping zero: ") ~.(round(summary_info$percent_not_zero * 100, 1)) ~ "%"),
                                                          bquote(bold("Multivariate effect size: ") ~.(round(mv_data$estimate, 2)) ~ "  [" ~.(round(mv_data$lb, 2)) ~ ", " ~.(round(mv_data$ub, 2)) ~ "]")), col = 1, bty = "n", cex = 1, x.intersp = 0, xpd = TRUE)
  } else {
    legend("bottomleft", inset = c(0, -0.5), legend = c(bquote(bold("Max conservative effect size: ") ~ .(summary_info$max_cons_effect)), 
                                                          bquote(bold("Percent not overlapping zero: ") ~.(round(summary_info$percent_not_zero * 100, 1)) ~ "%")),
                                                          col = 1, bty = "n", cex = 1, x.intersp = 0, xpd = TRUE)
  }
 
  
  if (save) {
    dev.off()
  }
}
