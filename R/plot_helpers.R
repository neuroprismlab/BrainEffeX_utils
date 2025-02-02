# Functions for plotting
# These functions describe different effect sizes (Cohen's d or R-squared) and simultaneous confidence intervals (CIs)
# for a given dataset. It allows optional grouping, visualization, and file saving.

#' Function: Plot Simultaeous CIs
#'
#' @param pp A list of plot parameters
#' @param plot_data_list A list containing effect size data for plotting:
#'  - `data`: A list containing sorted & downsampled effect size data & helpers: estimate, cons_estimate, lb, ub, below_cross_idx, and above_cross_idx
#'  - `summary_info`: A list containing extra descriptive info: percent_not_zero, max_cons_effect, group_by_title, and n_title
#'  - `mv_data`: A list containing multivariate effect size data: estimate, lb, and ub
#'  - `study_details`: A list of original study details: orig_stat_type, test_component_1, test_component_2, dataset, map_type, group, and ref
#'
#' @return A ggplot object containing one or overlapping densities
#' @export
#'
#' @examples
#' # Example usage
#' # plot_simci_panel(plot_data)
plot_simci_panel <- function(pp, plot_data_list) {

  # add simci-specific plot params
  pp$non_overlap_colors <- rgb(177/255, 207/255, 192/255, alpha = 0.5)
  pp$overlap_colors <- rgb(237/255, 185/255, 185/255, alpha = 0.5)
  pp$intercept_line_color <- "#ba2d25"
  pp$intercept_line_size <- 0.4
  pp$xlabel <- "Edges / Voxels, sorted by effect size"
  pp$ylabel <- "Effect Size"

  # make plot object

  p <- ggplot()

  for (i in seq_along(plot_data_list)) {

    # extract data from list
    plot_df <- data.frame(
      x = 1:length(plot_data_list[[i]]$data$estimate),
      estimate = plot_data_list[[i]]$data$estimate,
      ub = plot_data_list[[i]]$data$ub,
      lb = plot_data_list[[i]]$data$lb
    )
    below_cross_idx <- plot_data_list[[i]]$data$below_cross_idx
    above_cross_idx <- plot_data_list[[i]]$data$above_cross_idx

    # set y limits
    if (max(abs(c(plot_df$lb,plot_df$ub))) > pp$effect_size_thresh) {
      pp$ylim = pp$effect_size_limits_big
    } else {
      pp$ylim = pp$effect_size_limits_small
    }

    # plot
    p <- p +
      geom_line(data = plot_df, aes(x = x, y = estimate)) +
      geom_ribbon(data = subset(plot_df, x <= below_cross_idx),
                  aes(x = x, ymin = lb, ymax = ub),
                  fill = pp$non_overlap_colors) +
      geom_ribbon(data = subset(plot_df, x >= below_cross_idx & x <= above_cross_idx),
                  aes(x = x, ymin = lb, ymax = ub),
                  fill = pp$overlap_colors) +
      geom_ribbon(data = subset(plot_df, x >= above_cross_idx),
                  aes(x = x, ymin = lb, ymax = ub),
                  fill = pp$non_overlap_colors) +
      geom_hline(yintercept = 0, color = pp$intercept_line_color, linetype = "dashed", size = pp$intercept_line_size)
  }

  p <- p + labs(x = pp$xlabel, y = pp$ylabel) +
    scale_y_continuous(limits = pp$ylim) +
    theme_classic() +
    theme(axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank())

  return(p)

  # original, sans ggplot (base r)
  #   plot(plot_data_list[[i]]$data$estimate, type = "l", ylim = c(min(plot_data_list[[i]]$data$lb, na.rm = TRUE), max(plot_data_list[[i]]$data$ub, na.rm = TRUE)),
  #      xlab = "Edges/Voxels", ylab = "Cohen's d", axes = FALSE)
  #   # green: CI entirely < zero
  #   polygon(c(1:plot_data_list[[i]]$data$below_cross_idx, rev(1:plot_data_list[[i]]$data$below_cross_idx)),
  #           c(plot_data_list[[i]]$data$ub[1:plot_data_list[[i]]$data$below_cross_idx], rev(plot_data_list[[i]]$data$lb[1:plot_data_list[[i]]$data$below_cross_idx])),
  #           col = rgb(177/255, 207/255, 192/255, alpha = 0.5), border = NA)
  #   # red: CI including zero
  #   polygon(c(plot_data_list[[i]]$data$below_cross_idx:plot_data_list[[i]]$data$above_cross_idx, rev(plot_data_list[[i]]$data$below_cross_idx:plot_data_list[[i]]$data$above_cross_idx)),
  #           c(plot_data_list[[i]]$data$ub[plot_data_list[[i]]$data$below_cross_idx:plot_data_list[[i]]$data$above_cross_idx], rev(plot_data_list[[i]]$data$lb[plot_data_list[[i]]$data$below_cross_idx:plot_data_list[[i]]$data$above_cross_idx])),
  #           col = rgb(237/255, 185/255, 185/255, alpha = 0.5), border = NA)
  #   # green: CI entirely > zero
  #   polygon(c(plot_data_list[[i]]$data$above_cross_idx:length(plot_data_list[[i]]$data$ub), rev(plot_data_list[[i]]$data$above_cross_idx:length(plot_data_list[[i]]$data$ub))),
  #           c(plot_data_list[[i]]$data$ub[plot_data_list[[i]]$data$above_cross_idx:length(plot_data_list[[i]]$data$ub)], rev(plot_data_list[[i]]$data$lb[plot_data_list[[i]]$data$above_cross_idx:length(plot_data_list[[i]]$data$ub)])),
  #           col = rgb(177/255, 207/255, 192/255, alpha = 0.5), border = NA)
  #   # horizontal line at y = 0
  #   abline(h = 0, col = "#ba2d25", lty = 3)
  #   # left axis with labels parallel to the axis (las = 1)
  #   axis(2, las = 1)
}


#' Function: Plot Densities
#'
#' @param pp A list of plot parameters
#' @param plot_data_list A list containing effect size data for plotting:
#'  - `data`: A list containing sorted & downsampled effect size data & helpers: estimate, cons_estimate, lb, ub, below_cross_idx, and above_cross_idx
#'  - `summary_info`: A list containing extra descriptive info: percent_not_zero, max_cons_effect, group_by_title, and n_title
#'  - `mv_data`: A list containing multivariate effect size data: estimate, lb, and ub
#'  - `study_details`: A list of original study details: orig_stat_type, test_component_1, test_component_2, dataset, map_type, group, and ref
#'
#' @return A ggplot object containing one or overlapping densities
#' @export
#'
#' @examples
#' # Example usage
#' # plot_density_panel(plot_data)
plot_density_panel <- function(pp, plot_data_list) {

  # add density-specific plot params

  pp$xlim <- c(-0.15, 0.15)
  pp$alpha <- 0.1
  pp$size <- 0.4
  pp$colors__sample_size <- data.frame(labels = c("<1,000", "1,000-5,000", "5,000-10,000", ">10,000"), colors = c("#82A651", "#3AB7BE", "#E7786C", "#B873F7"), breaks_upper_lim = c(1000, 5000, 10000, Inf))
  pp$xlabel <- "Effect Size"
  pp$ylabel <- "Density"

  # make plot object

  p <- ggplot()

  for (i in seq_along(plot_data_list)) {

    # set x limits
    if (max(abs(c(plot_data_list[[i]]$data$lb,plot_data_list[[i]]$data$ub))) > pp$effect_size_thresh) {
      pp$xlim = pp$effect_size_limits_big
    } else {
      pp$xlim = pp$effect_size_limits_small
    }

    # create data for plot with effect size and binned sample size
    sample_size_category <- cut(as.numeric(gsub("n = ", "", plot_data_list[[i]]$extra_study_details$n_title)),
                                breaks = c(-Inf, pp$colors__sample_size$breaks_upper_lim),
                                labels = pp$colors__sample_size$labels)

    p <- p + geom_density(data = data.frame(value = plot_data_list[[i]]$data$cons_estimate, sample_size_category = sample_size_category),
                          aes(x = value, fill = sample_size_category, color = sample_size_category), alpha = pp$alpha, size = pp$size) # for unique color per study, do: fill = i, color = i
  }

  p <- p + theme_minimal() + labs(x = pp$xlabel, y = pp$ylabel) +
    xlim(pp$xlim) +
    theme(legend.position = "none") +
    scale_fill_manual(values = pp$colors__sample_size$colors, breaks = pp$colors__sample_size$labels) +
    scale_color_manual(values = pp$colors__sample_size$colors, breaks = pp$colors__sample_size$labels)

  return(p)
}



#' Add summary labels
#'
#' This function adds summary labels to a ggplot object.
#'
#' @param p A ggplot object to which the labels will be added.
#' @param study_details A list of original study details: orig_stat_type, test_component_1, test_component_2, dataset, map_type, group, and ref.
#' @param extra_study_details A list containing extra descriptive info: percent_not_zero, max_cons_effect, group_by_title, n_title, mv_estimate, and mv_ci.
#'
#' @return A ggplot object with summary labels.
#' @export
#'
#' @examples
#' # Example usage
#' # add_plot_description(p, study_details, extra_study_details)
add_plot_description <- function(p, study_details, extra_study_details) {

  # add description-specific plot params
  pp <- list()
  pp$title_size <- 10
  pp$caption_size <- 8

  pp$grouping_var_title <- switch(extra_study_details$grouping_var, # TODO: move w other pp but beware that singles may not have defined
                                  "none" = "None",
                                  "orig_stat_type" = "Statistic",
                                  "category" = "Outcome Measure")


  if (extra_study_details$grouping_var == 'none') {

    title_text <- paste0("Dataset: ", study_details$dataset, "    |    ",
                         "Test: ", study_details$orig_stat_type, ": ", study_details$test_component_1, ", ", study_details$test_component_2, "    |    ",
                         "Map: ", study_details$map_type, "    |    ",
                         "Sample Size: ", extra_study_details$n_title)

    bottom_text <- paste0("Max conservative effect size: ", extra_study_details$max_cons_effect, "\n",
                          "Percent not overlapping zero: ", round(extra_study_details$percent_not_zero * 100, 1), "%\n",
                          "Multivariate effect size: ", round(extra_study_details$mv_estimate, 2), " [", round(extra_study_details$mv_ci[1], 2), ", ", round(extra_study_details$mv_ci[2], 2), "]")

  } else {

    title_text <- paste0(pp$grouping_var_title, ": ", extra_study_details$group_level, "    |    ",
                         "Reference Space: ", extra_study_details$ref)

    # if field cons_mv_estimate exists in extra_study_details_multi, add to bottom text # TODO: currently not defined when using group_data
    if ("max_cons_mv_estimate" %in% names(extra_study_details)) {
      bottom_text <- paste0("Max conservative effect size: ", extra_study_details$max_cons_effect, "\n",
                            "Percent not overlapping zero: ", round(extra_study_details$percent_not_zero * 100, 1), "%\n",
                            "Max conservative multivariate effect size: ", round(extra_study_details$max_cons_mv_estimate, 2))
    } else {
      bottom_text <- paste0("Max conservative effect size: ", extra_study_details$max_cons_effect, "\n",
                            "Percent not overlapping zero: ", round(extra_study_details$percent_not_zero * 100, 1), "%")
    }
  }

  p <- p +
    ggtitle(title_text) +
    labs(caption = bottom_text) +
    theme(plot.title = element_text(hjust = 0.5, size = pp$title_size, face = "bold"),
          plot.caption = element_text(hjust = 0, size = pp$caption_size))

  return(p)
}


