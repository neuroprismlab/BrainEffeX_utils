#' Create plots
#'
#' This function is the master plotter for making effect size plots for individual
#' studies, overlapping studies, or meta-analytic results.
#'
#' @param plot_data_list A list of lists containing effect size data for plotting:
#'  - plot_data_list[i] = `plot_data`: contains:
#'    - `data`: A list containing sorted & downsampled effect size data & helpers: estimate, cons_estimate, lb, ub, below_cross_idx, and above_cross_idx
#'    - `extra_study_details`: A list containing extra descriptive info: percent_not_zero, max_cons_effect, group_by_title, n_title, mv_estimate, and mv_ci
#'    - `study_details`: A list of original study details: orig_stat_type, test_component_1, test_component_2, dataset, map_type, group, and ref
#' @param plot_type String; which plot type to make. Options are 'density' or 'simci'. Default is "simci".
#' @param add_description Logical; whether to add a description to the plot. Default is `FALSE`.
#'
#' @return A plot visualizing effect sizes and simulated CIs.
#' @export
#'
#' @examples
#' # Example usage
#' # create_plots(pd)
create_plots <- function(plot_data_list, plot_type = 'simci', add_description = FALSE, summary_info = NULL) {

  library(ggplot2)

  # General plot parameters
  pp <- list()
  # if (add_description) {   # if add description: set up with bigger margins
  #   pp$mar <- c(7, 7, 7, 10)
  # } else {
  #   pp$mar <- c(3, 4, 5, 2)
  # }
  pp$effect_size_limits_big <- c(-1.2, 1.2)
  pp$effect_size_limits_small <- c(-0.5, 0.5)
  pp$effect_size_limits_smaller <- c(-0.15, 0.15)
  pp$rsq_effect_size_limits <- c(-0.25, 0.5)
  pp$rsq_effect_size_limits_smaller <- c(-0.01, 0.05)
  pp$effect_size_thresh <- 0.5
  pp$axis_title_size = element_text(size = 16)
  pp$axis_text_size = element_text(size = 16)

  pp$plot_detail_style <- 'Shiny' # c('manuscript', 'Shiny') # for add_description

  # General setup

  # if it's a single study, nest it into a list so we can use the below loop
  if (!"data" %in% names(plot_data_list[[1]])) {
    plot_data_list <- list(plot_data_list)     # TODO: can record here that it was passed as a single study
  }

  # Make plot(s)

  # par(mar=pp$mar)
  if (plot_type == 'simci') {

    p <- plot_simci_panel(pp, plot_data_list)
    # p <- plot_simci_panel(plot_data_list)

  } else if (plot_type == 'density') {

    p <- plot_density_panel(pp, plot_data_list)
    # p <- plot_density_panel(plot_data_list)

  } else if (plot_type == 'spatial') {

    if (plot_data_list[[1]]$extra_study_details$ref[[1]] == 'voxel') { # TODO: could be
      p <- plot_activation_panel(pp, plot_data_list)
    } else {
      p <- plot_connectivity_panel(pp, plot_data_list)
    }

  } else {
    error('Please specify simci, density, or spatial')
  }

  # Add extra info, if specified

    p <- add_plot_description(p, pp, summary_info, add_description)

  return(p)
}
