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
#' @param do_minimal_title Logical; whether to shorten title. Default is `FALSE`.
#'
#' @return A plot visualizing effect sizes and simulated CIs.
#' @export
#'
#' @examples
#' # Example usage
#' # create_plots(pd)
create_plots <- function(plot_data_list, plot_type = 'simci', effect_type = 'd', do_multivariate = FALSE, add_description = FALSE, do_minimal_title = FALSE, summary_info = NULL) {
  
  library(ggplot2)

  # General plot parameters
  pp <- list()
  pp$effect_size_measure <- effect_type
  if (effect_type == 'r_sq') {
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
  if (do_multivariate) {
    pp$mv_multiplier <- 20
    pp$effect_size_limits_big <- pp$effect_size_limits_big * pp$mv_multiplier
    pp$effect_size_limits_small <- pp$effect_size_limits_small * pp$mv_multiplier
    pp$effect_size_limits_smaller <- pp$effect_size_limits_smaller * pp$mv_multiplier
  }
  pp$colors__sample_size <- data.frame(labels = c("<1,000", "1,000-5,000", "5,000-10,000", ">10,000", "NA"), colors = c("#82A651", "#3AB7BE", "#E7786C", "#B873F7","#B59410"), breaks_upper_lim = c(1000, 5000, 10000, 999999999, Inf))
  
  # for binning results - TESTING
  pp$effect_size_bins <- c(0, 0.05, 0.2, 0.5, 0.8, 1.5, 2.5, Inf)
  pp$effect_size_bin_labels <- c('Extremely Small','Very Small','Small','Medium','Large','Very Large','Extremely Large')
  pp$power_bins <- c(0, 0.2, 0.5, 0.8, 1, Inf)
  pp$power_bin_labels <- c('Very Low','Low','Medium','High', 'Very High')
  pp$sample_size_bins <- c(0, 25, 50, 100, 500, 1000, 5000, Inf)
  pp$sample_size_bin_labels <- c('Lab','Lab+','Center','Consortium','Consortium+','Large Consortium','Massive Consortium')
  
  pp$axis_title_size = element_text(size = 16) 
  pp$axis_text_size = element_text(size = 16)

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

  } else if (grepl('density', plot_type)) {
    
    if (grepl('binned', plot_type)) {
      pp$use_effect_size_bin <- TRUE
    } else {
      pp$use_effect_size_bin <- FALSE
    }
    
    p <- plot_density_panel(pp, plot_data_list, use_effect_size_bin = pp$use_effect_size_bin)
    
  } else if (plot_type == 'spatial') {

    if (plot_data_list[[1]]$extra_study_details$ref[[1]] == 'voxel') { # TODO: could be
      p <- plot_activation_panel(pp, plot_data_list)
    } else {
      p <- plot_connectivity_panel(pp, plot_data_list)
    }

  } else if (grepl('power', plot_type)) {
    
    if (grepl('power_n', plot_type)) {
      output_type <- 'n'
    } else {
      output_type <- 'power'
    }
    
    if (grepl('binned', plot_type)) {
      pp$use_effect_size_bin <- TRUE
    } else {
      pp$use_effect_size_bin <- FALSE
    }
    
    p <- plot_power_panel(pp, plot_data_list, output_type, use_effect_size_bin = pp$use_effect_size_bin)
  
  } else {
    error('Please specify simci, density, or spatial')
  }

  # Add extra info, if specified

  p <- add_plot_description(p, pp, summary_info, add_description, do_minimal_title)

  return(p)
}
