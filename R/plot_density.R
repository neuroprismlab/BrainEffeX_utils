#' Plot Conservative Effect Size Densities
#'
#' This function plots densities for conservative effect sizes (Cohen's d or R-squared)
#' for a given dataset. It allows optional grouping, visualization, and file saving.
#'
#' @param plot_data_list A list of lists containing effect size data for plotting:
#'  - plot_data_list[i] = `plot_data`: contains:
#'    - `data`: A list containing sorted & downsampled effect size data & helpers: estimate, cons_estimate, lb, ub, below_cross_idx, and above_cross_idx
#'    - `extra_study_details`: A list containing extra descriptive info: percent_not_zero, max_cons_effect, group_by_title, n_title, mv_estimate, and mv_ci
#'    - `study_details`: A list of original study details: orig_stat_type, test_component_1, test_component_2, dataset, map_type, group, and ref
#' @param add_description Logical; whether to add a description to the plot. Default is `FALSE`.
#' @param save Logical; whether to save the plot as a PNG file. Default is `FALSE`.
#' @param out_path A string specifying the output directory for saving plots. Default is "output".
#' @param file_name A string for the saved file name. Default is "plot".
#'
#' @return A plot visualizing effect sizes and simulated CIs.
#' @export
#'
#' @examples
#' # Example usage
#' # plot_density(pd)
plot_density <- function(plot_data_list, add_description = FALSE, save = FALSE, out_path = 'output', file_name = 'plot') {
  
  library(ggplot2)
  
  # plot params
  pp <- list()
  pp$width <- 6
  pp$height <- 5
  pp$res <- 300
  pp$units <- "in"
  pp$xlim <- c(-0.15, 0.15)
  pp$alpha <- 0.1
  pp$size <- 0.4
  pp$colors__sample_size <- data.frame(labels = c("<1,000", "1,000-5,000", "5,000-10,000", ">10,000"), colors = c("#82A651", "#3AB7BE", "#E7786C", "#B873F7"), breaks_upper_lim = c(1000, 5000, 10000, Inf))
  if (add_description) {   # if add description: set up with bigger margins
    pp$mar <- c(7, 7, 7, 10)
  } else {
    pp$mar <- c(3, 4, 5, 2)
  }
  
  # setup
  if (save) {
    out_name = paste0(out_path, '/', file_name)
    png(out_name, width = pp$width, height = pp$height, res = pp$res, units = pp$units)
  }
  
  # if it's a single study, nest it into a list so we can use the below loop
  if (!"data" %in% names(plot_data_list[[1]])) {
    plot_data_list <- list(plot_data_list)
    # TODO: can record here that it was passed as a single study
  }

  # make plot
  par(mar=pp$mar)
  p <- ggplot() + theme_minimal() + labs(x = "Effect Size", y = "Density") + xlim(pp$xlim)

  for (i in seq_along(plot_data_list)) {
    
    # create data for plot with effect size and binned sample size
    sample_size_category <- cut(as.numeric(gsub("n = ", "", plot_data_list[[i]]$extra_study_details$n_title)), 
                                breaks = c(-Inf, pp$colors__sample_size$breaks_upper_lim), 
                                labels = pp$colors__sample_size$labels)

    p <- p + geom_density(data = data.frame(value = plot_data_list[[i]]$data$cons_estimate, sample_size_category = sample_size_category),
                          aes(x = value, fill = sample_size_category, color = sample_size_category), alpha = pp$alpha, size = pp$size) # for unique color per study, do: fill = i, color = i
  }

  p <- p + theme(legend.position = "none") +
    scale_fill_manual(values = pp$colors__sample_size$colors, breaks = pp$colors__sample_size$labels) + 
    scale_color_manual(values = pp$colors__sample_size$colors, breaks = pp$colors__sample_size$labels)
  
  # if add description: add extra info
  # TODO: this shouldn't be the responsibility of plot_density...
  if (add_description) {
    if (length(plot_data_list) > 1) {

      # create study details for the multi-plot
      study_details_multi <- list()
      extra_study_details_multi <- list()
      study_details_multi$group <- "Multi"
      study_details_multi$ref <- "TBD"
      extra_study_details_multi$group_by_title == 'Multi' # TODO: temporary solution for triggering "group_by"
      extra_study_details_multi$group_by_title <- "Multi"
      
      # append all max effects and percent nonzeros
      max_cons_estimate__multi <- rep(0, length(plot_data_list))
      percent_not_zero__multi <- rep(0, length(plot_data_list))
      n_variables <- rep(0, length(plot_data_list))
      cons_mv_estimate__multi <- rep(0, length(plot_data_list))
      for (i in seq_along(plot_data_list)) {
        max_cons_estimate__multi[i] <- plot_data_list[[i]]$extra_study_details$max_cons_estimate
        percent_not_zero__multi[i] <- plot_data_list[[i]]$extra_study_details$percent_not_zero
        n_variables[i] <- length(plot_data_list[[i]]$data$cons_estimate)
        cons_mv_estimate__multi[i] <- plot_data_list[[i]]$extra_study_details$mv_ci[1]
      }
      
      # take max of all max effects and average across all percent nonzeros
      extra_study_details_multi$max_cons_effect <- max_cons_estimate__multi[which.max(abs(max_cons_estimate__multi))]
      extra_study_details_multi$percent_not_zero <- sum(percent_not_zero__multi*n_variables)/sum(n_variables)
      extra_study_details_multi$max_cons_mv_estimate <- max(cons_mv_estimate__multi)
      
      p <- add_plot_description(p, study_details_multi, extra_study_details_multi)
      
    } else {
      p <- add_plot_description(p, plot_data_list[[1]]$study_details, plot_data_list[[1]]$extra_study_details)
    }
  }
  
  print(p)
  
  if (save) {
    dev.off()
  }
}
