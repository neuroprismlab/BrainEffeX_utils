#########################################################################
# Functions for plotting
# These functions describe different effect sizes (Cohen's d or R-squared) and simultaneous confidence intervals (CIs)
# for a given dataset. It allows optional grouping, visualization, and file saving.

#########################################################################
# Density & Simultaneous CIs

#' Plot Simultaneous CIs
#'
#' This function creates a plot showing simultaneous confidence intervals for effect sizes.
#'
#' @param pp A list of plot parameters
#' @param plot_data_list A list containing effect size data for plotting with the following structure:
#'  \itemize{
#'    \item \code{data}: A list containing sorted & downsampled effect size data & helpers: estimate, cons_estimate, lb, ub, below_cross_idx, and above_cross_idx
#'    \item \code{summary_info}: A list containing extra descriptive info: percent_not_zero, max_cons_effect, group_by_title, and n_title
#'    \item \code{mv_data}: A list containing multivariate effect size data: estimate, lb, and ub
#'    \item \code{study_details}: A list of original study details: orig_stat_type, test_component_1, test_component_2, dataset, map_type, group, and ref
#'  }
#'
#' @return A ggplot object containing one or overlapping densities
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' plot_simci_panel(pp, plot_data)
#' }
#' 
#' @import ggplot2
#' @import metafor
#' @importFrom grDevices dev.off png
#' @importFrom graphics axis box image par
#' @importFrom utils read.csv
#' @importFrom reshape2 melt
#' @importFrom oro.nifti readNIfTI writeNIfTI
#' @importFrom neurobase ortho2
#' @importFrom grid rasterGrob
#' @importFrom pwr pwr.t.test
#' @importFrom ggbeeswarm geom_quasirandom
#' @importFrom colorspace diverge_hsv
#' @importFrom png readPNG
plot_simci_panel <- function(pp, plot_data_list) {
  # Function body remains the same...
  if (length(plot_data_list) == 0) {
    stop("plot_data_list cannot be empty")
  }
  
  # add simci-specific plot params
  pp$non_overlap_colors <- "#4ECDC4"
  pp$overlap_colors <- "#FF6F61"
  pp$other_overlap_colors <- "#8B859E" 
  pp$alpha_ribbon <- 0.3
  pp$alpha_line <- 1
  pp$intercept_line_color <- "#ba2d25"
  pp$intercept_line_size <- 0.3
  pp$xlabel <- "Edges / Voxels, sorted by effect size"
  pp$ylabel <- "Effect Size"
  
  # little function to simplify plotting
  add_geom_layers <- function(p, data, color, alpha_line, alpha_ribbon) {
    if (nrow(data) > 1) {
      p <- p +
        geom_line(data = data, aes(x = x, y = estimate), color = color, alpha = alpha_line) +
        geom_ribbon(data = data, aes(x = x, ymin = lb, ymax = ub), fill = color, alpha = alpha_ribbon)
    }
    return(p)
  }
  
  # make plot object
  p <- ggplot() + geom_hline(yintercept = 0, color = pp$intercept_line_color, linetype = "dashed", size = pp$intercept_line_size)
  
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
    pp$ylim = pp$effect_size_limits_big
    
    # plot
    if ((length(below_cross_idx) > 1) || (length(above_cross_idx) > 1)) {
      # IN PROGRESS: plot everything below below_cross_idx[[1]] in grey
      p <- add_geom_layers(p, plot_df, pp$other_overlap_colors, pp$alpha_line, pp$alpha_ribbon)
    } else {
      p <- add_geom_layers(p, subset(plot_df, x <= below_cross_idx), pp$non_overlap_colors, pp$alpha_line, pp$alpha_ribbon)
      p <- add_geom_layers(p, subset(plot_df, x >= below_cross_idx & x <= above_cross_idx), pp$overlap_colors, pp$alpha_line, pp$alpha_ribbon)
      p <- add_geom_layers(p, subset(plot_df, x >= above_cross_idx), pp$non_overlap_colors, pp$alpha_line, pp$alpha_ribbon)
    }
  }
  
  p <- p + labs(x = pp$xlabel, y = pp$ylabel) +
    coord_cartesian(ylim = pp$ylim) +
    theme_classic() +
    theme(axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.text.y = pp$axis_text_size, axis.title = pp$axis_title_size)
  
  return(p)
}


#' Plot Densities
#'
#' This function creates density plots for effect size data.
#'
#' @param pp A list of plot parameters
#' @param plot_data_list A list containing effect size data for plotting with the following structure:
#'  \itemize{
#'    \item \code{data}: A list containing sorted & downsampled effect size data & helpers: estimate, cons_estimate, lb, ub, below_cross_idx, and above_cross_idx
#'    \item \code{summary_info}: A list containing extra descriptive info: percent_not_zero, max_cons_effect, group_by_title, and n_title
#'    \item \code{mv_data}: A list containing multivariate effect size data: estimate, lb, and ub
#'    \item \code{study_details}: A list of original study details: orig_stat_type, test_component_1, test_component_2, dataset, map_type, group, and ref
#'  }
#' @param use_effect_size_bin Logical indicating whether to bin effect sizes (default: FALSE)
#'
#' @return A ggplot object containing one or overlapping densities
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' plot_density_panel(pp, plot_data)
#' }
plot_density_panel <- function(pp, plot_data_list, use_effect_size_bin = FALSE) {
  # Function body remains the same...
  # [Keep all the existing function code]
}


#' Plot Voxel-level Activation Maps
#'
#' This function creates spatial activation maps for voxel-level data.
#'
#' @param pp A list of plot parameters
#' @param plot_data_list A list containing effect size data for plotting with the following structure:
#'  \itemize{
#'    \item \code{data}: A list containing sorted & downsampled effect size data & helpers: estimate, cons_estimate, lb, ub, below_cross_idx, and above_cross_idx
#'    \item \code{summary_info}: A list containing extra descriptive info: percent_not_zero, max_cons_effect, group_by_title, and n_title
#'    \item \code{mv_data}: A list containing multivariate effect size data: estimate, lb, and ub
#'    \item \code{study_details}: A list of original study details: orig_stat_type, test_component_1, test_component_2, dataset, map_type, group, and ref
#'  }
#' @param threshold_category Optional threshold category for data filtering (default: NA)
#'
#' @return A ggplot object containing spatial activation maps
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' plot_activation_panel(pp, plot_data)
#' }
plot_activation_panel <- function(pp, plot_data_list, threshold_category = NA) {
  # Function body remains the same...
  # [Keep all the existing function code]
}


#' Plot Connectivity Maps
#'
#' This function creates functional connectivity matrix plots.
#'
#' @param pp A list of plot parameters
#' @param plot_data_list A list containing effect size data for plotting with the following structure:
#'  \itemize{
#'    \item \code{data}: A list containing sorted & downsampled effect size data & helpers: estimate, cons_estimate, lb, ub, below_cross_idx, and above_cross_idx
#'    \item \code{summary_info}: A list containing extra descriptive info: percent_not_zero, max_cons_effect, group_by_title, and n_title
#'    \item \code{mv_data}: A list containing multivariate effect size data: estimate, lb, and ub
#'    \item \code{study_details}: A list of original study details: orig_stat_type, test_component_1, test_component_2, dataset, map_type, group, and ref
#'  }
#' @param threshold_category Optional threshold category for data filtering (default: NA)
#'
#' @return A ggplot object containing connectivity matrix plots
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' plot_connectivity_panel(pp, plot_data)
#' }
plot_connectivity_panel <- function(pp, plot_data_list, threshold_category = NA) {
  # Function body remains the same...
  # [Keep all the existing function code]
}


#' Plot Full Matrix
#'
#' This function creates a full connectivity matrix plot from triangle-ordered data.
#'
#' @param pp A list of plot parameters
#' @param triangle_ordered A vector of triangle-ordered connectivity values
#' @param ukb Logical indicating if using UKB data (default: FALSE)
#' @param mapping_path Path to the mapping file for node labels (default: NA)
#'
#' @return A ggplot object containing the full matrix heatmap
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' plot_full_mat(pp, triangle_data, mapping_path = "path/to/mapping.csv")
#' }
plot_full_mat <- function(pp, triangle_ordered, ukb = FALSE, mapping_path = NA) {
  # Function body remains the same...
  # [Keep all the existing function code]
}


#' Plot Power Analysis
#'
#' This function creates power analysis plots showing statistical power or required sample sizes.
#'
#' @param pp A list of plot parameters
#' @param plot_data_list A list containing effect size data for plotting
#' @param output_type Character string specifying output type ("power" or "sample_size")
#' @param use_category_bins Logical indicating whether to use categorical bins (default: FALSE)
#' @param do_spatial_plot Logical indicating whether to create spatial plots (default: FALSE)
#'
#' @return A ggplot object or list containing power analysis plots
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' plot_power_panel(pp, plot_data, "power")
#' }
plot_power_panel <- function(pp, plot_data_list, output_type, use_category_bins = FALSE, do_spatial_plot = FALSE) {
  # Function body remains the same...
  # [Keep all the existing function code]
}


#' Get Summary Information
#'
#' This function extracts and formats summary information from study details.
#'
#' @param study_details A list of original study details including orig_stat_type, test_component_1, test_component_2, dataset, map_type, group, and ref
#' @param extra_study_details A list containing extra descriptive info including percent_not_zero, max_cons_effect, group_by_title, n_title, mv_estimate, and mv_ci
#'
#' @return A list containing formatted summary information including title_text and bottom_text
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' summary <- get_summary_info(study_details, extra_study_details)
#' }
get_summary_info <- function(study_details, extra_study_details) {
  # Function body remains the same...
  # [Keep all the existing function code]
}


#' Combine Summary Information
#'
#' This function combines summary information from multiple studies for overlapping plots.
#'
#' @param summary_info A list of summary information objects to be combined
#'
#' @return A combined summary information object
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' combined_summary <- combine_summary_info(summary_list)
#' }
combine_summary_info <- function(summary_info) {
  # Function body remains the same...
  # [Keep all the existing function code]
}


#' Add Plot Description
#'
#' This function adds descriptive labels and titles to ggplot objects.
#'
#' @param p A ggplot object to which the labels will be added
#' @param pp A list of plot parameters
#' @param summary_info A list containing summary information with title_text and bottom_text
#' @param add_extra_text Logical indicating whether to add extra descriptive text
#' @param do_minimal_title Logical indicating whether to use minimal title formatting
#'
#' @return A ggplot object with added descriptive labels and titles
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' p_with_labels <- add_plot_description(p, pp, summary_info, TRUE, FALSE)
#' }
add_plot_description <- function(p, pp, summary_info, add_extra_text, do_minimal_title) {
  # Function body remains the same...
  # [Keep all the existing function code]
}


#' Create NIfTI Template
#'
#' This function creates a NIfTI template file from a sample NIfTI file.
#'
#' @param sample_nifti_path Path to the sample NIfTI file (default: 'data/EMOTION_cope3_GroupSize482_dcoeff.nii.gz')
#' @param out_path Output directory path (default: 'data/')
#'
#' @return NULL (saves template file to disk)
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' create_nifti_template("path/to/sample.nii.gz", "output/")
#' }
create_nifti_template <- function(sample_nifti_path = 'data/EMOTION_cope3_GroupSize482_dcoeff.nii.gz', out_path = 'data/') {
  # Function body remains the same...
  # [Keep all the existing function code]
}


#' Create NIfTI File
#'
#' This function creates a NIfTI file from a template, data, and mask.
#'
#' @param nifti_template A loaded NIfTI template file containing metadata
#' @param data A 1D vector of data values extracted from the mask
#' @param mask An nD mask array used to extract the original data
#'
#' @return A NIfTI object with data inserted back into the mask structure
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' nii <- create_nifti(template, effect_data, brain_mask)
#' }
create_nifti <- function(nifti_template, data, mask) {
  # Function body remains the same...
  # [Keep all the existing function code]
}


#' Custom Colorbar
#'
#' This function creates a custom colorbar for neuroimaging plots.
#'
#' @param breaks Numeric vector of break points for the colorbar
#' @param col Vector of colors for the colorbar segments
#' @param text.col Color for axis and text labels (default: "white")
#' @param labels Logical or vector specifying colorbar labels (default: TRUE)
#' @param maxleft Maximum left position for the colorbar (default: 0.95)
#' @param text.size Size of text labels (default: 4)
#'
#' @return NULL (creates colorbar as side effect)
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' colorbar_custom(breaks = seq(-1, 1, 0.1), col = rainbow(20))
#' }
colorbar_custom <- function(breaks, col, text.col = "white", labels = TRUE, maxleft = 0.95, text.size = 4) {
  # Function body remains the same...
  # [Keep all the existing function code]
}