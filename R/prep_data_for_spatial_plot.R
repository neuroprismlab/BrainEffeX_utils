#' Prepare Effect Size Data for Plot
#'
#' This function prepares a given study for effect size plots.
#'
#' @param data A list containing effect size data (e.g., `v$d_clean[[i]]`).
#' @param study_details A list of study details (e.g., `v$study[i, ]`).
#' @param combo_name A string specifying the combo to plot.
#' @param mv_combo_name A string specifying the multivariate combo to plot.
#' @param estimate A string to specify the effect size estimate: "d" or "r_sq"
#' @param plot_info A list containing extra plot information (group_var, 
#' level, and reference atlas)
#' @param brain_masks A list containing brain mask information
#'
#' @return A plot visualizing effect sizes and simulated CIs.
#' @export
#'
#' @examples
#' # Example usage
#' \dontrun{
#' pd <- prep_data_for_spatial_plot(data = v$data[[i]], 
#' brain_masks = brain_masks,
#' study_details = v$study[i, ],
#' combo_name = "pooling.none.motion.none.mv.none", 
#' mv_combo_name = "pooling.none.motion.none.mv.multi")
#' }
prep_data_for_spatial_plot <- function(data, brain_masks, study_details, combo_name, mv_combo_name, estimate = 'd', plot_info = 'NA') {

  # Input validation
  if (!combo_name %in% names(data)) {
    stop("combo_name '", combo_name, "' not found in data")
  }
  
  if (!estimate %in% names(data[[combo_name]])) {
    stop("estimate '", estimate, "' not found in data for combo '", combo_name, "'")
  }
  
  if (!is.list(plot_info)) {
    stop("plot_info must be a list containing grouping_var, group_level, and ref")
  }
  
  # 1. Get data

  # unlist if list
  if (is.list(data[[combo_name]][[estimate]])) {
    data[[combo_name]][[estimate]] <- unlist(data[[combo_name]][[estimate]])
  }


  # 2. Return ready-to-plot structure

  # includes less data than for other plot types, except also adds mask

  plot_data <- list(
    data = list(
      estimate = data[[combo_name]][[estimate]]
    ),
    extra_study_details = list(
      n_title = paste0("n = ", data[[combo_name]]$n), # TODO: this should not be defined if group_type != "none",
      grouping_var = plot_info$grouping_var,
      group_level = plot_info$group_level,
      ref = plot_info$ref,
      brain_masks = brain_masks
    ),
    study_details = study_details
  )

  return(plot_data)


}
