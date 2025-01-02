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
  
  if (extra_study_details$group_by_title == 'None') {
    
    title_text <- paste0("Dataset: ", study_details$dataset, "    |    ",
                         "Test: ", study_details$orig_stat_type, ": ", study_details$test_component_1, ", ", study_details$test_component_2, "    |    ",
                         "Map: ", study_details$map_type, "    |    ",
                         "Sample Size: ", extra_study_details$n_title)
    
    bottom_text <- paste0("Max conservative effect size: ", extra_study_details$max_cons_effect, "\n",
                          "Percent not overlapping zero: ", round(extra_study_details$percent_not_zero * 100, 1), "%\n",
                          "Multivariate effect size: ", round(extra_study_details$mv_estimate, 2), " [", round(extra_study_details$mv_ci[1], 2), ", ", round(extra_study_details$mv_ci[2], 2), "]")
    
    p <- p + 
      ggtitle(title_text) +
      labs(caption = bottom_text) +
      theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
            plot.caption = element_text(hjust = 0, size = 8))
    
  } else {
    
    title_text <- paste0(extra_study_details$group_by_title, ": ", study_details$group, "    |    ",
                         "Reference Space: ", study_details$ref)
    
    # if field cons_mv_estimate exists in extra_study_details_multi, add to bottom text # TODO: currently not defined when using group_data
    if ("max_cons_mv_estimate" %in% names(extra_study_details)) {
      bottom_text <- paste0("Max conservative effect size: ", extra_study_details$max_cons_effect, "\n",
                            "Percent not overlapping zero: ", round(extra_study_details$percent_not_zero * 100, 1), "%\n",
                            "Max conservative multivariate effect size: ", round(extra_study_details$max_cons_mv_estimate, 2))
    } else {
      bottom_text <- paste0("Max conservative effect size: ", extra_study_details$max_cons_effect, "\n",
                            "Percent not overlapping zero: ", round(extra_study_details$percent_not_zero * 100, 1), "%")
    }
    
    p <- p + 
      ggtitle(title_text) +
      labs(caption = bottom_text) +
      theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
            plot.caption = element_text(hjust = 0, size = 8))
  }
  
  return(p)
}