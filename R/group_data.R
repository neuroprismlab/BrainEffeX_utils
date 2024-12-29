#' Group Studies by Factor
#'
#' This function plots effect sizes (Cohen's d or R-squared) and simulated confidence intervals (CIs) 
#' for a given dataset. It allows optional grouping, visualization, and file saving.
#'
#' @param v A list containing effect size data
#' @param brain_masks A list containing the brain masks
#' @param combo_name A string specifying the combo to plot - # TODO: note: in app, this is saved directly in v
#' @param group_by A string to specify grouping: "orig_stat_type" or "category"
#'
#' @return An updated list with the grouped_by data
#' @export
#'
#' @examples
#' # Example usage
#' # group_by(v,v$brain_masks, "pooling.none.motion.none.mv.none")
group_data <- function(v, brain_masks, combo_name, group_by = "category") {
  
  # initialize a list to store the data for each stat type and ref type
  v$d_group <- list()
  
  # initialize a new study dataframe to store the info for the groupings
  v$study_group <- data.frame(group = character(0), ref = character(0), name = character(0))
  
  
  # variable of grouping
  if (group_by == "orig_stat_type") {
    group_var <- "orig_stat_type"
  } else if (group_by == "category") {
    group_var <- "category"
  }
  
  # for each statistic type
  for (level in unique(v$study[[group_var]])) {
    # for each reference type
    for (ref in unique(v$study$ref)) {
      matching_idx <- which(v$study[[group_var]] == level & v$study$ref == ref)
      #print(paste0("length of matching index: ", length(matching_idx)))
      if (length(matching_idx) > 0) {
        matching_names <- v$study$name[matching_idx]
        matching_d_idx <- which(toupper(names(v$data)) %in% toupper(matching_names))
        # matching_d_idx is the idx of the studies in d that match the current stat and ref
        # average across all studies in matching_d_idx
        # initialize an empty vector to store the sum across studies
        if (v$study$map_type[matching_idx[1]] == "act") {
          # if an activation study, create empty total vectors from mask instead of maps
          d_total <- rep(0, length(c(brain_masks[[v$study$name[matching_idx[1]]]]$mask))) # initialize to the size of the largest matrix
          ci_lb_total <- rep(0, length(c(brain_masks[[v$study$name[matching_idx[1]]]]$mask))) # TODO: for now just average across CIs, but ask Steph how we should do this!!!
          ci_ub_total <- rep(0, length(c(brain_masks[[v$study$name[matching_idx[1]]]]$mask)))
        } else {
          d_total <- rep(0, length(v$data[[matching_d_idx[1]]][[combo_name]]$d)) # initialize to the size of the largest matrix
          ci_lb_total <- rep(0, length(v$data[[matching_d_idx[1]]][[combo_name]]$d)) # TODO: for now just average across CIs, but ask Steph how we should do this!!!
          ci_ub_total <- rep(0, length(v$data[[matching_d_idx[1]]][[combo_name]]$d))
        }
        
        for (i in matching_d_idx) {
          # if an activation study, then we need to first use the study's mask to fill in the values in the 
          # appropriate spots in the effect size matrix
          this_d <- v$data[[i]][[combo_name]]$d
          this_ci_lb <- v$data[[i]][[combo_name]]$sim_ci_lb
          this_ci_ub <- v$data[[i]][[combo_name]]$sim_ci_ub
          
          if (is.list(this_ci_lb)) { # unlist confidence intervals if list
            this_ci_lb <- unlist(this_ci_lb)
            this_ci_ub <- unlist(this_ci_ub)
          }
          
          if (v$study$map_type[i] == "act") {
            # get the mask for this study
            d_mask <- brain_masks[[v$study$name[i]]]$mask
            ci_lb_mask <- brain_masks[[v$study$name[i]]]$mask
            ci_ub_mask <- brain_masks[[v$study$name[i]]]$mask
            # fill in the values in the effect size matrix
            d_mask[d_mask == 1] <- this_d
            ci_lb_mask[ci_lb_mask == 1] <- this_ci_lb
            ci_ub_mask[ci_ub_mask == 1] <- this_ci_ub
            # add to total
            d_total <- d_total + c(d_mask)
            ci_lb_total <- ci_lb_total + c(ci_lb_mask)
            ci_ub_total <- ci_ub_total + c(ci_ub_mask)
          } else {
            d_total <- d_total + this_d
            ci_lb_total <- ci_lb_total + this_ci_lb
            ci_ub_total <- ci_ub_total + this_ci_ub
          }
        }
        d_avg <- d_total / length(matching_d_idx)
        ci_lb_avg <- ci_lb_total / length(matching_d_idx)
        ci_ub_avg <- ci_ub_total / length(matching_d_idx)
        
        # if activation map, remove values from indices that are zero in d_total, ci_lb_total, and ci_ub_total
        if (v$study$map_type[matching_idx[1]] == "act") {
          zero_idx <- which((d_total == 0) & (ci_lb_total == 0) & (ci_ub_total == 0))
          d_avg <- d_avg[-zero_idx]
          ci_lb_avg <- ci_lb_avg[-zero_idx]
          ci_ub_avg <- ci_ub_avg[-zero_idx]
        }
        
        # store d_avg, ci_lb_avg, and ci_ub_avg in d_group list as a list
        v$d_group[[paste0(group_var, "_", level, "_reference_", ref)]][[combo_name]]$d <- d_avg
        v$d_group[[paste0(group_var, "_", level, "_reference_", ref)]][[combo_name]]$sim_ci_lb <- ci_lb_avg
        v$d_group[[paste0(group_var, "_", level, "_reference_", ref)]][[combo_name]]$sim_ci_ub <- ci_ub_avg
        
        # store the study info in the study_stat dataframe
        v$study_group <- rbind(v$study_group, data.frame(group = level, ref = ref, name = paste0(group_var, "_", level, "_reference_", ref)))
        
      }
    }
  }
  
  return(v)
  
  
}
