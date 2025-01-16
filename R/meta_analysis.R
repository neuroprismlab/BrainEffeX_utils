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
meta_analysis <- function(v, brain_masks, combo_name, group_by = "category") {
  
  library(metafor)
  
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
      
      matching_idx <- which(v$study[[group_var]] == level & v$study$ref == ref) # TODO: maybe explicitly add map type here
      #print(paste0("length of matching index: ", length(matching_idx)))
      
      if (length(matching_idx) > 0) {
        matching_names <- v$study$name[matching_idx]
        matching_d_idx <- which(toupper(names(v$data)) %in% toupper(matching_names))
        # matching_d_idx is the idx of the studies in d that match the current stat and ref
        
        # get n variables per study
        # for activation studies, get intersection mask
        if (v$study$map_type[matching_idx[1]] == "act") { # get intersection of all masks
          intersection_mask <- brain_masks[[v$study$name[matching_idx[1]]]]$mask
          for (i in matching_d_idx) {
            intersection_mask <- intersection_mask & brain_masks[[v$study$name[i]]]$mask
          }
          # n_var = sum(intersection_mask)
        } else {
          # n_var = length(v$data[[matching_d_idx[1]]][[combo_name]]$d)
        }
        
        # initialize an empty vector to store values across studies
        
        effect_sizes <- c()
        sample_sizes <- c()
        
        for (i in matching_d_idx) {
          
          # if an activation study, then we need to first use the study's mask to fill in the values in the 
          # appropriate spots in the effect size matrix
          this_d <- v$data[[i]][[combo_name]]$d # TODO: for r2, $d -> $R2
          this_n <- v$data[[i]][[combo_name]]$n
          
          if (v$study$map_type[i] == "act") {
            
            # make vector for indexing that combines d_mask and intersection_mask
            d_mask <- brain_masks[[v$study$name[i]]]$mask
            mask_of_masks <- intersection_mask[d_mask == 1]
            
            this_d <- this_d[mask_of_masks]
            
          }
          
          # append to total
          effect_sizes <- c(effect_sizes, this_d)
          sample_sizes <- c(sample_sizes, this_n)
          
        }
        
        
        # Meta analysis
        
        variances <- (4 / sample_sizes) + (effect_sizes^2 / (2 * sample_sizes)) # TODO: check for 1- and 2-sample
        
        # TODO: For R2, convert to z and different variance calc
        # z_effect_sizes <- atanh(sqrt(effect_sizes))
        # variances <- 1 / (sample_sizes - 3)
        
        meta_analysis <- rma.uni(yi = effect_sizes, vi = variances, method = "REML")
        estimate_meta <- meta_analysis$b
        ci_lb_meta <- meta_analysis$ci.lb # TODO: for R2, here and next line $ci.lb -> $ci.lb-R2 (or equiv)
        ci_ub_meta <- meta_analysis$ci.ub
        
        
        
        # TODO: For R2, convert back to r
        # estimate_meta <- tanh(estimate_meta)
        # ci_lb_meta <- tanh(ci_lb_meta)
        # ci_ub_meta <- tanh(ci_ub_meta)
        
        # if activation map, remove values from indices that are zero in d_total, ci_lb_total, and ci_ub_total
        if (v$study$map_type[matching_idx[1]] == "act") {
          zero_idx <- which((estimate_meta == 0) & (ci_lb_meta == 0) & (ci_ub_meta == 0))
          estimate_meta <- estimate_meta[-zero_idx]
          ci_lb_meta <- ci_lb_meta[-zero_idx]
          ci_ub_meta <- ci_ub_meta[-zero_idx]
        }
        
        # store d_avg, ci_lb_avg, and ci_ub_avg in d_group list as a list
        v$d_group[[paste0(group_var, "_", level, "_reference_", ref)]][[combo_name]]$d <- estimate_meta # TODO: for r2, $d -> $R2
        v$d_group[[paste0(group_var, "_", level, "_reference_", ref)]][[combo_name]]$sim_ci_lb <- ci_lb_meta  # TODO: for R2, here and next line $sim_ci_lb -> $sim_ci_lb-R2 (or equiv)
        v$d_group[[paste0(group_var, "_", level, "_reference_", ref)]][[combo_name]]$sim_ci_ub <- ci_ub_meta
        
        # store the study info in the study_stat dataframe
        v$study_group <- rbind(v$study_group, data.frame(group = level, ref = ref, name = paste0(group_var, "_", level, "_reference_", ref)))
        
      }
    }
  }
  
  return(v)
  
  
}
