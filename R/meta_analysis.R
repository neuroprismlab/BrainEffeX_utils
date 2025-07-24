#' Group Studies by Factor
#'
#' This function plots effect sizes (Cohen's d or R-squared) and simulated confidence intervals (CIs)
#' for a given dataset. It allows optional grouping, visualization, and file saving.
#' @import metafor
#' @importFrom metafor rma.mv
#' @param v A list containing effect size data
#' @param brain_masks A list containing the brain masks
#' @param combo_name A string specifying the combo to plot (if NA, processes all combos)
#' @param grouping_var A string to specify grouping: "orig_stat_type" or "category"
#'
#' @return An updated list with the grouped_by data
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' meta_analysis(v, v$brain_masks, "pooling.none.motion.none.mv.none")
#' }
meta_analysis <- function(v, brain_masks, combo_name = NA, grouping_var = "category") {
  
  testing <- FALSE
  
  # if combo_name not specified, create a list of all combo names
  # otherwise create a list of just the given combo name
  if (is.na(combo_name)) {
    combo_names = names(v$data[[1]])
  } else {
    combo_names = c(combo_name)
  }
  
  # helpers
  
  d_se <- function(d, n1, n2 = NULL) {
    if (is.null(n2)) { # one-sample
      se <- sqrt(1 / n1 + (d^2 / (2 * n1)))
    } else { # two-sample
      se <- sqrt((n1 + n2) / (n1 * n2) + (d^2 / (2 * (n1 + n2))))
    }
    return(se)
  }
  
  r_sq_se <- function(r_sq, n) {
    r <- sqrt(r_sq)
    se_r <- sqrt((1 - r^2) / (n - 2));
    se <- se_r^2
    return(se)
  }
  
  # Input validation
  if (!is.list(v)) {
    stop("v must be a list")
  }
  
  if (!"study" %in% names(v)) {
    stop("v must contain a 'study' component")
  }
  
  if (!"data" %in% names(v)) {
    stop("v must contain a 'data' component")
  }
  
  if (!is.data.frame(v$study)) {
    stop("v$study must be a data frame")
  }
  
  if (!is.list(v$data)) {
    stop("v$data must be a list")
  }
  
  if (!grouping_var %in% names(v$study)) {
    stop("grouping_var '", grouping_var, "' not found in v$study columns")
  }
  
  if (length(v$data) == 0) {
    stop("v$data is empty")
  }
  
  # MAIN CHANGE: Add loop for each combo_name
  for (current_combo in combo_names) {
    
    cat("Processing combo:", current_combo, "\n")
    
    # Check if current_combo exists in at least one study
    combo_exists <- any(sapply(v$data, function(study_data) {
      current_combo %in% names(study_data) || 
        any(grepl(current_combo, names(study_data)))
    }))
    
    if (!combo_exists) {
      warning("combo_name '", current_combo, "' not found in any study data, skipping...")
      next
    }
    
    # initialize vars for storing grouping results
    # only create if data_group doesn't exist
    meta_str <- paste0('meta_',grouping_var)
    if (!(meta_str %in% names(v))) {
      v[[meta_str]] <- list()
      v[[meta_str]]$data <- list() # store data for each stat + ref type
      v[[meta_str]]$study <- data.frame(group = character(0), ref = character(0), name = character(0)) # store grouping info
      v[[meta_str]]$brain_masks <- list()
    }
    
    # for each level of the grouping var (e.g., statistic type, category)
    for (level in unique(v$study[[grouping_var]])) {
      
      # for each reference type
      for (ref in unique(v$study$ref)) {
        
        matching_idx__study <- which(v$study[[grouping_var]] == level & v$study$ref == ref)
        
        if (length(matching_idx__study) > 0) {
          
          this_meta_label <- paste0(level, "_reference_", ref) # label to refer to this meta-analysis by name
          
          print(paste0("- ", this_meta_label, " (combo: ", current_combo, ")"))
          
          matching_names <- v$study$name[matching_idx__study]
          matching_idx__data <- which(toupper(names(v$data)) %in% toupper(matching_names))
          
          # get intersection of all masks
          
          if (grepl("net", current_combo) | grepl("mv.multi", current_combo)) {
            
            # Make mask from data
            template_combo_name <- names(v$data[[1]])[grepl(current_combo, names(v$data[[1]]))]
            intersection_mask <- rep(TRUE, length(v$data[[matching_idx__study[1]]][[template_combo_name]]$d))
            if (length(intersection_mask) == 0) {
              intersection_mask <- TRUE
            }
            
          } else {
            
            # Get existing edge- / voxel-level mask
            intersection_mask <- brain_masks[[v$study$name[matching_idx__study[1]]]]$mask
            for (this_study in matching_idx__data) {
              intersection_mask <- intersection_mask & brain_masks[[v$study$name[this_study]]]$mask
            }
          }
          
          # Combine data
          
          # initialize
          d__group <- NULL
          d_se__group <- NULL
          r_sq__group <- NULL
          r_sq_se__group <- NULL
          
          d_sim_ci_lb__group <- NULL
          d_sim_ci_ub__group <- NULL
          r_sq_sim_ci_lb__group <- NULL
          r_sq_sim_ci_ub__group <- NULL
          
          if (length(matching_idx__data) == 1) { # NO META-ANALYSIS:
            
            # get individual study effect size, sample size, & ci's
            
            this_study <- matching_idx__data[1]
            
            # Handle combo name resolution for multi cases
            working_combo_name <- current_combo
            if (grepl("multi", current_combo)) {
              matching_combos <- names(v$data[[this_study]])[grepl(current_combo, names(v$data[[this_study]]))]
              if (length(matching_combos) > 0) {
                working_combo_name <- matching_combos[1]
              }
            }
            
            # Check if the combo exists in this study
            if (!working_combo_name %in% names(v$data[[this_study]])) {
              warning("Combo '", working_combo_name, "' not found in study ", this_study, ", skipping...")
              next
            }
            
            # set up n's for se calc
            this_n_total <- as.numeric(v$data[[this_study]][[working_combo_name]]$n[1])
            if (!is.null(this_n_total) && !is.na(this_n_total)) {
              this_n1 <- this_n_total/2
              this_n2 <- this_n_total/2
              this_n_total <- this_n_total
            } else {
              this_n1 <- as.numeric(v$data[[this_study]][[working_combo_name]]$n1[1])
              this_n2 <- as.numeric(v$data[[this_study]][[working_combo_name]]$n2[1])
              this_n_total <- this_n1 + this_n2
            }
            
            d__group <- as.numeric(v$data[[this_study]][[working_combo_name]]$d)
            d_sim_ci_lb__group <- as.numeric(v$data[[this_study]][[working_combo_name]]$sim_ci_lb)
            d_sim_ci_ub__group <- as.numeric(v$data[[this_study]][[working_combo_name]]$sim_ci_ub)
            d_se__group <- d_se(d__group, n1 = this_n1, n2 = this_n2)
            
            r_sq__group <- as.numeric(v$data[[this_study]][[working_combo_name]]$r_sq)
            r_sq_sim_ci_ub__group <- as.numeric(v$data[[this_study]][[working_combo_name]]$r_sq_sim_ci_lb)
            r_sq_sim_ci_lb__group <- as.numeric(v$data[[this_study]][[working_combo_name]]$r_sq_sim_ci_ub)
            r_sq_se__group <- r_sq_se(r_sq__group, n = this_n_total)
            
            print("- single")
            
          } else { # META-ANALYSIS:
            
            # 1. Get individual study effect sizes + sample sizes
            
            n_vars_intersection <- sum(intersection_mask)
            
            d__all <- matrix(NA, nrow = n_vars_intersection, ncol = length(matching_idx__data))
            d_sim_ci_ub__all <- d__all
            d_sim_ci_lb__all <- d__all
            d_se__all <- d__all
            
            r_sq__all <- d__all
            r_sq_sim_ci_ub__all <- d__all
            r_sq_sim_ci_lb__all <- d__all
            r_sq_se__all <- d__all
            
            # get data
            it <- 1
            for (this_study in matching_idx__data) {
              
              # Handle combo name resolution for multi cases
              working_combo_name <- current_combo
              if (grepl("multi", current_combo)) {
                matching_combos <- names(v$data[[this_study]])[grepl(current_combo, names(v$data[[this_study]]))]
                if (length(matching_combos) > 0) {
                  working_combo_name <- matching_combos[1]
                }
              }
              
              # Check if the combo exists in this study
              if (!working_combo_name %in% names(v$data[[this_study]])) {
                warning("Combo '", working_combo_name, "' not found in study ", this_study, ", skipping...")
                next
              }
              
              # get n's
              this_n_total <- as.numeric(v$data[[this_study]][[working_combo_name]]$n[1])
              if (!is.null(this_n_total) && !is.na(this_n_total)) {
                this_n1 <- this_n_total/2
                this_n2 <- this_n_total/2
                this_n_total <- this_n_total
              } else {
                this_n1 <- as.numeric(v$data[[this_study]][[working_combo_name]]$n1[1])
                this_n2 <- as.numeric(v$data[[this_study]][[working_combo_name]]$n2[1])
                this_n_total <- this_n1 + this_n2
              }
              
              this_d <- as.numeric(v$data[[this_study]][[working_combo_name]]$d)
              this_d_sim_ci_lb <- as.numeric(v$data[[this_study]][[working_combo_name]]$sim_ci_lb)
              this_d_sim_ci_ub <- as.numeric(v$data[[this_study]][[working_combo_name]]$sim_ci_ub)
              
              this_r_sq <- as.numeric(v$data[[this_study]][[working_combo_name]]$r_sq)
              this_r_sq_sim_ci_lb <- as.numeric(v$data[[this_study]][[working_combo_name]]$r_sq_sim_ci_lb)
              this_r_sq_sim_ci_ub <- as.numeric(v$data[[this_study]][[working_combo_name]]$r_sq_sim_ci_ub)
              
              this_d_se <- d_se(this_d, n1 = this_n1, n2 = this_n2)
              this_r_sq_se <- r_sq_se(this_r_sq, n = this_n_total)
              
              # make vector for indexing
              if (grepl("net", current_combo) | grepl("multi", current_combo)) {
                mask_of_masks <- intersection_mask
              } else {
                d_mask <- brain_masks[[v$study$name[this_study]]]$mask
                mask_of_masks <- intersection_mask[d_mask == 1]
              }
              
              this_d <- this_d[mask_of_masks]
              this_d_sim_ci_lb <- this_d_sim_ci_lb[mask_of_masks]
              this_d_sim_ci_ub <- this_d_sim_ci_ub[mask_of_masks]
              
              this_r_sq <- this_r_sq[mask_of_masks]
              this_r_sq_sim_ci_lb <- this_r_sq_sim_ci_lb[mask_of_masks]
              this_r_sq_sim_ci_ub <- this_r_sq_sim_ci_ub[mask_of_masks]
              
              this_d_se <- this_d_se[mask_of_masks]
              this_r_sq_se <- this_r_sq_se[mask_of_masks]
              
              # append to total
              d__all[, it] <- this_d
              d_sim_ci_lb__all[, it] <- this_d_sim_ci_lb
              d_sim_ci_ub__all[, it] <- this_d_sim_ci_ub
              d_se__all[, it] <- this_d_se
              
              r_sq__all[, it] <- this_r_sq
              r_sq_sim_ci_lb__all[, it] <- this_r_sq_sim_ci_lb
              r_sq_sim_ci_ub__all[, it] <- this_r_sq_sim_ci_ub
              r_sq_se__all[, it] <- this_r_sq_se
              
              it <- it + 1
            }
            
            # 2. Meta analysis
            
            # preallocate to store results
            n_vars <- dim(d__all)[1]
            
            d__group <- numeric(n_vars)
            d_sim_ci_lb__group <- d__group
            d_sim_ci_ub__group <- d__group
            
            r_sq__group <- d__group
            r_sq_sim_ci_lb__group <- d__group
            r_sq_sim_ci_ub__group <- d__group
            
            ci_level <- 1 - (0.05 / n_vars) # confidence level for simultaneous CIs
            
            start_time <- Sys.time()
            
            for (this_variable in 1:n_vars) {
              
              if (testing) { # simple mean
                
                d__group[this_variable] <- mean(d__all[this_variable,], na.rm = TRUE)
                d_sim_ci_lb__group[this_variable] <- mean(d_sim_ci_lb__all[this_variable,], na.rm = TRUE)
                d_sim_ci_ub__group[this_variable] <- mean(d_sim_ci_ub__all[this_variable,], na.rm = TRUE)
                
                r_sq__group[this_variable] <- mean(r_sq__all[this_variable,], na.rm = TRUE)
                r_sq_sim_ci_lb__group[this_variable] <- mean(r_sq_sim_ci_lb__all[this_variable,], na.rm = TRUE)
                r_sq_sim_ci_ub__group[this_variable] <- mean(r_sq_sim_ci_ub__all[this_variable,], na.rm = TRUE)
                
              } else { # meta-analysis
                
                optimizers <- c("nlminb", "Nelder-Mead", "BFGS", "bobyqa","nloptr","nlm","hjk")
                
                # For d: do meta only if not empty or NA; otherwise set NA
                if (length(d__all[this_variable,]) == 0 || length(d_se__all[this_variable,]) == 0 || 
                    all(is.na(d__all[this_variable,])) || all(is.na(d_se__all[this_variable,]))) {
                  d__group[this_variable] <- NA
                  d_sim_ci_lb__group[this_variable] <- NA
                  d_sim_ci_ub__group[this_variable] <- NA
                  
                } else { # do meta
                  
                  d_meta_analysis <- NULL
                  
                  # nested by dataset
                  df <- data.frame(
                    yi = d__all[this_variable,], 
                    vi = d_se__all[this_variable,]^2, 
                    dataset = v$study$dataset[matching_idx__study], 
                    name = v$study$name[matching_idx__study]
                  )
                  
                  for (opt in optimizers) {
                    d_meta_analysis <- tryCatch(
                      rma.mv(yi = yi, V = vi, data = df, slab = dataset, level = ci_level, 
                             random = ~1 | dataset / name, control=list(optimizer=opt)),
                      error = function(e) NULL                           
                    )
                    if (!is.null(d_meta_analysis)) {
                      break
                    }
                  }
                  
                  if (!is.null(d_meta_analysis)) {
                    d__group[this_variable] <- d_meta_analysis$b
                    d_sim_ci_lb__group[this_variable] <- d_meta_analysis$ci.lb
                    d_sim_ci_ub__group[this_variable] <- d_meta_analysis$ci.ub
                  } else {
                    d__group[this_variable] <- NA
                    d_sim_ci_lb__group[this_variable] <- NA
                    d_sim_ci_ub__group[this_variable] <- NA
                  }
                }
                
                # For r_sq: similar logic
                if (length(r_sq__all[this_variable,]) == 0 || length(r_sq_se__all[this_variable,]) == 0 || 
                    all(is.na(r_sq__all[this_variable,])) || all(is.na(r_sq_se__all[this_variable,]))) {
                  r_sq__group[this_variable] <- NA
                  r_sq_sim_ci_lb__group[this_variable] <- NA
                  r_sq_sim_ci_ub__group[this_variable] <- NA
                  
                } else {
                  
                  r_sq_meta_analysis <- NULL
                  
                  # nested by dataset
                  df <- data.frame(
                    yi = r_sq__all[this_variable,], 
                    vi = r_sq_se__all[this_variable,]^2, 
                    dataset = v$study$dataset[matching_idx__study], 
                    name = v$study$name[matching_idx__study]
                  )
                  
                  for (opt in optimizers) {
                    r_sq_meta_analysis <- tryCatch(
                      rma.mv(yi = yi, V = vi, data = df, slab = dataset, level = ci_level, 
                             random = ~1 | dataset / name, control=list(optimizer=opt)),
                      error = function(e) NULL                           
                    )
                    if (!is.null(r_sq_meta_analysis)) {
                      break
                    }
                  }
                  
                  if (!is.null(r_sq_meta_analysis)) {
                    r_sq__group[this_variable] <- r_sq_meta_analysis$b
                    r_sq_sim_ci_lb__group[this_variable] <- r_sq_meta_analysis$ci.lb
                    r_sq_sim_ci_ub__group[this_variable] <- r_sq_meta_analysis$ci.ub
                  } else {
                    r_sq__group[this_variable] <- NA
                    r_sq_sim_ci_lb__group[this_variable] <- NA
                    r_sq_sim_ci_ub__group[this_variable] <- NA
                  }
                }
              }
            }
            elapsed_time <- Sys.time() - start_time
            print(elapsed_time)
          }
          
          # remove values from indices that are zero
          zero_idx <- which((d__group == 0) & (d_sim_ci_lb__group == 0) & (d_sim_ci_ub__group == 0))
          r_sq_zero_idx <- which((r_sq__group == 0) & (r_sq_sim_ci_lb__group == 0) & (r_sq_sim_ci_ub__group == 0))
          
          if (length(zero_idx) > 0 && testing) {
            d__group <- d__group[-zero_idx]
            d_sim_ci_lb__group <- d_sim_ci_lb__group[-zero_idx]
            d_sim_ci_ub__group <- d_sim_ci_ub__group[-zero_idx]
          }
          
          if (length(r_sq_zero_idx) > 0 && testing) {
            r_sq__group <- r_sq__group[-r_sq_zero_idx]
            r_sq_sim_ci_lb__group <- r_sq_sim_ci_lb__group[-r_sq_zero_idx]
            r_sq_sim_ci_ub__group <- r_sq_sim_ci_ub__group[-r_sq_zero_idx]
          }
          
          # store results - use current_combo as the key
          v[[meta_str]]$data[[this_meta_label]][[current_combo]]$d <- d__group
          v[[meta_str]]$data[[this_meta_label]][[current_combo]]$r_sq <- r_sq__group
          v[[meta_str]]$data[[this_meta_label]][[current_combo]]$sim_ci_lb <- d_sim_ci_lb__group
          v[[meta_str]]$data[[this_meta_label]][[current_combo]]$sim_ci_ub <- d_sim_ci_ub__group
          v[[meta_str]]$data[[this_meta_label]][[current_combo]]$r_sq_sim_ci_lb <- r_sq_sim_ci_lb__group
          v[[meta_str]]$data[[this_meta_label]][[current_combo]]$r_sq_sim_ci_ub <- r_sq_sim_ci_ub__group
          
          # store the study info (only once per level/ref combination)
          existing_entry <- which(v[[meta_str]]$study$group_level == level & v[[meta_str]]$study$ref == ref)
          if (length(existing_entry) == 0) {
            v[[meta_str]]$study <- rbind(v[[meta_str]]$study, 
                                         data.frame(group_level = level, ref = ref, name = this_meta_label))
          }
          
          # store intersection masks
          v[[meta_str]]$brain_masks[[this_meta_label]][[current_combo]]$mask <- intersection_mask
          v[[meta_str]]$brain_masks[[this_meta_label]][[current_combo]]$mask_type <- "intersection"
        }
      }
    }
  } # End of combo_names loop
  
  return(v)
}