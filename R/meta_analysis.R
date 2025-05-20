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
#' # meta_analysis(v,v$brain_masks, "pooling.none.motion.none.mv.none")
meta_analysis <- function(v, brain_masks, combo_name, grouping_var = "category") {

  testing <- FALSE

  # libraries & functions

  library(metafor)

  # helpers

  which_triangle <- function(mat) {
    if (!is.matrix(mat)) stop("Input must be a matrix")

    is_upper <- all(mat[lower.tri(mat)] == 0)
    is_lower <- all(mat[upper.tri(mat)] == 0)
    # note: we do not care about checking for diagonal (this function does not include diagonal)

    if (is_upper && is_lower) { return("both")
    } else if (is_upper) { return("upper")
    } else if (is_lower) { return("lower")
    } else { return("no_data")
    }
  }

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


  # initialize vars for storing grouping results
  # if data_group doesn't exist, create
  meta_str <- paste0('meta_',grouping_var)
  if (!(meta_str %in% names(v))) {
    v[[meta_str]] <- list()
    v[[meta_str]]$data <- list() # store data for each stat + ref type
    v[[meta_str]]$study <- data.frame(group = character(0), ref = character(0), name = character(0)) # store grouping info
    v[[meta_str]]$brain_masks <- list()
  }

  # combo_name <- names(data)[grepl(combo_name, names(v$data[[1]]))] # because some are "multi" and some "multi.r" - TODO: this isn't great - somehow it changes mv.none to mv.multi

  # for each level of the grouping var (e.g., statistic type, category)
  for (level in unique(v$study[[grouping_var]])) {

    # for each reference type
    for (ref in unique(v$study$ref)) {

      matching_idx__study <- which(v$study[[grouping_var]] == level & v$study$ref == ref) # TODO: maybe explicitly add map type here

      if (length(matching_idx__study) > 0) {

        matching_names <- v$study$name[matching_idx__study]
        matching_idx__data <- which(toupper(names(v$data)) %in% toupper(matching_names))
        # idx of the studies in d that match the current stat and ref

        # get intersection of all masks

        if (grepl("net", combo_name) | grepl("mv.multi", combo_name)) {
          
          # Make mask from data
          template_combo_name <- names(v$data[[1]])[grepl(combo_name, names(v$data[[1]]))] # mv combo_names change
          intersection_mask <- rep(TRUE, length(v$data[[matching_idx__study[1]]][[template_combo_name]]$d))
          if (length(intersection_mask) == 0) {
            intersection_mask <- TRUE
          }
          
        } else {
          
          # Get existing edge- / voxel-level mask
        
          intersection_mask <- brain_masks[[v$study$name[matching_idx__study[1]]]]$mask
          for (this_study in matching_idx__data) {

            if (v$study$map_type[this_study] == "fc") {
  
              # TEMP mask flipper - TODO: we make sure everything is upper tri in calc_gl . Fix the masks accordingly there, then remove this next part
              triangle_type <- which_triangle(brain_masks[[v$study$name[this_study]]]$mask)
              switch(triangle_type,
                     # "upper" = leave as is
                     "lower" = { # transpose
                       warning("Data is upper triangular but mask is lower triangular.")
                       brain_masks[[v$study$name[this_study]]]$mask <- t(brain_masks[[v$study$name[this_study]]]$mask)
                     },
                     "both" = { # remove lower
                       warning("Data should be upper triangular but contains entries on both sides of diagonal.")
                       # v$data[[this_study]][[combo_name]]$d[lower.tri(v$data[[this_study]][[combo_name]]$d)] <- 0
                       brain_masks[[v$study$name[this_study]]]$mask[lower.tri(brain_masks[[v$study$name[this_study]]]$mask)] <- 0
                     },
                     "no_data" = { # remove this data
                       warning("Mask suggests no data exists.")
                       # v$data[[this_study]][[combo_name]]$d <- NULL
                       # v$study$name[this_study]]]$mask[lower.tri(brain_masks[[v$study$name[this_study]]]$mask <- NULL
                     }
                    # TODO: check if there are too few entries in mask (e.g., <75%)
                )
            }
            
            intersection_mask <- intersection_mask & brain_masks[[v$study$name[this_study]]]$mask
          }
        }


        # Combine data

        # TODO: should actually save all results so data_group fields mirror data_group (e.g., v$data_group$d, v$data_group$r_sq, etc)

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
          
          if (grepl("multi",combo_name)) {
            combo_name_orig <- combo_name
            combo_name <- names(v$data[[this_study]])[grepl(combo_name_orig, names(v$data[[this_study]]))] # because some are "multi" and some "multi.r" - TODO: check - previous version somehow changed mv.none to mv.multi - check
          }

          # set up n's for se calc

          this_n_total <- as.numeric(v$data[[this_study]][[combo_name]]$n[1])
          if (!is.null(this_n_total)) { # correlation, so undefined
            this_n1 <- this_n_total/2 # TODO: check
            this_n2 <- this_n_total/2
            this_n_total <- this_n_total
          } else {
            this_n1 <- as.numeric(v$data[[this_study]][[combo_name]]$n1[1])
            this_n2 <- as.numeric(v$data[[this_study]][[combo_name]]$n2[1])
            this_n_total <- this_n1 + this_n2
          }

          d__group <- as.numeric(v$data[[this_study]][[combo_name]]$d) # TODO: for r2, $d -> $R2
          d_sim_ci_lb__group <- as.numeric(v$data[[this_study]][[combo_name]]$sim_ci_lb)  # TODO: remove all references to CI if we end up using se's
          d_sim_ci_ub__group <- as.numeric(v$data[[this_study]][[combo_name]]$sim_ci_ub)
          d_se__group <- d_se(d__group, n1 = this_n1, n2 = this_n2)

          r_sq__group <- as.numeric(v$data[[this_study]][[combo_name]]$r_sq) # TODO: for r2, $d -> $R2
          r_sq_sim_ci_ub__group <- as.numeric(v$data[[this_study]][[combo_name]]$r_sq_sim_ci_lb)
          r_sq_sim_ci_lb__group <- as.numeric(v$data[[this_study]][[combo_name]]$r_sq_sim_ci_ub)
          r_sq_se__group <- r_sq_se(r_sq__group, n = this_n_total)

          print("- single")

        } else { # META-ANALYSIS:

          # 1. Get individual study effect sizes + sample sizes

          # preallocate to store data across studies

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

            if (grepl("multi", combo_name)) {
              combo_name <- names(v$data[[this_study]])[grepl(combo_name_orig, names(v$data[[this_study]]))] # because some are "multi" and some "multi.r" - TODO: check - previous version somehow changed mv.none to mv.multi - check
            }
            
            # get n's

            this_n_total <- as.numeric(v$data[[this_study]][[combo_name]]$n[1])
            if (!is.null(this_n_total)) { # correlation, so undefined
              this_n1 <- this_n_total/2
              this_n2 <- this_n_total/2
              this_n_total <- this_n_total
            } else {
              this_n1 <- as.numeric(v$data[[this_study]][[combo_name]]$n1[1])
              this_n2 <- as.numeric(v$data[[this_study]][[combo_name]]$n2[1])
              this_n_total <- this_n1 + this_n2
            }

            this_d <- as.numeric(v$data[[this_study]][[combo_name]]$d)
            this_d_sim_ci_lb <- as.numeric(v$data[[this_study]][[combo_name]]$sim_ci_lb)
            this_d_sim_ci_ub <- as.numeric(v$data[[this_study]][[combo_name]]$sim_ci_ub)

            this_r_sq <- as.numeric(v$data[[this_study]][[combo_name]]$r_sq)
            this_r_sq_sim_ci_lb <- as.numeric(v$data[[this_study]][[combo_name]]$r_sq_sim_ci_lb)
            this_r_sq_sim_ci_ub <- as.numeric(v$data[[this_study]][[combo_name]]$r_sq_sim_ci_ub)

            this_d_se <- d_se(this_d, n1 = this_n1, n2 = this_n2)
            this_r_sq_se <- r_sq_se(this_r_sq, n = this_n_total)

            # make vector for indexing that combines d_mask (1D) and intersection_mask (2D/3D)
            #   mask_of_masks is a 1D that is as long as d_mask,
            #   but only has 1's where effects exist across studies (i.e., where there are 1's in intersection_mask)
            #   -> this lets us grab effects only where they exist across studies

            if (grepl("net", combo_name) | grepl("multi", combo_name)) {
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
          # d_se__group <- d__group

          r_sq__group <- d__group
          r_sq_sim_ci_lb__group <- d__group
          r_sq_sim_ci_ub__group <- d__group
          # r_sq_se__group <- d__group

          ci_level <- 1 - (0.05 / n_vars) # confidence level for simultaneous CIs

          start_time <- Sys.time()

          for (this_variable in 1:n_vars) {

            if (testing) { # simple mean

              d__group[this_variable] <- mean(d__all[this_variable,])
              d_sim_ci_lb__group[this_variable] <- mean(d_sim_ci_lb__all[this_variable,])
              d_sim_ci_ub__group[this_variable] <- mean(d_sim_ci_ub__all[this_variable,])

              r_sq__group[this_variable] <- mean(r_sq__all[this_variable,])
              r_sq_sim_ci_lb__group[this_variable] <- mean(r_sq_sim_ci_lb__all[this_variable,])
              r_sq_sim_ci_ub__group[this_variable] <- mean(r_sq_sim_ci_ub__all[this_variable,])

            } else { # meta-analysis

              # For d: do meta only if not empty or NA; otherwise set NA

              if (length(d__all[this_variable,]) == 0 || length(d_se__all[this_variable,]) == 0 || all(is.na(d__all[this_variable,])) || all(is.na(d_se__all[this_variable,]))) {
                d__group[this_variable] <- NA
                d_sim_ci_lb__group[this_variable] <- NA
                d_sim_ci_ub__group[this_variable] <- NA

              } else { # do meta

                d_meta_analysis <- NULL
                d_meta_analysis <- rma.uni(yi = d__all[this_variable,], se = d_se__all[this_variable,], method = c("REML","DL"), level = ci_level) # added alternative closed form method in case REML doesn't converge
                # d_meta_analysis <- rma.uni(yi = d__all[this_variable,], se = d_se__all[this_variable,], method = "REML", control=list(stepadj=0.5, maxiter=1000)) # added control to help with convergence

                d__group[this_variable] <- d_meta_analysis$b

                # this_ci <- confint(d_meta_analysis, level = ci_level)
                # d_sim_ci_lb__group[this_variable] <- this_ci$ci.lb
                # d_sim_ci_ub__group[this_variable] <- this_ci$ci.ub
                d_sim_ci_lb__group[this_variable] <- d_meta_analysis$ci.lb # TODO: here and below - re-specify alpha/n_vars for corrected CI
                d_sim_ci_ub__group[this_variable] <- d_meta_analysis$ci.ub
              }

              # For r_sq: do meta only if not empty or NA; otherwise set NA

              if (length(r_sq__all[this_variable,]) == 0 || length(r_sq_se__all[this_variable,]) == 0 || all(is.na(r_sq__all[this_variable,])) || all(is.na(r_sq_se__all[this_variable,]))) {
                r_sq__group[this_variable] <- NA
                r_sq_sim_ci_lb__group[this_variable] <- NA
                r_sq_sim_ci_ub__group[this_variable] <- NA

              } else {

                r_sq_meta_analysis <- NULL
                r_sq_meta_analysis <- rma.uni(yi = r_sq__all[this_variable,], se = r_sq_se__all[this_variable,], method = c("REML","DL"), level = ci_level) # added alternative closed form method in case REML doesn't converge
                # r_sq_meta_analysis <- rma.uni(yi = r_sq__all[this_variable,], se = r_sq_se__all[this_variable,], method = "REML", control=list(stepadj=0.5, maxiter=1000))

                r_sq__group[this_variable] <- r_sq_meta_analysis$b

                # this_ci <- confint(r_sq_meta_analysis, level = ci_level)
                # r_sq_sim_ci_lb__group[this_variable] <- this_ci$ci.lb
                # r_sq_sim_ci_ub__group[this_variable] <- this_ci$ci.ub
                r_sq_sim_ci_lb__group[this_variable] <- r_sq_meta_analysis$ci.lb
                r_sq_sim_ci_ub__group[this_variable] <- r_sq_meta_analysis$ci.ub
              }

            }

          }
          elapsed_time <- Sys.time() - start_time
          print(elapsed_time)
        }


        # remove values from indices that are zero in d_total, sim_ci_lb_total, and sim_ci_ub_total


        # TODO: necessary? we already do the intersection mask. (do we get enough 0's to worry, esp in activation?)
        zero_idx <- which((d__group == 0) & (d_sim_ci_lb__group == 0) & (d_sim_ci_ub__group == 0))
        r_sq_zero_idx <- which((r_sq__group == 0) & (r_sq_sim_ci_lb__group == 0) & (r_sq_sim_ci_ub__group == 0))

        # TODO: not sure we want this, after all the masking - if so, keep a new mask

        if (length(zero_idx) > 0) {
          if (testing) {
            d__group <- d__group[-zero_idx]
            # d_se__group <- d_se__group[-zero_idx]
            d_sim_ci_lb__group <- d_sim_ci_lb__group[-zero_idx]
            d_sim_ci_ub__group <- d_sim_ci_ub__group[-zero_idx]
          }

        }
        if (length(r_sq_zero_idx) > 0) {
          if (testing) {
            r_sq__group <- r_sq__group[-zero_idx]
            # r_sq_se__group <- r_sq_se__group[-zero_idx]
            r_sq_sim_ci_lb__group <- r_sq_sim_ci_lb__group[-zero_idx]
            r_sq_sim_ci_ub__group <- r_sq_sim_ci_ub__group[-zero_idx]
          }
        }

        # store d_avg, sim_ci_lb_avg, and sim_ci_ub_avg in data_group list as a list

        if (grepl("multi",combo_name)) {
          combo_name <- combo_name_orig
        }
        
        v[[meta_str]]$data[[paste0(level, "_reference_", ref)]][[combo_name]]$d <- d__group
        # v[[meta_str]]$data[[paste0(level, "_reference_", ref)]][[combo_name]]$se <- d_se__group

        v[[meta_str]]$data[[paste0(level, "_reference_", ref)]][[combo_name]]$r_sq <- r_sq__group
        # v[[meta_str]]$data[[paste0(level, "_reference_", ref)]][[combo_name]]$r_sq_se <- r_sq_se__group

        v[[meta_str]]$data[[paste0(level, "_reference_", ref)]][[combo_name]]$sim_ci_lb <- d_sim_ci_lb__group
        v[[meta_str]]$data[[paste0(level, "_reference_", ref)]][[combo_name]]$sim_ci_ub <- d_sim_ci_ub__group
        v[[meta_str]]$data[[paste0(level, "_reference_", ref)]][[combo_name]]$r_sq_sim_ci_lb <- r_sq_sim_ci_lb__group
        v[[meta_str]]$data[[paste0(level, "_reference_", ref)]][[combo_name]]$r_sq_sim_ci_ub <- r_sq_sim_ci_ub__group

        # store the study info in the study_stat dataframe

        v[[meta_str]]$study <- rbind(v[[meta_str]]$study, data.frame(group_level = level, ref = ref, name = paste0(level, "_reference_", ref)))

        # store intersection masks

        # TODO: pass this up through plotter for visualization
        v[[meta_str]]$brain_masks[[paste0(level, "_reference_", ref)]][[combo_name]]$mask <- intersection_mask
        v[[meta_str]]$brain_masks[[this_label]][[combo_name]]$mask_type <- "intersection"

      }
    }
  }

  return(v)


}

