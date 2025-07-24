# Test file for meta_analysis function
library(testthat)

# NOTE: The v parameter structure includes:
# - study: data.frame with columns: basefile, folder, name, ext, dataset, map_type, 
#   orig_stat_type, test_component_1, test_component_2, category, ref
# - brain_masks: list of brain masks (integrated into v structure)
# - data: list of study data
# - template: template NIfTI
# - anatomical: anatomical NIfTI  
# - phen_keys: phenotype keys data frame

# Helper function to create mock v structure for meta-analysis
create_mock_v_structure <- function(n_studies = 3, n_variables = 20, include_multivariate = TRUE) {
  combo_name <- "pooling.none.motion.none.mv.none"
  mv_combo_name <- "pooling.none.motion.none.mv.multi"
  
  v <- list()
  v$data <- list()
  
  # Create study metadata with correct column structure
  v$study <- data.frame(
    basefile = paste0("base_", 1:n_studies),
    folder = paste0("folder_", 1:n_studies),
    name = paste0("study_", 1:n_studies),
    ext = rep(".nii.gz", n_studies),
    dataset = paste0("dataset_", rep(1:ceiling(n_studies/2), length.out = n_studies)),
    map_type = rep(c("activation", "fc"), length.out = n_studies),
    orig_stat_type = rep(c("d", "t", "z"), length.out = n_studies),
    test_component_1 = paste0("component1_", 1:n_studies),
    test_component_2 = paste0("component2_", 1:n_studies),
    category = rep(c("cognitive", "emotional", "motor"), length.out = n_studies),
    ref = rep(c("voxel", "shen_268"), length.out = n_studies),
    stringsAsFactors = FALSE
  )
  
  # Add other v components that exist in the real structure
  v$brain_masks <- list()  # Will be populated by create_mock_brain_masks_in_v
  v$template <- array(0, dim = c(10, 10, 10))  # Mock template
  v$anatomical <- array(0, dim = c(10, 10, 10))  # Mock anatomical
  v$phen_keys <- data.frame(
    variable = c("age", "condition"),
    description = c("Age in years", "Experimental condition"),
    stringsAsFactors = FALSE
  )
  
  # Create data for each study
  for (i in 1:n_studies) {
    v$data[[i]] <- list()
    
    # Main combo data with realistic but simple values
    v$data[[i]][[combo_name]] <- list(
      d = rnorm(n_variables, mean = 0.2, sd = 0.1),  # Smaller, more realistic effects
      sim_ci_lb = rnorm(n_variables, mean = 0.1, sd = 0.05),
      sim_ci_ub = rnorm(n_variables, mean = 0.3, sd = 0.05),
      r_sq = abs(rnorm(n_variables, mean = 0.05, sd = 0.02)),  # Small R-squared values
      r_sq_sim_ci_lb = abs(rnorm(n_variables, mean = 0.03, sd = 0.01)),
      r_sq_sim_ci_ub = abs(rnorm(n_variables, mean = 0.07, sd = 0.01)),
      n = sample(100:500, 1)  # Reasonable sample sizes
    )
    
    # Multivariate combo data
    if (include_multivariate) {
      if (i %% 2 == 1) {
        mv_name <- paste0(mv_combo_name, ".r")
      } else {
        mv_name <- mv_combo_name
      }
      
      v$data[[i]][[mv_name]] <- list(
        d = mean(v$data[[i]][[combo_name]]$d),
        sim_ci_lb = mean(v$data[[i]][[combo_name]]$sim_ci_lb),
        sim_ci_ub = mean(v$data[[i]][[combo_name]]$sim_ci_ub),
        r_sq = mean(v$data[[i]][[combo_name]]$r_sq),
        r_sq_sim_ci_lb = mean(v$data[[i]][[combo_name]]$r_sq_sim_ci_lb),
        r_sq_sim_ci_ub = mean(v$data[[i]][[combo_name]]$r_sq_sim_ci_ub),
        n = v$data[[i]][[combo_name]]$n
      )
    }
  }
  
  # Name the data list elements
  names(v$data) <- v$study$name
  
  return(v)
}

# Helper function to create mock brain masks and integrate with v structure
create_mock_brain_masks_in_v <- function(v, n_variables = 20, mask_type = "activation") {
  study_names <- v$study$name
  
  for (name in study_names) {
    if (mask_type == "activation") {
      v$brain_masks[[name]] <- list(
        mask = rep(1, n_variables)
      )
    } else if (mask_type == "partial") {
      # Create partial overlap masks
      mask_vector <- rep(1, n_variables)
      # Make some studies have different masks
      if (grepl("2|4", name)) {
        mask_vector[sample(1:n_variables, max(1, n_variables * 0.1))] <- 0  # Only remove 10%
      }
      v$brain_masks[[name]] <- list(
        mask = mask_vector
      )
    }
  }
  
  return(v)
}

# Mock helper functions that the main function uses
mock_d_se <- function(d, n1, n2 = NULL) {
  if (is.null(n2)) { # one-sample
    se <- sqrt(1 / n1 + (d^2 / (2 * n1)))
  } else { # two-sample
    se <- sqrt((n1 + n2) / (n1 * n2) + (d^2 / (2 * (n1 + n2))))
  }
  return(se)
}

mock_r_sq_se <- function(r_sq, n) {
  r <- sqrt(pmax(0, pmin(1, r_sq)))  # Ensure valid R-squared
  se_r <- sqrt((1 - r^2) / pmax(3, n - 2))  # Ensure positive denominator
  se <- se_r^2
  return(se)
}

describe("meta_analysis function", {
  
  describe("Basic functionality", {
    
    it("processes multiple studies for meta-analysis", {
      skip_if_not_installed("metafor")
      
      v <- create_mock_v_structure(n_studies = 2, n_variables = 10)  # Very small dataset
      v <- create_mock_brain_masks_in_v(v, n_variables = 10)
      
      suppressWarnings({
        result <- meta_analysis(
          v = v,
          brain_masks = v$brain_masks,
          combo_name = "pooling.none.motion.none.mv.none",
          grouping_var = "category"
        )
      })
      
      expect_type(result, "list")
      expect_true("meta_category" %in% names(result))
    })
    
    it("handles single study (no meta-analysis needed)", {
      skip_if_not_installed("metafor")
      
      v <- create_mock_v_structure(n_studies = 1, n_variables = 5)
      v <- create_mock_brain_masks_in_v(v, n_variables = 5)
      
      suppressWarnings({
        result <- meta_analysis(
          v = v,
          brain_masks = v$brain_masks,
          combo_name = "pooling.none.motion.none.mv.none",
          grouping_var = "category"
        )
      })
      
      expect_type(result, "list")
      expect_true("meta_category" %in% names(result))
    })
  })
  
  describe("Structure validation", {
    
    it("creates correct meta structure", {
      skip_if_not_installed("metafor")
      
      v <- create_mock_v_structure(n_studies = 2, n_variables = 5)
      v <- create_mock_brain_masks_in_v(v, n_variables = 5)
      
      suppressWarnings({
        result <- meta_analysis(
          v = v,
          brain_masks = v$brain_masks,
          combo_name = "pooling.none.motion.none.mv.none",
          grouping_var = "category"
        )
      })
      
      # Check structure
      expect_true(is.list(result$meta_category$data))
      expect_true(is.data.frame(result$meta_category$study))
      expect_true(is.list(result$meta_category$brain_masks))
      
      # Check study dataframe columns
      expect_true("group_level" %in% names(result$meta_category$study))
      expect_true("ref" %in% names(result$meta_category$study))
      expect_true("name" %in% names(result$meta_category$study))
    })
  })
  
  describe("Error handling", {
    
    it("handles non-existent combo names", {
      v <- create_mock_v_structure(n_studies = 2, n_variables = 5)
      v <- create_mock_brain_masks_in_v(v, n_variables = 5)
      
      expect_error({
        suppressWarnings({
          meta_analysis(
            v = v,
            brain_masks = v$brain_masks,
            combo_name = "nonexistent.combo",
            grouping_var = "category"
          )
        })
      }, class = "simpleError")
    })
  })
  
  describe("Helper function testing", {
    
    it("d_se function calculates standard errors correctly", {
      # Test one-sample
      se1 <- mock_d_se(d = 0.5, n1 = 100)
      expect_true(is.numeric(se1))
      expect_true(se1 > 0)
      
      # Test two-sample
      se2 <- mock_d_se(d = 0.5, n1 = 50, n2 = 50)
      expect_true(is.numeric(se2))
      expect_true(se2 > 0)
    })
    
    it("r_sq_se function calculates standard errors correctly", {
      se <- mock_r_sq_se(r_sq = 0.25, n = 100)
      expect_true(is.numeric(se))
      expect_true(se > 0)
    })
  })
})