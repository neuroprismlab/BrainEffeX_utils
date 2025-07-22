# Test file for prep_data_for_plot function
library(testthat)

# Helper function to create mock data structure
create_mock_data <- function(n_points = 200, has_mv = TRUE, has_na = FALSE, all_nan = FALSE) {
  combo_name <- "pooling.none.motion.none.mv.none"
  mv_combo_name <- "pooling.none.motion.none.mv.multi"
  
  if (all_nan) {
    # Create data with all NaN values
    d_values <- rep(NaN, n_points)
    r_sq_values <- rep(NaN, n_points)
  } else {
    # Create realistic effect size data
    d_values <- rnorm(n_points, mean = 0.3, sd = 0.4)
    r_sq_values <- abs(rnorm(n_points, mean = 0.1, sd = 0.05))
  }
  
  if (has_na) {
    # Add some NA values
    na_indices <- sample(1:n_points, n_points * 0.1)
    d_values[na_indices] <- NA
    r_sq_values[na_indices] <- NA
  }
  
  data <- list()
  
  # Main combo data
  data[[combo_name]] <- list(
    d = d_values,
    sim_ci_lb = d_values - abs(rnorm(n_points, mean = 0.1, sd = 0.05)),
    sim_ci_ub = d_values + abs(rnorm(n_points, mean = 0.1, sd = 0.05)),
    r_sq = r_sq_values,
    r_sq_sim_ci_lb = pmax(0, r_sq_values - abs(rnorm(n_points, mean = 0.02, sd = 0.01))),
    r_sq_sim_ci_ub = r_sq_values + abs(rnorm(n_points, mean = 0.02, sd = 0.01)),
    n = sample(500:2000, 1)
  )
  
  # Multivariate combo data (if requested)
  if (has_mv) {
    data[[mv_combo_name]] <- list(
      d = mean(d_values, na.rm = TRUE),
      sim_ci_lb = mean(d_values, na.rm = TRUE) - 0.1,
      sim_ci_ub = mean(d_values, na.rm = TRUE) + 0.1,
      r_sq = mean(r_sq_values, na.rm = TRUE),
      r_sq_sim_ci_lb = mean(r_sq_values, na.rm = TRUE) - 0.02,
      r_sq_sim_ci_ub = mean(r_sq_values, na.rm = TRUE) + 0.02
    )
  }
  
  return(data)
}

# Helper function to create mock study details
create_mock_study_details <- function() {
  list(
    dataset = "test_dataset",
    orig_stat_type = "d",
    test_component_1 = "component1",
    test_component_2 = "component2",
    map_type = "activation",
    group = "test_group",
    ref = "voxel"
  )
}

# Helper function to create mock plot info
create_mock_plot_info <- function(grouping_var = "none") {
  list(
    grouping_var = grouping_var,
    group_level = "test_level",
    ref = "voxel"
  )
}

# Helper function to create mock brain masks
create_mock_brain_masks <- function(n_points = 200) {
  list(
    mask = rep(1, n_points)
  )
}

describe("prep_data_for_plot function", {
  
  describe("Basic functionality", {
    
    it("processes Cohen's d data successfully", {
      data <- create_mock_data()
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      result <- prep_data_for_plot(
        data = data,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      expect_type(result, "list")
      expect_true("data" %in% names(result))
      expect_true("extra_study_details" %in% names(result))
      expect_true("study_details" %in% names(result))
    })
    
    it("processes R-squared data successfully", {
      data <- create_mock_data()
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      result <- prep_data_for_plot(
        data = data,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "r_sq",
        plot_info = plot_info
      )
      
      expect_type(result, "list")
      expect_true(all(result$data$estimate >= 0)) # R-squared should be non-negative
    })
    
    it("returns correct data structure", {
      data <- create_mock_data()
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      result <- prep_data_for_plot(
        data = data,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      # Check data structure
      expect_true("estimate" %in% names(result$data))
      expect_true("ub" %in% names(result$data))
      expect_true("lb" %in% names(result$data))
      expect_true("cons_estimate" %in% names(result$data))
      expect_true("below_cross_idx" %in% names(result$data))
      expect_true("above_cross_idx" %in% names(result$data))
      
      # Check extra_study_details structure
      expect_true("percent_not_zero" %in% names(result$extra_study_details))
      expect_true("max_cons_estimate" %in% names(result$extra_study_details))
      expect_true("n_title" %in% names(result$extra_study_details))
      expect_true("mv_estimate" %in% names(result$extra_study_details))
      expect_true("mv_ci" %in% names(result$extra_study_details))
      expect_true("grouping_var" %in% names(result$extra_study_details))
      expect_true("group_level" %in% names(result$extra_study_details))
      expect_true("ref" %in% names(result$extra_study_details))
    })
  })
  
  describe("Data cleaning and preprocessing", {
    
    it("removes NA values correctly", {
      data <- create_mock_data(has_na = TRUE)
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      result <- prep_data_for_plot(
        data = data,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      # Check that no NA values remain
      expect_false(any(is.na(result$data$estimate)))
      expect_false(any(is.na(result$data$lb)))
      expect_false(any(is.na(result$data$ub)))
    })
    
    it("sorts data by effect size", {
      data <- create_mock_data()
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      result <- prep_data_for_plot(
        data = data,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      # Check that data is sorted
      expect_true(all(diff(result$data$estimate) >= 0))
    })
    
    it("downsamples data for large datasets", {
      data <- create_mock_data(n_points = 500)
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      result <- prep_data_for_plot(
        data = data,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      # Should be downsampled to around 100 points or fewer
      expect_lte(length(result$data$estimate), 102) # Allow some margin for last element inclusion
    })
    
    it("preserves all data for small datasets", {
      data <- create_mock_data(n_points = 50)
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      result <- prep_data_for_plot(
        data = data,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      # Small datasets should not be downsampled much
      expect_gte(length(result$data$estimate), 40) # Should preserve most data
    })
    
    it("includes last element after downsampling", {
      data <- create_mock_data(n_points = 1000)
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      # Set a known last value
      combo_name <- "pooling.none.motion.none.mv.none"
      data[[combo_name]]$d[1000] <- 999 # This will be the largest after sorting
      
      result <- prep_data_for_plot(
        data = data,
        study_details = study_details,
        combo_name = combo_name,
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      # The largest value should be preserved
      expect_equal(max(result$data$estimate), 999)
    })
  })
  
  describe("Conservative effect size calculation", {
    
    it("calculates conservative effect sizes correctly", {
      data <- create_mock_data()
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      result <- prep_data_for_plot(
        data = data,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      # Conservative estimates should be between lower and upper bounds
      expect_true(all(result$data$cons_estimate >= pmin(result$data$lb, 0)))
      expect_true(all(result$data$cons_estimate <= pmax(result$data$ub, 0)))
    })
    
    it("finds maximum conservative effect size", {
      data <- create_mock_data()
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      result <- prep_data_for_plot(
        data = data,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      # Max conservative estimate should be one of the conservative estimates
      expect_true(result$extra_study_details$max_cons_estimate %in% result$data$cons_estimate)
      
      # Should be the one with maximum absolute value
      max_abs_cons <- result$data$cons_estimate[which.max(abs(result$data$cons_estimate))]
      expect_equal(result$extra_study_details$max_cons_estimate, max_abs_cons)
    })
  })
  
  describe("Cross-over index calculation", {
    
    it("calculates cross-over indices correctly", {
      data <- create_mock_data()
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      result <- prep_data_for_plot(
        data = data,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      expect_type(result$data$below_cross_idx, "double")
      expect_type(result$data$above_cross_idx, "double")
      
      # Check that indices are within valid range
      expect_true(all(result$data$below_cross_idx >= 1))
      expect_true(all(result$data$above_cross_idx <= length(result$data$estimate)))
      expect_true(length(result$data$below_cross_idx) >= 1)
      expect_true(length(result$data$above_cross_idx) >= 1)
    })
    
    it("handles cases with no negative effects", {
      # Create data with all positive effects
      data <- create_mock_data()
      combo_name <- "pooling.none.motion.none.mv.none"
      data[[combo_name]]$d <- abs(data[[combo_name]]$d) + 0.1
      data[[combo_name]]$sim_ci_lb <- abs(data[[combo_name]]$sim_ci_lb)
      data[[combo_name]]$sim_ci_ub <- abs(data[[combo_name]]$sim_ci_ub) + 0.1
      
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      result <- prep_data_for_plot(
        data = data,
        study_details = study_details,
        combo_name = combo_name,
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      expect_equal(result$data$below_cross_idx, 1)
    })
    
    it("handles cases with no positive effects", {
      # Create data with all negative effects
      data <- create_mock_data()
      combo_name <- "pooling.none.motion.none.mv.none"
      data[[combo_name]]$d <- -abs(data[[combo_name]]$d) - 0.1
      data[[combo_name]]$sim_ci_lb <- -abs(data[[combo_name]]$sim_ci_lb) - 0.1
      data[[combo_name]]$sim_ci_ub <- -abs(data[[combo_name]]$sim_ci_ub)
      
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      result <- prep_data_for_plot(
        data = data,
        study_details = study_details,
        combo_name = combo_name,
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      expect_equal(result$data$above_cross_idx, length(result$data$estimate))
    })
  })
  
  describe("Percent not zero calculation", {
    
    it("calculates percent not overlapping zero correctly", {
      data <- create_mock_data()
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      result <- prep_data_for_plot(
        data = data,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      expect_gte(result$extra_study_details$percent_not_zero, 0)
      expect_lte(result$extra_study_details$percent_not_zero, 1)
    })
  })
  
  describe("Multivariate data handling", {
    
    it("handles multivariate data when present", {
      data <- create_mock_data(has_mv = TRUE)
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      result <- prep_data_for_plot(
        data = data,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      expect_false(is.null(result$extra_study_details$mv_estimate))
      expect_length(result$extra_study_details$mv_ci, 2)
    })
    
    it("handles missing multivariate data gracefully", {
      data <- create_mock_data(has_mv = FALSE)
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      result <- prep_data_for_plot(
        data = data,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      expect_true(is.null(result$extra_study_details$mv_estimate))
    })
    
    it("handles list format confidence intervals", {
      data <- create_mock_data(has_mv = TRUE)
      combo_name <- "pooling.none.motion.none.mv.none"
      mv_combo_name <- "pooling.none.motion.none.mv.multi"
      
      # Convert CI to list format
      data[[combo_name]]$sim_ci_lb <- list(data[[combo_name]]$sim_ci_lb)
      data[[combo_name]]$sim_ci_ub <- list(data[[combo_name]]$sim_ci_ub)
      data[[mv_combo_name]]$sim_ci_lb <- list(data[[mv_combo_name]]$sim_ci_lb)
      data[[mv_combo_name]]$sim_ci_ub <- list(data[[mv_combo_name]]$sim_ci_ub)
      
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      expect_no_error({
        result <- prep_data_for_plot(
          data = data,
          study_details = study_details,
          combo_name = combo_name,
          mv_combo_name = mv_combo_name,
          estimate = "d",
          plot_info = plot_info
        )
      })
    })
  })
  
  describe("Spatial plotting preparation", {
    
    it("handles spatial plotting flag correctly", {
      data <- create_mock_data()
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      brain_masks <- create_mock_brain_masks()
      
      result <- prep_data_for_plot(
        data = data,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info,
        prep_spatial = TRUE,
        brain_masks = brain_masks
      )
      
      expect_true("brain_masks" %in% names(result$extra_study_details))
      expect_equal(result$extra_study_details$brain_masks, brain_masks)
      
      # Should not downsample for spatial plots
      expect_gte(length(result$data$estimate), 180) # Should preserve most data
    })
    
    it("errors when prep_spatial is TRUE but brain_masks is NA", {
      data <- create_mock_data()
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      expect_error({
        prep_data_for_plot(
          data = data,
          study_details = study_details,
          combo_name = "pooling.none.motion.none.mv.none",
          mv_combo_name = "pooling.none.motion.none.mv.multi",
          estimate = "d",
          plot_info = plot_info,
          prep_spatial = TRUE,
          brain_masks = NA
        )
      }, "prep_spatial is TRUE but brain_masks is NA")
    })
    
    it("returns data in original order for spatial plots", {
      data <- create_mock_data()
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      brain_masks <- create_mock_brain_masks()
      
      # Set a known pattern that would be disrupted by sorting
      combo_name <- "pooling.none.motion.none.mv.none"
      data[[combo_name]]$d <- c(1, 2, 3, 4, 5, rep(0, 195)) # Ascending pattern
      
      result <- prep_data_for_plot(
        data = data,
        study_details = study_details,
        combo_name = combo_name,
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info,
        prep_spatial = TRUE,
        brain_masks = brain_masks
      )
      
      # First few elements should be in original order
      expect_equal(result$data$estimate[1:5], c(1, 2, 3, 4, 5))
    })
  })
  
  describe("Different grouping variables", {
    
    it("handles different grouping variables", {
      data <- create_mock_data()
      study_details <- create_mock_study_details()
      
      grouping_vars <- c("none", "orig_stat_type", "category")
      
      for (grouping_var in grouping_vars) {
        plot_info <- create_mock_plot_info(grouping_var = grouping_var)
        
        result <- prep_data_for_plot(
          data = data,
          study_details = study_details,
          combo_name = "pooling.none.motion.none.mv.none",
          mv_combo_name = "pooling.none.motion.none.mv.multi",
          estimate = "d",
          plot_info = plot_info
        )
        
        expect_equal(result$extra_study_details$grouping_var, grouping_var)
      }
    })
  })
  
  describe("Edge cases and error handling", {
    
    it("returns NULL for data with all NaN values", {
      data <- create_mock_data(all_nan = TRUE)
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      result <- prep_data_for_plot(
        data = data,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      expect_null(result)
    })
    
    it("handles very small datasets", {
      data <- create_mock_data(n_points = 1)
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      result <- prep_data_for_plot(
        data = data,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      expect_type(result, "list")
      expect_length(result$data$estimate, 1)
    })
    
    it("handles missing combo names gracefully", {
      data <- create_mock_data()
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      expect_error({
        prep_data_for_plot(
          data = data,
          study_details = study_details,
          combo_name = "nonexistent.combo",
          mv_combo_name = "pooling.none.motion.none.mv.multi",
          estimate = "d",
          plot_info = plot_info
        )
      })
    })
    
    it("validates estimate parameter", {
      data <- create_mock_data()
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      # Valid estimates should work
      expect_no_error({
        prep_data_for_plot(
          data = data,
          study_details = study_details,
          combo_name = "pooling.none.motion.none.mv.none",
          mv_combo_name = "pooling.none.motion.none.mv.multi",
          estimate = "d",
          plot_info = plot_info
        )
      })
      
      expect_no_error({
        prep_data_for_plot(
          data = data,
          study_details = study_details,
          combo_name = "pooling.none.motion.none.mv.none",
          mv_combo_name = "pooling.none.motion.none.mv.multi",
          estimate = "r_sq",
          plot_info = plot_info
        )
      })
    })
  })
})