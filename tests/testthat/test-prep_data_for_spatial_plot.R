# Test file for prep_data_for_spatial_plot function
library(testthat)

# Helper function to create mock data structure for spatial plotting
create_mock_spatial_data <- function(n_points = 1000, estimate_type = "d", as_list = FALSE) {
  combo_name <- "pooling.none.motion.none.mv.none"
  mv_combo_name <- "pooling.none.motion.none.mv.multi"
  
  # Create effect size data
  if (estimate_type == "d") {
    estimate_values <- rnorm(n_points, mean = 0.3, sd = 0.4)
  } else if (estimate_type == "r_sq") {
    estimate_values <- abs(rnorm(n_points, mean = 0.1, sd = 0.05))
  } else {
    estimate_values <- rnorm(n_points, mean = 0, sd = 1)
  }
  
  # Convert to list format if requested
  if (as_list) {
    estimate_values <- list(estimate_values)
  }
  
  data <- list()
  
  # Main combo data
  data[[combo_name]] <- list()
  data[[combo_name]][[estimate_type]] <- estimate_values
  data[[combo_name]]$n <- sample(500:2000, 1)
  
  # Multivariate combo data
  data[[mv_combo_name]] <- list()
  data[[mv_combo_name]][[estimate_type]] <- mean(unlist(estimate_values), na.rm = TRUE)
  data[[mv_combo_name]]$n <- data[[combo_name]]$n
  
  return(data)
}

# Helper function to create mock brain masks
create_mock_brain_masks <- function(n_points = 1000, mask_type = "simple") {
  if (mask_type == "simple") {
    list(
      mask = rep(1, n_points)
    )
  } else if (mask_type == "complex") {
    list(
      mask = rep(1, n_points),
      coordinates = data.frame(
        x = 1:n_points,
        y = rep(1, n_points),
        z = rep(1, n_points)
      ),
      atlas_info = "test_atlas"
    )
  } else if (mask_type == "partial") {
    mask_vector <- rep(0, n_points)
    mask_vector[sample(1:n_points, n_points * 0.7)] <- 1  # 70% active voxels
    list(
      mask = mask_vector
    )
  }
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
create_mock_plot_info <- function(grouping_var = "none", ref = "voxel") {
  list(
    grouping_var = grouping_var,
    group_level = "test_level",
    ref = ref
  )
}

describe("prep_data_for_spatial_plot function", {
  
  describe("Basic functionality", {
    
    it("processes Cohen's d data successfully", {
      data <- create_mock_spatial_data(estimate_type = "d")
      brain_masks <- create_mock_brain_masks()
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      result <- prep_data_for_spatial_plot(
        data = data,
        brain_masks = brain_masks,
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
      data <- create_mock_spatial_data(estimate_type = "r_sq")
      brain_masks <- create_mock_brain_masks()
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      result <- prep_data_for_spatial_plot(
        data = data,
        brain_masks = brain_masks,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "r_sq",
        plot_info = plot_info
      )
      
      expect_type(result, "list")
      expect_true(all(result$data$estimate >= 0)) # R-squared should be non-negative
    })
    
    it("returns correct simplified data structure for spatial plots", {
      data <- create_mock_spatial_data()
      brain_masks <- create_mock_brain_masks()
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      result <- prep_data_for_spatial_plot(
        data = data,
        brain_masks = brain_masks,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      # Check data structure - should be simpler than regular prep_data_for_plot
      expect_true("estimate" %in% names(result$data))
      expect_length(names(result$data), 1) # Should only have estimate, not CI bounds etc.
      
      # Check extra_study_details structure
      expect_true("n_title" %in% names(result$extra_study_details))
      expect_true("grouping_var" %in% names(result$extra_study_details))
      expect_true("group_level" %in% names(result$extra_study_details))
      expect_true("ref" %in% names(result$extra_study_details))
      expect_true("brain_masks" %in% names(result$extra_study_details))
      
      # Check that study_details is preserved
      expect_equal(result$study_details, study_details)
    })
  })
  
  describe("Data format handling", {
    
    it("handles regular vector data", {
      data <- create_mock_spatial_data(as_list = FALSE)
      brain_masks <- create_mock_brain_masks()
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      result <- prep_data_for_spatial_plot(
        data = data,
        brain_masks = brain_masks,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      expect_type(result$data$estimate, "double")
      expect_false(is.list(result$data$estimate))
    })
    
    it("unlists data when provided as list", {
      data <- create_mock_spatial_data(as_list = TRUE)
      brain_masks <- create_mock_brain_masks()
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      result <- prep_data_for_spatial_plot(
        data = data,
        brain_masks = brain_masks,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      expect_type(result$data$estimate, "double")
      expect_false(is.list(result$data$estimate))
      expect_length(result$data$estimate, 1000) # Should be unlisted
    })
    
    it("preserves data length for spatial plots", {
      n_points <- 2500
      data <- create_mock_spatial_data(n_points = n_points)
      brain_masks <- create_mock_brain_masks(n_points = n_points)
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      result <- prep_data_for_spatial_plot(
        data = data,
        brain_masks = brain_masks,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      # Should preserve all data points for spatial plotting (no downsampling)
      expect_length(result$data$estimate, n_points)
    })
  })
  
  describe("Brain masks handling", {
    
    it("includes brain masks in output", {
      data <- create_mock_spatial_data()
      brain_masks <- create_mock_brain_masks(mask_type = "simple")
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      result <- prep_data_for_spatial_plot(
        data = data,
        brain_masks = brain_masks,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      expect_equal(result$extra_study_details$brain_masks, brain_masks)
      expect_true("mask" %in% names(result$extra_study_details$brain_masks))
    })
    
    it("handles complex brain masks", {
      data <- create_mock_spatial_data()
      brain_masks <- create_mock_brain_masks(mask_type = "complex")
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      result <- prep_data_for_spatial_plot(
        data = data,
        brain_masks = brain_masks,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      expect_equal(result$extra_study_details$brain_masks, brain_masks)
      expect_true("coordinates" %in% names(result$extra_study_details$brain_masks))
      expect_true("atlas_info" %in% names(result$extra_study_details$brain_masks))
    })
    
    it("handles partial brain masks", {
      data <- create_mock_spatial_data()
      brain_masks <- create_mock_brain_masks(mask_type = "partial")
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      result <- prep_data_for_spatial_plot(
        data = data,
        brain_masks = brain_masks,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      expect_equal(result$extra_study_details$brain_masks, brain_masks)
      # Check that partial mask has some 0s and some 1s
      mask_values <- result$extra_study_details$brain_masks$mask
      expect_true(any(mask_values == 0))
      expect_true(any(mask_values == 1))
    })
  })
  
  describe("Sample size and metadata handling", {
    
    it("formats sample size correctly", {
      data <- create_mock_spatial_data()
      # Set specific sample size
      data[["pooling.none.motion.none.mv.none"]]$n <- 1500
      
      brain_masks <- create_mock_brain_masks()
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      result <- prep_data_for_spatial_plot(
        data = data,
        brain_masks = brain_masks,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      expect_equal(result$extra_study_details$n_title, "n = 1500")
    })
    
    it("preserves plot info correctly", {
      data <- create_mock_spatial_data()
      brain_masks <- create_mock_brain_masks()
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info(grouping_var = "orig_stat_type", ref = "shen_268")
      
      result <- prep_data_for_spatial_plot(
        data = data,
        brain_masks = brain_masks,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      expect_equal(result$extra_study_details$grouping_var, "orig_stat_type")
      expect_equal(result$extra_study_details$group_level, "test_level")
      expect_equal(result$extra_study_details$ref, "shen_268")
    })
    
    it("preserves study details exactly", {
      data <- create_mock_spatial_data()
      brain_masks <- create_mock_brain_masks()
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      result <- prep_data_for_spatial_plot(
        data = data,
        brain_masks = brain_masks,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      expect_identical(result$study_details, study_details)
    })
  })
  
  describe("Different reference spaces", {
    
    it("handles voxel reference space", {
      data <- create_mock_spatial_data()
      brain_masks <- create_mock_brain_masks()
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info(ref = "voxel")
      
      result <- prep_data_for_spatial_plot(
        data = data,
        brain_masks = brain_masks,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      expect_equal(result$extra_study_details$ref, "voxel")
    })
    
    it("handles parcellation reference spaces", {
      data <- create_mock_spatial_data(n_points = 268) # Typical parcellation size
      brain_masks <- create_mock_brain_masks(n_points = 268)
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info(ref = "shen_268")
      
      result <- prep_data_for_spatial_plot(
        data = data,
        brain_masks = brain_masks,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      expect_equal(result$extra_study_details$ref, "shen_268")
      expect_length(result$data$estimate, 268)
    })
    
    it("handles connectivity reference spaces", {
      data <- create_mock_spatial_data(n_points = 1431) # 268*267/2 for connectivity
      brain_masks <- create_mock_brain_masks(n_points = 1431)
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info(ref = "shen_268_conn")
      
      result <- prep_data_for_spatial_plot(
        data = data,
        brain_masks = brain_masks,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      expect_equal(result$extra_study_details$ref, "shen_268_conn")
      expect_length(result$data$estimate, 1431)
    })
  })
  
  describe("Different grouping variables", {
    
    it("handles 'none' grouping", {
      data <- create_mock_spatial_data()
      brain_masks <- create_mock_brain_masks()
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info(grouping_var = "none")
      
      result <- prep_data_for_spatial_plot(
        data = data,
        brain_masks = brain_masks,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      expect_equal(result$extra_study_details$grouping_var, "none")
    })
    
    it("handles 'orig_stat_type' grouping", {
      data <- create_mock_spatial_data()
      brain_masks <- create_mock_brain_masks()
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info(grouping_var = "orig_stat_type")
      
      result <- prep_data_for_spatial_plot(
        data = data,
        brain_masks = brain_masks,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      expect_equal(result$extra_study_details$grouping_var, "orig_stat_type")
    })
    
    it("handles 'category' grouping", {
      data <- create_mock_spatial_data()
      brain_masks <- create_mock_brain_masks()
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info(grouping_var = "category")
      
      result <- prep_data_for_spatial_plot(
        data = data,
        brain_masks = brain_masks,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      expect_equal(result$extra_study_details$grouping_var, "category")
    })
  })
  
  describe("Data validation and edge cases", {
    
    it("handles very small datasets", {
      data <- create_mock_spatial_data(n_points = 1)
      brain_masks <- create_mock_brain_masks(n_points = 1)
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      result <- prep_data_for_spatial_plot(
        data = data,
        brain_masks = brain_masks,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      expect_length(result$data$estimate, 1)
      expect_type(result, "list")
    })
    
    it("handles very large datasets", {
      data <- create_mock_spatial_data(n_points = 100000)
      brain_masks <- create_mock_brain_masks(n_points = 100000)
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      result <- prep_data_for_spatial_plot(
        data = data,
        brain_masks = brain_masks,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      expect_length(result$data$estimate, 100000)
      expect_type(result, "list")
    })
    
    it("handles extreme effect size values", {
      data <- create_mock_spatial_data()
      # Set extreme values
      combo_name <- "pooling.none.motion.none.mv.none"
      data[[combo_name]]$d <- c(rep(-1000, 500), rep(1000, 500))
      
      brain_masks <- create_mock_brain_masks()
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      result <- prep_data_for_spatial_plot(
        data = data,
        brain_masks = brain_masks,
        study_details = study_details,
        combo_name = combo_name,
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      expect_true(any(result$data$estimate == -1000))
      expect_true(any(result$data$estimate == 1000))
    })
    
    it("handles zero effect sizes", {
      data <- create_mock_spatial_data()
      # Set all values to zero
      combo_name <- "pooling.none.motion.none.mv.none"
      data[[combo_name]]$d <- rep(0, 1000)
      
      brain_masks <- create_mock_brain_masks()
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      result <- prep_data_for_spatial_plot(
        data = data,
        brain_masks = brain_masks,
        study_details = study_details,
        combo_name = combo_name,
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      expect_true(all(result$data$estimate == 0))
    })
  })
  
  describe("Error handling", {
    
    it("handles missing combo names", {
      data <- create_mock_spatial_data()
      brain_masks <- create_mock_brain_masks()
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      expect_error({
        prep_data_for_spatial_plot(
          data = data,
          brain_masks = brain_masks,
          study_details = study_details,
          combo_name = "nonexistent.combo",
          mv_combo_name = "pooling.none.motion.none.mv.multi",
          estimate = "d",
          plot_info = plot_info
        )
      }, "combo_name 'nonexistent.combo' not found in data")
    })
    
    it("handles missing estimate type in data", {
      data <- create_mock_spatial_data(estimate_type = "d")
      brain_masks <- create_mock_brain_masks()
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      expect_error({
        prep_data_for_spatial_plot(
          data = data,
          brain_masks = brain_masks,
          study_details = study_details,
          combo_name = "pooling.none.motion.none.mv.none",
          mv_combo_name = "pooling.none.motion.none.mv.multi",
          estimate = "r_sq", # Requesting r_sq but data only has d
          plot_info = plot_info
        )
      }, "estimate 'r_sq' not found in data for combo 'pooling.none.motion.none.mv.none'")
    })
    
    it("handles invalid plot_info", {
      data <- create_mock_spatial_data()
      brain_masks <- create_mock_brain_masks()
      study_details <- create_mock_study_details()
      
      # Test with string instead of list
      expect_error({
        prep_data_for_spatial_plot(
          data = data,
          brain_masks = brain_masks,
          study_details = study_details,
          combo_name = "pooling.none.motion.none.mv.none",
          mv_combo_name = "pooling.none.motion.none.mv.multi",
          estimate = "d",
          plot_info = "invalid_plot_info"
        )
      }, "plot_info must be a list containing grouping_var, group_level, and ref")
    })
  })
  
  describe("Comparison with regular prep_data_for_plot", {
    
    it("returns simpler structure than prep_data_for_plot", {
      data <- create_mock_spatial_data()
      brain_masks <- create_mock_brain_masks()
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      result <- prep_data_for_spatial_plot(
        data = data,
        brain_masks = brain_masks,
        study_details = study_details,
        combo_name = "pooling.none.motion.none.mv.none",
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      # Should only have estimate in data (no CI bounds, conservative estimates, etc.)
      expect_length(names(result$data), 1)
      expect_equal(names(result$data), "estimate")
      
      # Should not have statistical summaries that prep_data_for_plot includes
      expect_false("percent_not_zero" %in% names(result$extra_study_details))
      expect_false("max_cons_estimate" %in% names(result$extra_study_details))
      expect_false("mv_estimate" %in% names(result$extra_study_details))
      expect_false("mv_ci" %in% names(result$extra_study_details))
      
      # But should have spatial-specific items
      expect_true("brain_masks" %in% names(result$extra_study_details))
    })
    
    it("preserves original data order (no sorting)", {
      data <- create_mock_spatial_data()
      # Set a known pattern that would be disrupted by sorting
      combo_name <- "pooling.none.motion.none.mv.none"
      data[[combo_name]]$d <- c(5, 1, 3, 2, 4) # Non-sorted pattern
      
      brain_masks <- create_mock_brain_masks(n_points = 5)
      study_details <- create_mock_study_details()
      plot_info <- create_mock_plot_info()
      
      result <- prep_data_for_spatial_plot(
        data = data,
        brain_masks = brain_masks,
        study_details = study_details,
        combo_name = combo_name,
        mv_combo_name = "pooling.none.motion.none.mv.multi",
        estimate = "d",
        plot_info = plot_info
      )
      
      # Should preserve original order
      expect_equal(result$data$estimate, c(5, 1, 3, 2, 4))
    })
  })
})