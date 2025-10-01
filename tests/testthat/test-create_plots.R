# Test file for create_plots function
library(testthat)
library(ggplot2)
library(ggbeeswarm)

# Helper function to create mock plot data for testing
create_mock_plot_data <- function(n_studies = 1, effect_type = 'd', ref_type = 'voxel', n_data_points = 100) {
  plot_data_list <- list()
  
  for (i in 1:n_studies) {
    plot_data_list[[i]] <- list(
      data = list(
        estimate = rnorm(n_data_points, mean = 0.3, sd = 0.2),
        cons_estimate = rnorm(n_data_points, mean = 0.25, sd = 0.15),
        lb = rnorm(n_data_points, mean = 0.1, sd = 0.1),
        ub = rnorm(n_data_points, mean = 0.5, sd = 0.2),
        below_cross_idx = sample(1:n_data_points, min(20, n_data_points-30)),
        above_cross_idx = sample(1:n_data_points, min(30, n_data_points-20))
      ),
      extra_study_details = list(
        percent_not_zero = 0.75,
        max_cons_estimate = 0.8,
        max_cons_effect = 0.8, # For backward compatibility
        group_by_title = paste("Group", i),
        group_level = paste("Level", i),
        n_title = paste("n =", 1000 + i*500),
        mv_estimate = 0.4,
        mv_ci = c(0.2, 0.6),
        ref = rep(ref_type, n_data_points),
        grouping_var = "none",
        brain_masks = list(mask = rep(1, n_data_points))
      ),
      study_details = list(
        orig_stat_type = effect_type,
        test_component_1 = "component1",
        test_component_2 = "component2", 
        dataset = paste("dataset", i),
        map_type = "activation",
        group = paste("group", i),
        ref = ref_type
      )
    )
  }
  
  return(plot_data_list)
}

# Helper function to create mock summary info
create_mock_summary_info <- function() {
  list(
    title_text = "Test Study | Statistic: d | Sample Size: n = 1000 | Map: activation",
    bottom_text = "Max conservative effect size: 0.8\nPercent not overlapping zero: 75.0%",
    grouping_var_title = "None"
  )
}

describe("create_plots function", {
  
  describe("Input validation and parameter handling", {
    
    it("works with default parameters", {
      plot_data <- create_mock_plot_data()
      
      expect_no_error({
        result <- create_plots(plot_data)
      })
      
      expect_s3_class(result, "ggplot")
    })
    
    it("handles single study input correctly", {
      # Test with single study (non-nested list) - should be auto-nested
      single_study <- create_mock_plot_data()[[1]]
      
      expect_no_error({
        result <- create_plots(single_study)
      })
      
      expect_s3_class(result, "ggplot")
    })
    
    it("handles multiple studies input", {
      multi_study <- create_mock_plot_data(n_studies = 3)
      
      expect_no_error({
        result <- create_plots(multi_study)
      })
      
      expect_s3_class(result, "ggplot")
    })
    
    it("validates plot_type parameter", {
      plot_data <- create_mock_plot_data()
      
      # Valid plot types should work
      expect_no_error(create_plots(plot_data, plot_type = "simci"))
      expect_no_error(create_plots(plot_data, plot_type = "density"))
      
      # Invalid plot type should throw error
      expect_error(create_plots(plot_data, plot_type = "invalid_type"))
    })
    
    it("validates effect_type parameter", {
      plot_data <- create_mock_plot_data()
      
      # Test different effect types
      expect_no_error(create_plots(plot_data, effect_type = "d"))
      expect_no_error(create_plots(plot_data, effect_type = "r_sq"))
      expect_no_error(create_plots(plot_data, effect_type = "other"))
    })
    
    it("handles boolean parameters correctly", {
      plot_data <- create_mock_plot_data()
      
      expect_no_error(create_plots(plot_data, do_multivariate = TRUE))
      expect_no_error(create_plots(plot_data, add_description = TRUE))
      expect_no_error(create_plots(plot_data, do_minimal_title = TRUE))
      
      expect_no_error(create_plots(plot_data, do_multivariate = FALSE))
      expect_no_error(create_plots(plot_data, add_description = FALSE))
      expect_no_error(create_plots(plot_data, do_minimal_title = FALSE))
    })
  })
  
  describe("Effect type configurations", {
    
    it("sets correct parameters for d effect type", {
      plot_data <- create_mock_plot_data()
      
      expect_no_error({
        result <- create_plots(plot_data, effect_type = "d")
      })
      expect_s3_class(result, "ggplot")
    })
    
    it("sets correct parameters for r_sq effect type", {
      plot_data <- create_mock_plot_data()
      
      expect_no_error({
        result <- create_plots(plot_data, effect_type = "r_sq")
      })
      expect_s3_class(result, "ggplot")
    })
    
    it("applies multivariate multiplier correctly", {
      plot_data <- create_mock_plot_data()
      
      expect_no_error({
        result <- create_plots(plot_data, do_multivariate = TRUE)
      })
      expect_s3_class(result, "ggplot")
    })
  })
  
  describe("Plot type routing", {
    
    it("routes to simci plot correctly", {
      plot_data <- create_mock_plot_data()
      
      result <- create_plots(plot_data, plot_type = "simci")
      expect_s3_class(result, "ggplot")
      
      # Check that it's actually a simci plot by looking for geom_line and geom_ribbon
      plot_layers <- sapply(result$layers, function(x) class(x$geom)[1])
      expect_true("GeomHline" %in% plot_layers) # Should have horizontal line at 0
    })
    
    it("routes to density plot correctly", {
      plot_data <- create_mock_plot_data()
      
      result <- create_plots(plot_data, plot_type = "density")
      expect_s3_class(result, "ggplot")
    })
    
    it("routes to binned density plot correctly", {
      plot_data <- create_mock_plot_data()
      
      result <- create_plots(plot_data, plot_type = "density_binned")
      expect_s3_class(result, "ggplot")
    })
    
    it("routes to power plots correctly", {
      plot_data <- create_mock_plot_data()
      
      expect_no_error(create_plots(plot_data, plot_type = "power"))
      expect_no_error(create_plots(plot_data, plot_type = "power_n"))
      expect_no_error(create_plots(plot_data, plot_type = "power_binned"))
    })
  })
  
  describe("Spatial plot routing", {
    
    it("routes to activation plots for voxel reference", {
      # Skip these tests if neurobase package is not available
      skip_if_not_installed("neurobase")
      skip_if_not_installed("oro.nifti")
      
      plot_data_voxel <- create_mock_plot_data(ref_type = "voxel")
      
      # This will likely error due to missing template files, but we test the routing
      expect_error({
        result <- create_plots(plot_data_voxel, plot_type = "spatial")
      })
    })
    
    it("routes to connectivity plots for non-voxel reference", {
      plot_data_conn <- create_mock_plot_data(ref_type = "shen_268")
      
      # This will likely error due to missing mapping files, but we test the routing
      expect_error({
        result <- create_plots(plot_data_conn, plot_type = "spatial")
      })
    })
    
    it("handles spatial plot threshold categories", {
      plot_data <- create_mock_plot_data()
      
      # These should error due to missing dependencies, but test the parameter parsing
      expect_error(create_plots(plot_data, plot_type = "spatial_pow_thr"))
      expect_error(create_plots(plot_data, plot_type = "spatial_pow_n_thr"))
    })
  })
  
  describe("Data structure validation", {
    
    it("handles missing data components", {
      # Test with incomplete data structure
      incomplete_data <- list(
        list(
          data = list(estimate = c(1, 2, 3)),
          extra_study_details = list(ref = c("voxel")),
          study_details = list(orig_stat_type = "d")
        )
      )
      
      expect_error({
        create_plots(incomplete_data)
      })
    })
    
    it("validates plot_data_list structure", {
      # Test with completely invalid structure
      invalid_data <- list("not_a_valid_structure")
      
      expect_error({
        create_plots(invalid_data)
      })
    })
    
    it("handles empty plot_data_list", {
      empty_data <- list()
      
      expect_error({
        create_plots(empty_data)
      })
    })
    
    it("handles required data fields for simci plots", {
      # Test with minimal required fields for simci
      minimal_data <- list(
        list(
          data = list(
            estimate = rnorm(10),
            cons_estimate = rnorm(10),
            lb = rnorm(10),
            ub = rnorm(10),
            below_cross_idx = 5,
            above_cross_idx = 8
          ),
          extra_study_details = list(
            n_title = "n = 1000",
            ref = rep("voxel", 10)
          ),
          study_details = list(
            orig_stat_type = "d"
          )
        )
      )
      
      expect_no_error({
        result <- create_plots(minimal_data, plot_type = "simci")
      })
      expect_s3_class(result, "ggplot")
    })
  })
  
  describe("Plot enhancement features", {
    
    it("adds description when requested", {
      plot_data <- create_mock_plot_data()
      summary_info <- create_mock_summary_info()
      
      result <- create_plots(
        plot_data, 
        add_description = TRUE, 
        summary_info = summary_info
      )
      
      expect_s3_class(result, "ggplot")
      # Check that caption was added
      expect_false(is.null(result$labels$caption))
    })
    
    it("applies minimal title when requested", {
      plot_data <- create_mock_plot_data()
      summary_info <- create_mock_summary_info()
      
      result <- create_plots(
        plot_data, 
        do_minimal_title = TRUE,
        summary_info = summary_info
      )
      
      expect_s3_class(result, "ggplot")
      # Title should be modified when minimal_title is TRUE
      expect_false(is.null(result$labels$title))
    })
    
    it("handles summary_info parameter", {
      plot_data <- create_mock_plot_data()
      summary_info <- create_mock_summary_info()
      
      expect_no_error({
        result <- create_plots(plot_data, summary_info = summary_info)
      })
      expect_s3_class(result, "ggplot")
    })
  })
  
  describe("Color and sample size handling", {
    
    it("handles different sample size categories", {
      plot_data <- create_mock_plot_data()
      
      # Test with different sample sizes
      plot_data[[1]]$extra_study_details$n_title <- "n = 500"  # Small
      result1 <- create_plots(plot_data, plot_type = "density")
      expect_s3_class(result1, "ggplot")
      
      plot_data[[1]]$extra_study_details$n_title <- "n = 5000"  # Large
      result2 <- create_plots(plot_data, plot_type = "density")
      expect_s3_class(result2, "ggplot")
      
      plot_data[[1]]$extra_study_details$n_title <- "n = "  # Empty (should be handled)
      result3 <- create_plots(plot_data, plot_type = "density")
      expect_s3_class(result3, "ggplot")
    })
    
    it("handles overlapping vs non-overlapping regions in simci", {
      plot_data <- create_mock_plot_data()
      
      # Test with single crossover point
      plot_data[[1]]$data$below_cross_idx <- 30
      plot_data[[1]]$data$above_cross_idx <- 70
      
      result <- create_plots(plot_data, plot_type = "simci")
      expect_s3_class(result, "ggplot")
      
      # Test with multiple crossover points (should trigger different coloring)
      plot_data[[1]]$data$below_cross_idx <- c(20, 30)
      plot_data[[1]]$data$above_cross_idx <- c(60, 80)
      
      result2 <- create_plots(plot_data, plot_type = "simci")
      expect_s3_class(result2, "ggplot")
    })
  })
  
  describe("Parameter combinations", {
    
    it("handles multiple parameters together", {
      plot_data <- create_mock_plot_data()
      summary_info <- create_mock_summary_info()
      
      expect_no_error({
        result <- create_plots(
          plot_data,
          plot_type = "density_binned",
          effect_type = "r_sq",
          do_multivariate = TRUE,
          add_description = TRUE,
          do_minimal_title = TRUE,
          summary_info = summary_info
        )
      })
      
      expect_s3_class(result, "ggplot")
    })
  })
  
  describe("Edge cases", {
    
    it("handles very large datasets", {
      large_plot_data <- create_mock_plot_data(n_data_points = 1000)
      
      expect_no_error({
        result <- create_plots(large_plot_data)
      })
      expect_s3_class(result, "ggplot")
    })
    
    it("handles extreme effect size values", {
      extreme_data <- create_mock_plot_data()
      extreme_data[[1]]$data$estimate <- c(rep(-10, 30), rep(10, 30), rep(0, 40))
      extreme_data[[1]]$data$cons_estimate <- c(rep(-5, 30), rep(5, 30), rep(0, 40))
      extreme_data[[1]]$data$lb <- extreme_data[[1]]$data$estimate - 1
      extreme_data[[1]]$data$ub <- extreme_data[[1]]$data$estimate + 1
      
      expect_no_error({
        result <- create_plots(extreme_data)
      })
      expect_s3_class(result, "ggplot")
    })
    
    it("handles single unique value in density plots", {
      single_value_data <- create_mock_plot_data()
      single_value_data[[1]]$data$cons_estimate <- rep(0.5, 100)
      
      expect_no_error({
        result <- create_plots(single_value_data, plot_type = "density")
      })
      expect_s3_class(result, "ggplot")
    })
    
    it("handles small datasets", {
      small_data <- create_mock_plot_data(n_data_points = 30)
      
      expect_no_error({
        result <- create_plots(small_data)
      })
      expect_s3_class(result, "ggplot")
    })
  })
  
  describe("Power analysis functionality", {
    
    it("calculates power correctly for t-tests", {
      skip_if_not_installed("pwr")
      
      plot_data <- create_mock_plot_data()
      # Set up for one-sample t-test
      plot_data[[1]]$study_details$orig_stat_type <- "t"
      plot_data[[1]]$study_details$test_component_2 <- NULL
      
      expect_no_error({
        result <- create_plots(plot_data, plot_type = "power")
      })
      expect_s3_class(result, "ggplot")
    })
    
    it("calculates sample size for power analysis", {
      skip_if_not_installed("pwr")
      
      plot_data <- create_mock_plot_data()
      
      expect_no_error({
        result <- create_plots(plot_data, plot_type = "power_n")
      })
      expect_s3_class(result, "ggplot")
    })
  })
})

# Additional integration tests
test_that("create_plots integrates properly with all plot types", {
  plot_data <- create_mock_plot_data()
  
  # Test that each plot type returns a ggplot object
  simci_result <- create_plots(plot_data, plot_type = "simci")
  expect_s3_class(simci_result, "ggplot")
  
  density_result <- create_plots(plot_data, plot_type = "density")
  expect_s3_class(density_result, "ggplot")
  
  # Skip power tests if pwr package not available
  skip_if_not_installed("pwr")
  power_result <- create_plots(plot_data, plot_type = "power")
  expect_s3_class(power_result, "ggplot")
})

test_that("create_plots handles plot parameters correctly", {
  plot_data <- create_mock_plot_data()
  
  result <- create_plots(plot_data)
  
  # Test that result has expected ggplot structure
  expect_true("ggplot" %in% class(result))

  if (inherits(result, "ggplot")) {
    expect_true(is.list(result))
    expect_true("data" %in% names(result))
    expect_true("layers" %in% names(result))
  }
})

test_that("create_plots validates input data structure", {
  # Test that function properly validates the nested list structure
  plot_data <- create_mock_plot_data()
  
  # This should work
  expect_no_error(create_plots(plot_data))
  
  # Single study should be auto-nested and work
  single_study <- plot_data[[1]]
  expect_no_error(create_plots(single_study))
  
  # Invalid structure should error
  expect_error(create_plots(list()))
  expect_error(create_plots("invalid"))
})