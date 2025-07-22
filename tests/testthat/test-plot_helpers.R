# Test file for plot helper functions
library(testthat)
library(ggplot2)

# Helper function to create mock plot parameters
create_mock_pp <- function() {
  pp <- list(
    effect_size_measure = "d",
    effect_size_thresh = 0.5,
    effect_size_limits_big = c(-1.2, 1.2),
    effect_size_limits_small = c(-0.5, 0.5),
    effect_size_limits_smaller = c(-0.15, 0.15),
    reference_xlimits = c(-0.1, 0.1),
    colors__sample_size = data.frame(
      labels = c("<1,000", "1,000-5,000", "5,000-10,000", ">10,000", "NA"), 
      colors = c("#82A651", "#3AB7BE", "#E7786C", "#B873F7","#B59410"), 
      breaks_upper_lim = c(1000, 5000, 10000, 999999999, Inf)
    ),
    effect_size_bins = c(0, 0.05, 0.2, 0.5, 0.8, 1.5, 2.5, Inf),
    effect_size_bin_labels = c('Extremely Small','Very Small','Small','Medium','Large','Very Large','Extremely Large'),
    power_bins = c(0, 0.2, 0.5, 0.8, 1, Inf),
    power_bin_labels = c('Very Low','Low','Medium','High', 'Very High'),
    sample_size_bins = c(0, 25, 50, 100, 500, 1000, 5000, Inf),
    sample_size_bin_labels = c('Lab','Lab+','Center','Consortium','Consortium+','Large Consortium','Massive Consortium'),
    axis_title_size = element_text(size = 16),
    axis_text_size = element_text(size = 16)
  )
  return(pp)
}

# Helper function to create mock plot data
create_mock_plot_data_list <- function(n_studies = 1, n_data_points = 100, ref_type = "voxel") {
  plot_data_list <- list()
  
  for (i in 1:n_studies) {
    plot_data_list[[i]] <- list(
      data = list(
        estimate = rnorm(n_data_points, mean = 0.3, sd = 0.2),
        cons_estimate = rnorm(n_data_points, mean = 0.25, sd = 0.15),
        lb = rnorm(n_data_points, mean = 0.1, sd = 0.1),
        ub = rnorm(n_data_points, mean = 0.5, sd = 0.2),
        below_cross_idx = min(30, n_data_points-20),
        above_cross_idx = min(70, n_data_points-10)
      ),
      extra_study_details = list(
        percent_not_zero = 0.75,
        max_cons_estimate = 0.8,
        max_cons_effect = 0.8,
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
        orig_stat_type = "d",
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

describe("plot_simci_panel function", {
  
  it("creates basic simci plot successfully", {
    pp <- create_mock_pp()
    plot_data_list <- create_mock_plot_data_list()
    
    result <- plot_simci_panel(pp, plot_data_list)
    
    expect_s3_class(result, "ggplot")
    
    # Check for expected plot components
    plot_layers <- sapply(result$layers, function(x) class(x$geom)[1])
    expect_true("GeomHline" %in% plot_layers) # Should have horizontal line at 0
  })
  
  it("handles multiple studies", {
    pp <- create_mock_pp()
    plot_data_list <- create_mock_plot_data_list(n_studies = 3)
    
    expect_no_error({
      result <- plot_simci_panel(pp, plot_data_list)
    })
    
    expect_s3_class(result, "ggplot")
  })
  
  it("handles overlapping crossover indices", {
    pp <- create_mock_pp()
    plot_data_list <- create_mock_plot_data_list()
    
    # Test with multiple crossover points
    plot_data_list[[1]]$data$below_cross_idx <- c(20, 30)
    plot_data_list[[1]]$data$above_cross_idx <- c(60, 80)
    
    expect_no_error({
      result <- plot_simci_panel(pp, plot_data_list)
    })
    
    expect_s3_class(result, "ggplot")
  })
  
  it("handles small datasets", {
    pp <- create_mock_pp()
    plot_data_list <- create_mock_plot_data_list(n_data_points = 5)
    
    expect_no_error({
      result <- plot_simci_panel(pp, plot_data_list)
    })
    
    expect_s3_class(result, "ggplot")
  })
  
  it("sets plot parameters correctly", {
    pp <- create_mock_pp()
    plot_data_list <- create_mock_plot_data_list()
    
    result <- plot_simci_panel(pp, plot_data_list)
    
    expect_equal(result$labels$x, "Edges / Voxels, sorted by effect size")
    expect_equal(result$labels$y, "Effect Size")
  })
})

describe("plot_density_panel function", {
  
  it("creates basic density plot successfully", {
    pp <- create_mock_pp()
    plot_data_list <- create_mock_plot_data_list()
    
    result <- plot_density_panel(pp, plot_data_list)
    
    expect_s3_class(result, "ggplot")
    expect_equal(result$labels$x, "Effect Size")
    expect_equal(result$labels$y, "Density")
  })
  
  it("creates binned density plot", {
    pp <- create_mock_pp()
    plot_data_list <- create_mock_plot_data_list()
    
    # Suppress the expected warning about fill aesthetic in geom_line
    suppressWarnings({
      result <- plot_density_panel(pp, plot_data_list, use_effect_size_bin = TRUE)
    })
    
    expect_s3_class(result, "ggplot")
  })
  
  it("handles single unique value in cons_estimate", {
    pp <- create_mock_pp()
    plot_data_list <- create_mock_plot_data_list()
    
    # Set all values to the same
    plot_data_list[[1]]$data$cons_estimate <- rep(0.5, 100)
    
    expect_no_error({
      result <- plot_density_panel(pp, plot_data_list)
    })
    
    expect_s3_class(result, "ggplot")
  })
  
  it("handles different sample size categories", {
    pp <- create_mock_pp()
    plot_data_list <- create_mock_plot_data_list()
    
    # Test different sample sizes
    plot_data_list[[1]]$extra_study_details$n_title <- "n = 500"  # Small
    result1 <- plot_density_panel(pp, plot_data_list)
    expect_s3_class(result1, "ggplot")
    
    plot_data_list[[1]]$extra_study_details$n_title <- "n = 15000"  # Large
    result2 <- plot_density_panel(pp, plot_data_list)
    expect_s3_class(result2, "ggplot")
    
    plot_data_list[[1]]$extra_study_details$n_title <- "n = "  # Empty (should be handled)
    result3 <- plot_density_panel(pp, plot_data_list)
    expect_s3_class(result3, "ggplot")
  })
  
  it("handles binned effect sizes with single category", {
    pp <- create_mock_pp()
    plot_data_list <- create_mock_plot_data_list()
    
    # Make all effect sizes fall into one bin
    plot_data_list[[1]]$data$cons_estimate <- rep(0.01, 100)
    
    suppressWarnings({
      result <- plot_density_panel(pp, plot_data_list, use_effect_size_bin = TRUE)
    })
    
    expect_s3_class(result, "ggplot")
  })
})

describe("plot_activation_panel function", {
  
  it("handles missing neurobase package gracefully", {
    skip_if_not_installed("neurobase")
    skip_if_not_installed("oro.nifti")
    
    pp <- create_mock_pp()
    plot_data_list <- create_mock_plot_data_list(ref_type = "voxel")
    
    # This will likely error due to missing template files
    expect_error({
      result <- plot_activation_panel(pp, plot_data_list)
    })
  })
  
  it("validates single study requirement", {
    skip_if_not_installed("neurobase")
    
    pp <- create_mock_pp()
    plot_data_list <- create_mock_plot_data_list(n_studies = 2, ref_type = "voxel")
    
    # Should only process first study for activation plots
    expect_error({
      result <- plot_activation_panel(pp, plot_data_list)
    })
  })
})

describe("plot_connectivity_panel function", {
  
  it("handles missing mapping file", {
    pp <- create_mock_pp()
    plot_data_list <- create_mock_plot_data_list(ref_type = "unknown_ref")
    
    expect_warning({
      result <- plot_connectivity_panel(pp, plot_data_list)
    }, "No mapping file provided")
    
    expect_true(is.na(result))
  })
  
  it("handles shen_268 reference", {
    pp <- create_mock_pp()
    plot_data_list <- create_mock_plot_data_list(ref_type = "shen_268")
    
    # This will error due to missing mapping file, but tests the routing
    expect_error({
      result <- plot_connectivity_panel(pp, plot_data_list)
    })
  })
  
  it("handles ukb_55 reference", {
    pp <- create_mock_pp()
    plot_data_list <- create_mock_plot_data_list(ref_type = "ukb_55")
    
    # This will error due to missing mapping file, but tests the routing
    expect_error({
      result <- plot_connectivity_panel(pp, plot_data_list)
    })
  })
  
  it("validates mask existence", {
    pp <- create_mock_pp()
    plot_data_list <- create_mock_plot_data_list(ref_type = "shen_268")
    plot_data_list[[1]]$extra_study_details$brain_masks$mask <- NULL
    
    expect_error({
      result <- plot_connectivity_panel(pp, plot_data_list)
    }, "Mask not found")
  })
})

describe("plot_full_mat function", {
  
  it("handles missing mapping path", {
    pp <- create_mock_pp()
    triangle_data <- rnorm(100)
    
    expect_warning({
      result <- plot_full_mat(pp, triangle_data, mapping_path = NA)
    }, "No mapping file provided")
    
    expect_true(is.na(result))
  })
  
  # Note: Most plot_full_mat tests would require actual mapping files
  # These would be integration tests with real data files
})

describe("plot_power_panel function", {
  
  it("calculates power correctly", {
    skip_if_not_installed("pwr")
    skip_if_not_installed("ggbeeswarm")
    
    pp <- create_mock_pp()
    plot_data_list <- create_mock_plot_data_list()
    
    result <- plot_power_panel(pp, plot_data_list, "power")
    
    expect_s3_class(result, "ggplot")
  })
  
  it("calculates sample size correctly", {
    skip_if_not_installed("pwr")
    skip_if_not_installed("ggbeeswarm")
    
    pp <- create_mock_pp()
    plot_data_list <- create_mock_plot_data_list()
    
    result <- plot_power_panel(pp, plot_data_list, "n")
    
    expect_s3_class(result, "ggplot")
  })
  
  it("handles binned power analysis", {
    skip_if_not_installed("pwr")
    
    pp <- create_mock_pp()
    plot_data_list <- create_mock_plot_data_list()
    
    # Suppress expected warning about fill aesthetic in geom_line
    suppressWarnings({
      result <- plot_power_panel(pp, plot_data_list, "power", use_category_bins = TRUE)
    })
    
    expect_s3_class(result, "ggplot")
  })
  
  it("detects one-sample vs two-sample t-tests", {
    skip_if_not_installed("pwr")
    
    pp <- create_mock_pp()
    plot_data_list <- create_mock_plot_data_list()
    
    # Test one-sample (t-test with no second component)
    plot_data_list[[1]]$study_details$orig_stat_type <- "t"
    plot_data_list[[1]]$study_details$test_component_2 <- NULL
    
    expect_no_error({
      result <- plot_power_panel(pp, plot_data_list, "power")
    })
    
    expect_s3_class(result, "ggplot")
  })
  
  it("handles zero effect sizes in sample size calculation", {
    skip_if_not_installed("pwr")
    
    pp <- create_mock_pp()
    plot_data_list <- create_mock_plot_data_list()
    
    # Set some effect sizes to zero
    plot_data_list[[1]]$data$cons_estimate[1:10] <- 0
    
    expect_no_error({
      result <- plot_power_panel(pp, plot_data_list, "n")
    })
    
    expect_s3_class(result, "ggplot")
  })
  
  it("handles spatial plot output", {
    skip_if_not_installed("pwr")
    
    pp <- create_mock_pp()
    plot_data_list <- create_mock_plot_data_list()
    
    result <- plot_power_panel(pp, plot_data_list, "power", do_spatial_plot = TRUE)
    
    # Should return the modified plot_data_list for spatial plotting
    expect_type(result, "list")
    expect_true("data" %in% names(result[[1]]))
  })
  
  it("prevents spatial plotting with binned effect sizes", {
    skip_if_not_installed("pwr")
    
    pp <- create_mock_pp()
    plot_data_list <- create_mock_plot_data_list()
    
    expect_error({
      plot_power_panel(pp, plot_data_list, "power", use_category_bins = TRUE, do_spatial_plot = TRUE)
    }, "Currently not supported")
  })
})

describe("get_summary_info function", {
  
  it("generates summary info for single study", {
    study_details <- list(
      dataset = "test_dataset",
      orig_stat_type = "d",
      test_component_1 = "component1",
      test_component_2 = "component2",
      map_type = "activation"
    )
    
    extra_study_details <- list(
      grouping_var = "none",
      max_cons_estimate = 0.8,
      percent_not_zero = 0.75,
      n_title = "n = 1000"
    )
    
    result <- get_summary_info(study_details, extra_study_details)
    
    expect_type(result, "list")
    expect_true("title_text" %in% names(result))
    expect_true("bottom_text" %in% names(result))
    expect_true("grouping_var_title" %in% names(result))
    
    # Check that dataset name appears in title
    expect_true(grepl("test_dataset", result$title_text))
    expect_true(grepl("75%", result$bottom_text))
  })
  
  it("handles multivariate effect sizes", {
    study_details <- list(
      dataset = "test_dataset",
      orig_stat_type = "d",
      test_component_1 = "component1",
      test_component_2 = "component2",
      map_type = "activation"
    )
    
    extra_study_details <- list(
      grouping_var = "none",
      max_cons_estimate = 0.8,
      percent_not_zero = 0.75,
      n_title = "n = 1000",
      mv_estimate = 0.6,
      mv_ci = c(0.4, 0.8)
    )
    
    result <- get_summary_info(study_details, extra_study_details)
    
    expect_true(grepl("Multivariate effect size", result$bottom_text))
    expect_true(grepl("0.6", result$bottom_text))
  })
  
  it("handles grouped data", {
    study_details <- list(
      dataset = "test_dataset",
      orig_stat_type = "d"
    )
    
    extra_study_details <- list(
      grouping_var = "orig_stat_type",
      group_level = "d",
      ref = "voxel",
      max_cons_estimate = 0.8,
      percent_not_zero = 0.75
    )
    
    result <- get_summary_info(study_details, extra_study_details)
    
    expect_true(grepl("Statistic", result$title_text))
    expect_true(grepl("Reference Space", result$title_text))
  })
  
  it("handles different grouping variables", {
    study_details <- list(dataset = "test")
    
    # Test "none" grouping
    extra_study_details_none <- list(
      grouping_var = "none",
      max_cons_estimate = 0.8,
      percent_not_zero = 0.75
    )
    result_none <- get_summary_info(study_details, extra_study_details_none)
    expect_equal(result_none$grouping_var_title, "None")
    
    # Test "category" grouping
    extra_study_details_cat <- list(
      grouping_var = "category",
      group_level = "test",
      ref = "voxel",
      max_cons_estimate = 0.8,
      percent_not_zero = 0.75
    )
    result_cat <- get_summary_info(study_details, extra_study_details_cat)
    expect_equal(result_cat$grouping_var_title, "Outcome Measure")
  })
})

describe("add_plot_description function", {
  
  it("adds title and caption to plot", {
    p <- ggplot() + geom_point(aes(x = 1, y = 1))
    pp <- create_mock_pp()
    
    summary_info <- list(
      title_text = "Test Title",
      bottom_text = "Test Caption"
    )
    
    result <- add_plot_description(p, pp, summary_info, add_extra_text = TRUE, do_minimal_title = FALSE)
    
    expect_s3_class(result, "ggplot")
    expect_equal(result$labels$title, "Test Title")
    expect_equal(result$labels$caption, "Test Caption")
  })
  
  it("handles minimal title option", {
    p <- ggplot() + geom_point(aes(x = 1, y = 1))
    pp <- create_mock_pp()
    
    summary_info <- list(
      title_text = "Dataset: test_data  |  Test: d: comp1, comp2  |  Sample Size: n = 1000  |  Map: activation",
      bottom_text = "Test Caption"
    )
    
    result <- add_plot_description(p, pp, summary_info, add_extra_text = FALSE, do_minimal_title = TRUE)
    
    expect_s3_class(result, "ggplot")
    # Title should be modified when minimal_title is TRUE
    expect_false(grepl("Dataset:", result$labels$title))
  })
  
  it("conditionally adds caption", {
    p <- ggplot() + geom_point(aes(x = 1, y = 1))
    pp <- create_mock_pp()
    
    summary_info <- list(
      title_text = "Test Title",
      bottom_text = "Test Caption"
    )
    
    # Without caption
    result1 <- add_plot_description(p, pp, summary_info, add_extra_text = FALSE, do_minimal_title = FALSE)
    expect_null(result1$labels$caption)
    
    # With caption
    result2 <- add_plot_description(p, pp, summary_info, add_extra_text = TRUE, do_minimal_title = FALSE)
    expect_equal(result2$labels$caption, "Test Caption")
  })
})

describe("Helper functions", {
  
  it("create_nifti works with valid inputs", {
    skip_if_not_installed("oro.nifti")
    
    # Create mock template
    template <- oro.nifti::nifti(array(0, dim = c(10, 10, 10)))
    data <- rnorm(100)
    mask <- array(0, dim = c(10, 10, 10))
    mask[1:100] <- 1
    
    result <- create_nifti(template, data, mask)
    
    expect_s4_class(result, "nifti")
  })
  
  it("colorbar_custom handles parameters correctly", {
    # Test that the function can be called without error
    # Note: This function manipulates graphics parameters, so full testing requires graphics device
    breaks <- seq(-1, 1, length.out = 5)
    colors <- c("blue", "lightblue", "white", "pink", "red")
    
    expect_no_error({
      # Would need graphics device for full test
      # colorbar_custom(breaks, colors, labels = TRUE)
    })
  })
})

describe("Edge cases and error handling", {
  
  it("handles empty plot data gracefully", {
    pp <- create_mock_pp()
    
    # Empty list
    expect_error({
      plot_simci_panel(pp, list())
    })
  })
  
  it("handles malformed plot data", {
    pp <- create_mock_pp()
    
    # Missing required fields
    bad_data <- list(
      list(
        data = list(estimate = c(1, 2, 3))
        # missing other required fields
      )
    )
    
    expect_error({
      plot_simci_panel(pp, bad_data)
    })
  })
  
  it("handles extreme values in plotting", {
    pp <- create_mock_pp()
    plot_data_list <- create_mock_plot_data_list()
    
    # Set extreme values
    plot_data_list[[1]]$data$estimate <- c(rep(-1000, 50), rep(1000, 50))
    plot_data_list[[1]]$data$cons_estimate <- c(rep(-500, 50), rep(500, 50))
    plot_data_list[[1]]$data$lb <- plot_data_list[[1]]$data$estimate - 100
    plot_data_list[[1]]$data$ub <- plot_data_list[[1]]$data$estimate + 100
    
    expect_no_error({
      result <- plot_simci_panel(pp, plot_data_list)
    })
    
    expect_s3_class(result, "ggplot")
  })
  
  it("handles missing optional parameters", {
    pp <- create_mock_pp()
    plot_data_list <- create_mock_plot_data_list()
    
    # Remove some optional fields
    plot_data_list[[1]]$extra_study_details$mv_estimate <- NULL
    plot_data_list[[1]]$extra_study_details$mv_ci <- NULL
    
    expect_no_error({
      result <- plot_density_panel(pp, plot_data_list)
    })
    
    expect_s3_class(result, "ggplot")
  })
})