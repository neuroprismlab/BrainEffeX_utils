#########################################################################
# Functions for plotting
# These functions describe different effect sizes (Cohen's d or R-squared) and simultaneous confidence intervals (CIs)
# for a given dataset. It allows optional grouping, visualization, and file saving.

#########################################################################
# Density & Simultaneous CIs

#' Function: Plot Simultaeous CIs
#'
#' @param pp A list of plot parameters
#' @param plot_data_list A list containing effect size data for plotting:
#'  - `data`: A list containing sorted & downsampled effect size data & helpers: estimate, cons_estimate, lb, ub, below_cross_idx, and above_cross_idx
#'  - `summary_info`: A list containing extra descriptive info: percent_not_zero, max_cons_effect, group_by_title, and n_title
#'  - `mv_data`: A list containing multivariate effect size data: estimate, lb, and ub
#'  - `study_details`: A list of original study details: orig_stat_type, test_component_1, test_component_2, dataset, map_type, group, and ref
#'
#' @return A ggplot object containing one or overlapping densities
#' @export
#'
#' @examples
#' # Example usage
#' # plot_simci_panel(plot_data)
plot_simci_panel <- function(pp, plot_data_list) {

  # add simci-specific plot params
  pp$non_overlap_colors <- "#4ECDC4"
  pp$overlap_colors <- "#FF6F61"
  pp$other_overlap_colors <- "#8B859E" 
  # pp$non_overlap_colors <- rgb(177/255, 207/255, 192/255, alpha = 0.5)
  # pp$overlap_colors <- rgb(237/255, 185/255, 185/255, alpha = 0.5)
  pp$alpha_ribbon <- 0.3
  pp$alpha_line <- 1
  pp$intercept_line_color <- "#ba2d25"
  pp$intercept_line_size <- 0.3
  pp$xlabel <- "Edges / Voxels, sorted by effect size"
  pp$ylabel <- "Effect Size"

  # little function to simplify plotting
  add_geom_layers <- function(p, data, color, alpha_line, alpha_ribbon) {
    if (nrow(data) > 1) {
      p <- p +
        geom_line(data = data, aes(x = x, y = estimate), color = color, alpha = alpha_line) +
        geom_ribbon(data = data, aes(x = x, ymin = lb, ymax = ub), fill = color, alpha = alpha_ribbon)
    }
    return(p)
  }

  # make plot object

  p <- ggplot() + geom_hline(yintercept = 0, color = pp$intercept_line_color, linetype = "dashed", size = pp$intercept_line_size)

  for (i in seq_along(plot_data_list)) {

    # extract data from list
    plot_df <- data.frame(
      x = 1:length(plot_data_list[[i]]$data$estimate),
      estimate = plot_data_list[[i]]$data$estimate,
      ub = plot_data_list[[i]]$data$ub,
      lb = plot_data_list[[i]]$data$lb
    )
    below_cross_idx <- plot_data_list[[i]]$data$below_cross_idx
    above_cross_idx <- plot_data_list[[i]]$data$above_cross_idx

    # set y limits
    # if (max(abs(c(plot_df$lb,plot_df$ub))) > pp$effect_size_thresh) {
      pp$ylim = pp$effect_size_limits_big
    # } else {
    #   pp$ylim = pp$effect_size_limits_small
    # }

    # TODO: temporary hack for R^2 limits
    # if ((max(plot_df$estimate,na.rm = TRUE) <= 1.1) && (min(plot_df$estimate,na.rm = TRUE) >= -0.1)) {
    #   pp$ylim = pp$rsq_effect_size_limits
    # }

    # plot

      if ((length(below_cross_idx) > 1) || (length(above_cross_idx) > 1)) {
        # IN PROGRESS: plot everything below below_cross_idx[[1]] in grey
        p <- add_geom_layers(p, plot_df, pp$other_overlap_colors, pp$alpha_line, pp$alpha_ribbon)
      } else {
        p <- add_geom_layers(p, subset(plot_df, x <= below_cross_idx), pp$non_overlap_colors, pp$alpha_line, pp$alpha_ribbon)
        p <- add_geom_layers(p, subset(plot_df, x >= below_cross_idx & x <= above_cross_idx), pp$overlap_colors, pp$alpha_line, pp$alpha_ribbon)
        p <- add_geom_layers(p, subset(plot_df, x >= above_cross_idx), pp$non_overlap_colors, pp$alpha_line, pp$alpha_ribbon)
      }
  }

  p <- p + labs(x = pp$xlabel, y = pp$ylabel) +
    # scale_y_continuous(limits = pp$ylim) +
    coord_cartesian(ylim = pp$ylim) +
    theme_classic() +
    theme(axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.text.y = pp$axis_text_size, axis.title = pp$axis_title_size)

  return(p)
}


#' Function: Plot Densities
#'
#' @param pp A list of plot parameters
#' @param plot_data_list A list containing effect size data for plotting:
#'  - `data`: A list containing sorted & downsampled effect size data & helpers: estimate, cons_estimate, lb, ub, below_cross_idx, and above_cross_idx
#'  - `summary_info`: A list containing extra descriptive info: percent_not_zero, max_cons_effect, group_by_title, and n_title
#'  - `mv_data`: A list containing multivariate effect size data: estimate, lb, and ub
#'  - `study_details`: A list of original study details: orig_stat_type, test_component_1, test_component_2, dataset, map_type, group, and ref
#'
#' @return A ggplot object containing one or overlapping densities
#' @export
#'
#' @examples
#' # Example usage
#' # plot_density_panel(plot_data)
plot_density_panel <- function(pp, plot_data_list, use_effect_size_bin = FALSE) {

  # add density-specific plot params

  # pp$xlim <- c(-0.15, 0.15)
  pp$alpha <- 0.1
  pp$size <- 0.4
  pp$hist_bin_width <- 0.001 # when there are all 0's, we want a very thin hist instead of density
  pp$xlabel <- "Effect Size"
  pp$ylabel <- "Density"

  # make plot object

  p <- ggplot()
  
  # add grey rectangle in background for reference
  # p <- p + geom_rect(aes(xmin = pp$reference_xlimits[1], xmax = pp$reference_xlimits[2], ymin = 0, ymax = Inf), fill = "grey", alpha = 0.2)
  
  # if (use_effect_size_bin) {
  # if plot data list is > 1 entry, set x limits to small
  # if (length(plot_data_list) > 1) {
    # pp$xlim = pp$effect_size_limits_smaller # small for overlapping
  # } else if (length(plot_data_list) == 1) {
  #   if (plot_data_list[[1]]$extra_study_details$n_title == "NA") {
      pp$xlim = pp$effect_size_limits_smaller # very small for meta
  #   } else {
      # pp$xlim = pp$effect_size_limits_big # big for individual studies
  #   }
  # }
  # } else {
  #   pp$xlim <- pp$binned_effect_size_limits
  # }
      
  # TODO: temporary hack for R^2 limits
  # if ((max(plot_data_list[[1]]$data$estimate,na.rm = TRUE) <= 1.1) && (min(plot_data_list[[1]]$data$estimate,na.rm = TRUE) >= -0.1)) {
  #   pp$xlim = pp$rsq_effect_size_limits
  # }

  for (i in seq_along(plot_data_list)) {

    # set x limits
    # if (max(abs(c(plot_data_list[[i]]$data$lb,plot_data_list[[i]]$data$ub))) > pp$effect_size_thresh) {
      # pp$xlim = pp$effect_size_limits_big
    # } else {
      # pp$xlim = pp$effect_size_limits_small
    # }
    
    
    # color plots by binned sample size

    if (plot_data_list[[i]]$extra_study_details$n_title == "n = ") {
      plot_data_list[[i]]$extra_study_details$n_title <- "n = 9999999999" # hack to plot empty sample size in gold
    }

    sample_size_category <- cut(as.numeric(gsub("n = ", "", plot_data_list[[i]]$extra_study_details$n_title)),
                            breaks = c(-Inf, pp$colors__sample_size$breaks_upper_lim),
                            labels = pp$colors__sample_size$labels)

    if (use_effect_size_bin) { # optional: bin effect sizes

      cons_estimate__binned <- as.numeric(cut(abs(plot_data_list[[i]]$data$cons_estimate), breaks = pp$effect_size_bins, right = FALSE))
      
      # manually make counts & normalize
      cons_estimate_counts <- as.data.frame( table(cons_estimate__binned) )
      colnames(cons_estimate_counts) <- c("category", "count")
      cons_estimate_counts$category <- as.numeric(as.character(cons_estimate_counts$category))
      cons_estimate_counts$count <- cons_estimate_counts$count / sum(cons_estimate_counts$count) # normalize counts
      
      # # manually add start and end points at zero
      # start_point <- data.frame(category = min(cons_estimate_counts$category), count = 0 )
      # end_point <- data.frame(category = max(cons_estimate_counts$category), count = 0)
      # cons_estimate_counts <- rbind(start_point, cons_estimate_counts, end_point)
      
      if (length(unique(cons_estimate_counts$category)) == 1) {
        
        p <- p + geom_col(data = data.frame(x = cons_estimate_counts$category, y = cons_estimate_counts$count, sample_size_category = sample_size_category),
                          aes(x = x, y = y, fill = sample_size_category), 
                          width = 0.25, alpha = pp$alpha, color = NA)
        
      } else {
      
        p <- p + 
          geom_area(data = data.frame(x = cons_estimate_counts$category, y = cons_estimate_counts$count, sample_size_category = sample_size_category),
                    aes(x = x, y = y, fill = sample_size_category, color = sample_size_category), 
                    alpha = pp$alpha * 0.5) +
          geom_line(data = data.frame(x = cons_estimate_counts$category, y = cons_estimate_counts$count, sample_size_category = sample_size_category),
                    aes(x = x, y = y, fill = sample_size_category, color = sample_size_category), 
                    size = pp$size, alpha = pp$alpha)
      }
      
      # older options
      
      # p <- p + geom_step(data = data.frame(value = cons_estimate__binned, sample_size_category = sample_size_category),
      #                         aes(x = value, fill = sample_size_category, color = sample_size_category),
      #                         size = pp$size, stat = "bin", binwidth = 1)
      
      # p <- p + geom_histogram(data = data.frame(value = cons_estimate__binned, sample_size_category = sample_size_category),
      #                       aes(x = value, fill = sample_size_category, color = sample_size_category),
      #                       alpha = pp$alpha, size = pp$size, fill = NA, binwidth = 1)
      
      # p <- p + geom_density(data = data.frame(value = cons_estimate__binned, sample_size_category = sample_size_category),
      #                       aes(x = value, fill = sample_size_category, color = sample_size_category),
      #                       bw = 0.5, alpha = pp$alpha, size = pp$size)
      
      # p <- p + geom_bar(data = data.frame(value = cons_estimate__binned, sample_size_category = sample_size_category),
      #                   aes(x = value, fill = sample_size_category, color = sample_size_category), alpha = pp$alpha, size = pp$size)
      
      
    } else {
      

      if (length(unique(plot_data_list[[i]]$data$cons_estimate)) == 1) {
        p <- p + geom_histogram(data = data.frame(value = plot_data_list[[i]]$data$cons_estimate, sample_size_category = sample_size_category),
                                aes(x = value, fill = sample_size_category, color = sample_size_category), alpha = pp$alpha, size = pp$size, binwidth = pp$hist_bin_width)
      } else {
        p <- p + geom_density(data = data.frame(value = plot_data_list[[i]]$data$cons_estimate, sample_size_category = sample_size_category),
                          aes(x = value, fill = sample_size_category, color = sample_size_category), alpha = pp$alpha, size = pp$size) # for unique color per study, do: fill = i, color = i
      }
    }
    
  }

  p <- p + theme_minimal() + labs(x = pp$xlabel, y = pp$ylabel) +
    theme(legend.position = "none", axis.text.y = pp$axis_text_size, axis.text.x = pp$axis_text_size, axis.title = pp$axis_title_size) +
    scale_fill_manual(values = pp$colors__sample_size$colors, breaks = pp$colors__sample_size$labels) +
    scale_color_manual(values = pp$colors__sample_size$colors, breaks = pp$colors__sample_size$labels)

  if (use_effect_size_bin) {
    # p <- p + scale_x_discrete(limits = c(0,(length(pp$effect_size_bins)-1))) +
    p <- p +
      scale_x_continuous(breaks = c(1:(length(pp$effect_size_bins)-1)),
                          labels = pp$effect_size_bin_labels, # replace x ticks with labels
                          # limits = c(0.5,(length(pp$effect_size_bins)-1)),
                          guide = guide_axis(check.overlap = FALSE)) +
      coord_cartesian(xlim = c(0.5, (length(pp$effect_size_bins)-1))) +
      scale_y_continuous(limits = c(0, 1)) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))   # Rotate x-axis labels
      
            
  } else {
    p <- p + xlim(pp$xlim)
  }
  
  return(p)
}


#########################################################################
# Task-Based Activation (Brain) Maps

#' Function: Plot Voxel-level Activation Maps
#'
#' @param pp A list of plot parameters
#' @param plot_data_list A list containing effect size data for plotting:
#'  - `data`: A list containing sorted & downsampled effect size data & helpers: estimate, cons_estimate, lb, ub, below_cross_idx, and above_cross_idx
#'  - `summary_info`: A list containing extra descriptive info: percent_not_zero, max_cons_effect, group_by_title, and n_title
#'  - `mv_data`: A list containing multivariate effect size data: estimate, lb, and ub
#'  - `study_details`: A list of original study details: orig_stat_type, test_component_1, test_component_2, dataset, map_type, group, and ref
#'
#' @return A ggplot object containing one or overlapping densities
#' @export
#'
#' @examples
#' # Example usage
#' # plot_activation_maps(plot_data)
plot_activation_panel <- function(pp, plot_data_list, threshold_category = NA) {

  library(neurobase)
  library(grid)

  # add spatial map-specific plot params

  # In shiny, we would first get this input from user, e.g.,
  # if (!exists("input")) {
  #   pp$xCoord <- input$xCoord
  #   pp$yCoord <- input$yCoord
  #   pp$zCoord <- input$zCoord
  # }

  pp$do_static_figs <- TRUE # TRUE = static figs (for manuscript), FALSE = interactions
  pp$xlabel <- "Effect Size"
  pp$ylabel <- "Spatial Map"
  pp$bg <- 'white'
  pp$text_color <- 'black'
  pp$text_size <- 10
  pp$ncolors <- 40 # also controls number of ticks on colorbar
  # pp$col_y <- colorspace::diverge_hsv(pp$ncol)
  # pp$n_colorbar_ticks <- 5
  pp$ycolorbar <- TRUE
  pp$colorbar_text_size <- 0.9
  pp$mfrow <- c(1,3) #c(3, 1)
  pp$xCoord <- 30
  pp$yCoord <- 30
  pp$zCoord <- 30


  # get template for later data->nifti
  template <- readNIfTI(system.file("data/plotting/template_nifti.nii.gz", package = "BrainEffeX.utils"))  # assumes MNI - TODO: get actual ref

  # make plot object
  # p <- ggplot()

  if (length(plot_data_list) == 1) { # only allowing 1 since would be weird to plot overlapping for this...
    for (i in seq_along(plot_data_list)) {

      # create nifti for this data

      if (!is.na(threshold_category)) {
        # get binary mask and then apply
        data <- plot_power_panel(pp, list(plot_data_list[[i]]), threshold_category, use_category_bins = FALSE, do_spatial_plot = TRUE)
        data <- plot_data_list[[i]]$data$estimate * data[[1]]$data$estimate
        data[is.na(data)] <- 0 # set all non-masked values to 0
      } else {
        data <- plot_data_list[[i]]$data$estimate
      }
      
      if (length(data) == 10) { # need atlas for network-level - currently Shen only - TODO: un-hard-code and pass the actual atlas
        atlas <- readNIfTI(system.file("data/parcellations/shen_2mm_268_parcellation__in_subnetworks.nii.gz", package = "BrainEffeX.utils"))
        for (j in 1:10) {
          atlas@.Data[atlas@.Data == j] <- data[j]
        }
        nii <- atlas
      } else {
        mask <- plot_data_list[[i]]$extra_study_details$brain_masks$mask
        nii <- create_nifti(template, data, mask)
      }
      
      if (all(data == 0)) { # need at least one non-zero voxel or plot won't render
        nii@.Data[1] <- 1
      }

      # set colorbar limits
      if (max(abs(data)) > pp$effect_size_thresh) {
        pp$zlim_range <- pp$effect_size_limits_big
      } else {
        pp$zlim_range <- pp$effect_size_limits_small
      }
      pp$zlim_range <- pp$zlim_range/2
      
      # TODO: temporary hack for R^2 limits
      # if ((max(data,na.rm = TRUE) <= 1.1) && (min(data,na.rm = TRUE) >= -0.1)) {
      #   pp$zlim_range = pp$rsq_effect_size_limits_smaller
      # }

      # hack to show colors above the limit
      nii[nii == 0] <- NA
      nii[nii > pp$zlim_range[2]] <- pp$zlim_range[2]
      nii[nii < pp$zlim_range[1]] <- pp$zlim_range[1]

      # pdf(file = NULL) # don't plot anywhere
      # par(mar = c(1, 1, 1, 4))

      # p <- grid.grabExpr({

      if (pp$do_static_figs) {
        png("ortho_tmp.png", width = 800, height = 800)
      }
      
      # hallee's
      # pp$zlim_range <- c(-1,1)
      # num_breaks = 20
      
      n_breaks <- pp$ncolors
      ybreaks <- seq(pp$zlim_range[1], pp$zlim_range[2], length.out = n_breaks)
      col_y <- colorspace::diverge_hsv(n_breaks-1) # previously pp$col_y
      
      
      ortho2(
        x = nii,
        y = nii,
        crosshairs = FALSE,
        NA.x = TRUE,
        NA.y = FALSE,
        col.y = col_y,
        xyz = c(pp$xCoord, pp$yCoord, pp$zCoord),
        bg = pp$bg,
        text.color = pp$text_color,
        text.cex = pp$text_size,
        # clabels = clabels,
        ybreaks = ybreaks,
        ycolorbar = pp$ycolorbar,
        mfrow = pp$mfrow,
        zlim = pp$zlim_range # TODO: check
      )
      
      n_breaks <- 10 # hallee's
      # n_breaks <- pp$ncol # orig
      ybreaks <- seq(pp$zlim_range[1], pp$zlim_range[2], length.out = n_breaks)
      clabels <- round(seq(pp$zlim_range[1], pp$zlim_range[2], length.out = n_breaks-1), 2)
      col_y <- colorspace::diverge_hsv(n_breaks-1) # previously pp$col_y
      
      colorbar_custom(
        breaks = ybreaks,
        col = col_y,
        labels = clabels,
        text.col = 'black',
        text.size = pp$colorbar_text_size
      )
      
      
      
      if (pp$do_static_figs) {
        dev.off()
      }
      # })

    }
  }

  # TODO: any extra additions
  # p <- p + theme_minimal() + labs(x = pp$xlabel, y = pp$ylabel) +
  #   theme(legend.position = "none")

  if (pp$do_static_figs) {
    p <- ggplot() +
      annotation_custom(rasterGrob(png::readPNG("ortho_tmp.png"), interpolate = TRUE), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
  } else {
    p <- list()
  }

  return(p)
}

# read nifti template file (for hcp studies specifically)
create_nifti_template <- function(sample_nifti_path = 'data/EMOTION_cope3_GroupSize482_dcoeff.nii.gz', out_path = 'data/') {
  template <- readNIfTI(sample_nifti_path, read_data = FALSE)
  writeNIfTI(template, paste0(out_path, 'template_nifti'))
}

# create a nifti file from a template and study name
create_nifti <- function(nifti_template, data, mask) {
  # INPUTS:
  # - nifti_template: loaded nifti file to set all meta-data/other nifti info on
  # - data: 1D data taken out of mask
  # - mask: nD mask used to get data save
  # OUTPUTS:
  # - nifti_template: nifti file that contains the data put back into the mask

  structured <- mask
  structured[structured==1] <- data
  nifti_template@.Data <- structured # nifti_template contains all the metadata of the reference nifti
  return(nifti_template)
}


# custom colorbar function (bc bug in coloring and size of ortho2)
# mostly copied from https://github.com/muschellij2/neurobase/blob/master/R/ortho2.R
# changed text color and added text.size

colorbar_custom <- function(breaks, #the minimum and maximum z values for which 
                            # colors should be plotted (see \code{\link{image}})
                            col, # a list of colors (see \code{\link{image}})
                            text.col = "white", # axis and text label color
                            labels = TRUE,
                            maxleft = 0.95,
                            text.size = 4
){
  # taken from vertical.image.legend from package aqfig
  starting.par.settings <- par(no.readonly = TRUE)
  on.exit({
    par(starting.par.settings)
  })
  mai <- par("mai")
  fin <- par("fin")
  rat = mai[4]/fin[1]
  rat = max(rat, 1 - maxleft)
  x.legend.fig <- c(1 - rat, 1)
  y.legend.fig <- c(mai[1]/fin[2], 1 - (mai[3]/fin[2]))
  x.legend.plt <- c(x.legend.fig[1] + (0.08 * (x.legend.fig[2] - 
                                                 x.legend.fig[1])), 
                    x.legend.fig[2] - (0.6 * (x.legend.fig[2] - 
                                                x.legend.fig[1])))
  y.legend.plt <- y.legend.fig
  cut.pts <- breaks
  z <- (cut.pts[1:length(col)] + cut.pts[2:(length(col) + 1)])/2
  par(new = TRUE, pty = "m", plt = c(x.legend.plt, y.legend.plt))
  image(x = 1, y = z, z = matrix(z, nrow = 1, ncol = length(col)), 
        col = col, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
  if (isTRUE(labels)) {
    at = NULL
  } else {
    at = z
  }
  axis(4, mgp = c(3, 0.2, 0), las = 2, cex.axis = text.size, 
       tcl = -0.1, 
       labels = labels,
       at = at,
       col.axis = text.col,
       col = text.col)
  box()
  mfg.settings <- par()$mfg
  par(mfg = mfg.settings, new = FALSE)
  invisible(NULL)
}

#########################################################################
# Functional Connectivity

#' Function: Plot Connectivity Maps
#'
#' @param pp A list of plot parameters
#' @param plot_data_list A list containing effect size data for plotting:
#'  - `data`: A list containing sorted & downsampled effect size data & helpers: estimate, cons_estimate, lb, ub, below_cross_idx, and above_cross_idx
#'  - `summary_info`: A list containing extra descriptive info: percent_not_zero, max_cons_effect, group_by_title, and n_title
#'  - `mv_data`: A list containing multivariate effect size data: estimate, lb, and ub
#'  - `study_details`: A list of original study details: orig_stat_type, test_component_1, test_component_2, dataset, map_type, group, and ref
#'
#' @return A ggplot object containing one or overlapping densities
#' @export
#'
#' @examples
#' # Example usage
#' # plot_connectivity_panel(plot_data)
plot_connectivity_panel <- function(pp, plot_data_list, threshold_category = NA) {

  library(reshape2)

  # add connectivity map-specific plot params
  # TODO: update with plot params
  # pp$xlabel <- "Effect Size"
  # pp$ylabel <- "Spatial Map"

  # make plot object
  p <- ggplot()

  for (i in seq_along(plot_data_list)) { # TODO: would be weird to plot overlapping for this...

    if (plot_data_list[[i]]$extra_study_details$ref[[1]] == "shen_268"){
      # if (plot_data_list[[i]]$study_details$ref == "net") { # TODO: get this input and finish this for pooling=net
      #   mapping_path <- "inst/data/parcellations/map268_subnetwork.csv"
      #   pooled <- TRUE
      # } else { # unpooled
      mapping_path <-  system.file("data/parcellations/map268_subnetwork.csv", package = "BrainEffeX.utils")
      # }
    } else if (plot_data_list[[i]]$extra_study_details$ref[[1]] == "ukb_55") {
      mapping_path <- system.file("data/parcellations/map55_ukb.csv", package = "BrainEffeX.utils")
    } else {
      mapping_path <- NA
    }

    #       template <- plot_data_list[[i]]$study_details$ref

    if (!is.na(threshold_category)) {
      # get binary mask and then apply
      data <- plot_power_panel(pp, list(plot_data_list[[i]]), threshold_category, use_category_bins = FALSE, do_spatial_plot = TRUE)
      data <- plot_data_list[[i]]$data$estimate * data[[1]]$data$estimate
      data[is.na(data)] <- 0 # set all non-masked values to 0
    } else {
      data <- plot_data_list[[i]]$data$estimate
    }
    mask <- plot_data_list[[i]]$extra_study_details$brain_masks$mask
    # catch mask if doesn't exist
    if (is.null(mask)) {
      stop("Mask not found. Check that data in correct format (and not using multivariate).")
    }


    
    p <- plot_full_mat(pp, data, mapping_path = mapping_path)

  }

  # TODO: any extra additions
  # p <- p + theme_minimal() + labs(x = pp$xlabel, y = pp$ylabel) +
  #   theme(legend.position = "none")

  return(p)
}

# plot_full_mat <- function(triangle_ordered, pooled = FALSE, ukb = FALSE, mapping_path = NA, save = TRUE, rearrange = TRUE, out_path = 'output', plot_name = 'matrix.png') {
plot_full_mat <- function(pp, triangle_ordered, ukb = FALSE, mapping_path = NA) {
  # takes an ordered triangle vector (without NAs) and plots the full matrix

  #TODO: look into heatmaply package for plotly interactive heatmap!
  # https://cran.r-project.org/web/packages/heatmaply/vignettes/heatmaply.html


  if (!is.na(mapping_path)) {

    # Set extra plot params

    pp$label_angle <- 45
    pp$boundary_color <- "black"
    pp$boundary_width <- 0.4
    pp$tick_label_size <- 6 #5
    pp$tick_label_size_small <- 4 #3.5
    
    
    pp$zlim <- pp$effect_size_limits_small/2
    # TODO: temporary hack for R^2 limits
    # if ((max(triangle_ordered,na.rm = TRUE) <= 1.1) && (min(triangle_ordered,na.rm = TRUE) >= -0.1)) {
    #   pp$zlim <- pp$rsq_effect_size_limits_smaller
    # }

    # Structure data into full mat

    # 1. determine whether we already pooled and whether diagonal included

    # load mapping
    mapping <- read.csv(mapping_path, header = TRUE)

    # match masked matrix to mapping
    n_nodes <- nrow(mapping)
    n_categories <- length(unique(mapping$category))
    pooling_opts <- c(FALSE, FALSE, TRUE, TRUE)
    diag_opts <- c(FALSE, TRUE, FALSE, TRUE)
    possible_lengths <- c(n_nodes * (n_nodes - 1) / 2, n_nodes * (n_nodes + 1) / 2, n_categories * (n_categories - 1) / 2, n_categories * (n_categories + 1) / 2)

    n_vars <- length(triangle_ordered)

    # try catch
    if (!length(triangle_ordered) %in% possible_lengths) {
      stop(paste0("Length of triangle_ordered (", length(triangle_ordered), ") does not match any possible lengths: ", paste(possible_lengths, collapse = ", ")))
    }
    using_pooling <- pooling_opts[which(possible_lengths == n_vars)]
    use_diag <- diag_opts[which(possible_lengths == n_vars)]

    # get number of rows
    if (using_pooling) {
      n_rows <- n_categories
      rearrange <- FALSE
    } else {
      n_rows <- n_nodes
      rearrange <- TRUE
    }


    mat <- matrix(0, nrow = n_rows, ncol = n_rows)
    mat[upper.tri(mat, diag = use_diag)] <- triangle_ordered

    # create full mat by adding transpose w diag removed
    mat2 <- mat
    mat2[lower.tri(mat2, diag = TRUE)] <- 0
    full_mat <- mat + t(mat2)


    # rearrange if necessary
    if (rearrange) {
      full_mat <- full_mat[mapping$oldroi, mapping$oldroi]
    }
    
    # hack to show colors above the limit
    # full_mat[full_mat == 0] <- NA
    full_mat[full_mat > pp$zlim[2]] <- pp$zlim[2]
    full_mat[full_mat < pp$zlim[1]] <- pp$zlim[1]

    # melt the matrix for ggplot
    melted <- melt(full_mat)
    colnames(melted) <- c("Var1", "Var2", "value")

    # set the title of the plot based on the number of nodes, atlas, and pooling
    plot_title <- paste0("Studies with ", tools::toTitleCase(basename(mapping_path)))
    if (using_pooling) {
      plot_title <- paste0(plot_title, " (pooled)")
    }

    # plot heatmap with title & theme

    heatmap_plot <- ggplot(melted, aes(Var1, Var2, fill = value)) +
      geom_tile() +
      labs(fill = paste0("Effect Size (", pp$effect_size_measure, ")"),
           title = plot_title,
           x = "Network", y = "Network") +
      # scale_fill_gradient2(limits = c(min(melted$value), max(melted$value)),
      scale_fill_gradient2(limits = pp$zlim,
                           low = "blue", mid = "white", high = "red", midpoint = 0) +
      theme_minimal() +
      theme(axis.title.x = element_text(margin = margin(t = 10)),
            axis.title.y = element_text(margin = margin(r = 10)),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title = pp$axis_title_size,
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.margin = margin(.5, .5, .5, .5, "lines"),
            plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

    # add data, lines, and labels

    # if (!is.na(mapping_path)) {
    if (!using_pooling) {
      # Get the boundary indices and label positions
      boundaries <- which(mapping$category[c(1:length(mapping$category)-1)] != mapping$category[c(2:length(mapping$category))])
      label_positions <- c(1, which(mapping$category[-1] != mapping$category[-length(mapping$category)]) + 1, length(mapping$category) + 1)
      label_positions <- (label_positions[-1] + label_positions[-length(label_positions)]) / 2
      label_strings <- mapping$label[label_positions]
      y_label_offset <- ifelse(n_rows > 100, -14, -2) # offsets proportional to matrix size
      x_label_offset <- ifelse(n_rows > 100, -12, -2) # offsets proportional to matrix size
      label_size <- pp$tick_label_size_small
    } else {
      boundaries <- 2:n_rows-1
      label_positions <- 1:n_rows
      label_strings <- unique(mapping$label)
      x_label_offset <- -0.1
      y_label_offset <- x_label_offset
      label_size <- pp$tick_label_size
    }

    # line_min_length = -1
    # line_max_length = n_rows + 1

    segments_df <- data.frame(
      x = c(boundaries + 0.5, rep(0.5, length(boundaries))),
      xend = c(boundaries + 0.5, rep(n_rows + 0.5, length(boundaries))),
      y = c(rep(0.5, length(boundaries)), boundaries + 0.5),
      yend = c(rep(n_rows + 0.5, length(boundaries)), boundaries + 0.5)
    )

    # Add labels to each mapping category
    heatmap_plot <- heatmap_plot +
      geom_segment(data = segments_df, aes(x = x, xend = xend, y = y, yend = yend), color = pp$boundary_color, size = pp$boundary_width, inherit.aes = FALSE) +
      annotate("text", x = label_positions, y = x_label_offset, label = label_strings, angle = pp$label_angle, hjust = 1, vjust = 0.5, size = label_size) +
      annotate("text", x = y_label_offset, y = label_positions, label = label_strings, angle = pp$label_angle, hjust = 0.5, vjust = 1, size = label_size) +
      coord_fixed(clip = "off")

  } else {

    warning("No mapping file provided. Skipping plot.")
    heatmap_plot <- NA

  }

  return(heatmap_plot)


}


#########################################################################
# Plot power
plot_power_panel <- function(pp, plot_data_list, output_type, use_category_bins = FALSE, do_spatial_plot = FALSE) {
  
  library(pwr)
  library(ggbeeswarm)
  
  # add power-specific plot params
  
  pp$size <- 0.4
  pp$alpha <- 0.1
  pp$alpha_sig <- 0.05/2 # two-sided
  pp$n_for_pwr <- 1000
  pp$reference_power <- 0.8
  pp$reference_n <- 1000
  # for plot
  pp$y_big <- 10000 # TODO: need better solution
  pp$thresholded_sizes <- c(1, 5)
  pp$thresholded_colors <- c("red", "black")
  
  this_type <- "two.sample"
  message("TESTING: assuming two-sample t-test for power calculation.")

  if (output_type == "power") {
    pp$xlabel <- "Power"
    pp$ylabel <- "Proportion"
    ylim <- c(0, 1)
  } else {
    pp$xlabel <- "Sample Size"
    pp$ylabel <- "Proportion"
    ylim <- c(0, pp$y_big) # set to max y - def need a better solution
  }
  
  if (use_category_bins) {
    
    if (output_type == "power") {
      these_bins <- pp$power_bins
      these_bin_labels <- pp$power_bin_labels
    } else {
      these_bins <- pp$sample_size_bins
      these_bin_labels <- pp$sample_size_bin_labels
    }
    
  } else {
    
    pp$hist_bin_width <- 0.001 # when there are all 0's, we want a very thin hist instead of density
    pp$xlabel <- "Effect Size"
    pp$ylabel <- "Density"
    pp$width_jitter <- 0.6
    pp$xlim = pp$effect_size_limits_smaller # very small for meta # TODO: change back or automate
  }
  
  if (output_type == "power") {
    this_thresh <- pp$reference_power
    # thresholded_sizes <- rev(pp$thresholded_sizes)
    # thresholded_colors <- rev(pp$thresholded_colors)
  } else {
    this_thresh <- pp$reference_n
    # thresholded_sizes <- pp$thresholded_sizes
    # thresholded_colors <- pp$thresholded_colors
    # ylim <- c(0, max(y, na.rm = TRUE) + 1) # set to max y))
  }
  
  if (do_spatial_plot && use_category_bins) {
    error("Currently not supported to plot spatial maps with binned effect sizes.")
  }
  
  
  # make plot object
  
  p <- ggplot()
  
  for (i in seq_along(plot_data_list)) {
    
      # color plots by binned sample size
      
      if (plot_data_list[[i]]$extra_study_details$n_title == "n = ") {
        plot_data_list[[i]]$extra_study_details$n_title <- "n = 9999999999" # hack to plot empty sample size in gold
      }
      
      # TODO: use to color
      sample_size_category <- cut(as.numeric(gsub("n = ", "", plot_data_list[[i]]$extra_study_details$n_title)),
                                  breaks = c(-Inf, pp$colors__sample_size$breaks_upper_lim),
                                  labels = pp$colors__sample_size$labels)
      
      # get power # TODO: two-sample only for testing
      
      # if (plot_data_list[[i]]$study_details$orig_stat_type == "t") {
      # infer whether one sample or two sample
      # if test component 2 is empty, it's one sample
      # if (plot_data_list[[i]]$study_details$test_component_2 == "") {
      #   this_type <- "one.sample"
      # } else {
      #     this_type <- "two.sample"
      #   }
      # } else {
      # }
      
      y <- numeric(length(plot_data_list[[i]]$data$cons_estimate))
      
      for (j in seq_along(plot_data_list[[i]]$data$cons_estimate)) {
        if (output_type == "power") {
          tmp <- pwr.t.test(d = plot_data_list[[i]]$data$cons_estimate[j], sig.level = pp$alpha_sig, n = pp$n_for_pwr, type = this_type)
          y[j] <- tmp$power
        } else {
          if (plot_data_list[[i]]$data$cons_estimate[j] != 0) { # can only calculate n for non-zero effects
            tmp <- pwr.t.test(d = abs(plot_data_list[[i]]$data$cons_estimate[j]), sig.level = pp$alpha_sig, power = pp$reference_power, type = this_type)
            y[j] <- tmp$n
          } else {
            y[j] <- pp$y_big
          }
        }
      }
      
      # TESTING
      # if (length(plot_data_list) > 4) {
      #   print(y)
      # }
      
    
    
    
      if (use_category_bins) {
        
        
        # create dataframe with y, sample_size_category, and binary of whether y > pp$reference_power
        # df <- data.frame(y = y, sample_size_category = sample_size_category, thresh = y > this_thresh)
        
        y__binned <- as.numeric(cut(abs(y), breaks = these_bins, right = FALSE))
        
        # manually make counts & normalize
        y_counts <- as.data.frame( table(y__binned) )
        colnames(y_counts) <- c("category", "count")
        y_counts$category <- as.numeric(as.character(y_counts$category))
        y_counts$count <- y_counts$count / sum(y_counts$count) # normalize counts
        
        
        if (length(unique(y_counts$category)) == 1) {
          
          df <- data.frame(x = y_counts$category, y = y_counts$count, sample_size_category = sample_size_category)
          
          p <- p + geom_col(data = df,
                            aes(x = x, y = y, fill = sample_size_category), 
                            width = 0.25, alpha = pp$alpha, color = NA)
          
        } else {
          
          # fill in 0 counts for the other categories
          y_counts <- merge(data.frame(category = 1:(length(these_bins)-1)), y_counts, by = "category", all.x = TRUE)
          y_counts$count[is.na(y_counts$count)] <- 0 # fill in 0 counts
          
          df <- data.frame(x = y_counts$category, y = y_counts$count, sample_size_category = sample_size_category)
          
          p <- p + 
            geom_area(data = df,
                      aes(x = x, y = y, fill = sample_size_category, color = sample_size_category), 
                      alpha = pp$alpha * 0.5) +
            geom_line(data = df,
                        aes(x = x, y = y, fill = sample_size_category, color = sample_size_category), 
                        size = pp$size, alpha = pp$alpha)
        }
      
      
      } else { # un-binned
      
        df <- data.frame(x = sample_size_category, y = y)
        
        if (do_spatial_plot) { # make spatial plot
          
          if (length(plot_data_list) > 1) {
            stop("Spatial plot only works for all_plot_combination_styles = 'single'")
          }
          
          # threshold
          if (output_type == "power") {
            plot_data_list[[i]]$data$estimate <- df$y > this_thresh
          } else {
            plot_data_list[[i]]$data$estimate <- df$y < this_thresh
          }
          
          p <- plot_data_list
          
          # # plot
          # if (plot_data_list[[1]]$extra_study_details$ref[[1]] == 'voxel') { # TODO: check
          #   p <- plot_activation_panel(pp, plot_data_list)
          # } else {
          #   p <- plot_connectivity_panel(pp, plot_data_list)
          # }
          

        } else { # standard
        
          p <- p +
            geom_violin(data = df,
                        aes(x = sample_size_category, y = y)) +
            geom_quasirandom(data = df,
                             aes(x = sample_size_category, y = y),
                             dodge.width = 0.9, varwidth = TRUE) +
            # scale_color_manual(values = c("FALSE" = thresholded_colors[1], "TRUE" = thresholded_colors[2])) +  # Colors for below/above threshold
            geom_hline(yintercept = this_thresh, linetype = "dashed", color = "red") +  # Add a threshold line
            labs(y = output_type) + # rename y axis
            theme_minimal() +
            theme(axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  legend.position = "none") +  # Hides the legend
            coord_cartesian(ylim = ylim)
        
          # TEMP - prepare spatial plot
          
        }
        
        
      
      }
      
      

  }
  
  # format theme elements
  
  if (use_category_bins) {
    p <- p + theme_minimal() + labs(x = pp$xlabel, y = pp$ylabel) +
      theme(legend.position = "none", axis.text.y = pp$axis_text_size, axis.text.x = pp$axis_text_size, axis.title = pp$axis_title_size) +
      # scale_fill_manual(values = pp$colors__sample_size$colors, breaks = pp$colors__sample_size$labels) +
      # scale_color_manual(values = pp$colors__sample_size$colors, breaks = pp$colors__sample_size$labels) +
      scale_x_continuous(breaks = c(1:(length(these_bins)-1)),
                         labels = these_bin_labels, # replace x ticks with labels
                         # limits = c(0.5,(length(these_bins) - 0.25)),
                         guide = guide_axis(check.overlap = FALSE)) +
      coord_cartesian(xlim = c(0.5, length(these_bins) - 1)) + # avoid cutting off the edge
      scale_y_continuous(limits = c(0, 1)) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))   # Rotate x-axis labels
  }
  
  return(p)
  
}


#########################################################################
# Summary info


#' Get summary info
#'
#' This function gets summary info to a ggplot object.
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
#' # get_summary_info(pp, study_details, extra_study_details)
get_summary_info <- function(study_details, extra_study_details) {

  summary_info <- list()

  summary_info$grouping_var_title <- switch(extra_study_details$grouping_var, # TODO: move w other pp but beware that singles may not have defined
                                  "none" = "None",
                                  "orig_stat_type" = "Statistic",
                                  "category" = "Outcome Measure")

  # set text

  summary_info$bottom_text <- paste0("Max conservative effect size: ", extra_study_details$max_cons_estimate, "\n",
                        "Percent not overlapping zero: ", round(extra_study_details$percent_not_zero * 100, 1), "%")

  if (extra_study_details$grouping_var == 'none') {

    summary_info$title_text <- paste0("Dataset: ", study_details$dataset, "   |  ",
                         "Test: ", study_details$orig_stat_type, ": ", study_details$test_component_1, ", ", study_details$test_component_2, "   |   ",
                         "Sample Size: ", extra_study_details$n_title, "   |  ",
                         "Map: ", study_details$map_type)

    # if field cons_mv_estimate exists in extra_study_details_multi, add to bottom text
    if ("mv_estimate" %in% names(extra_study_details)) {
      summary_info$bottom_text <- paste0(summary_info$bottom_text,"\n",
                            "Multivariate effect size: ", round(extra_study_details$mv_estimate, 2), " [", round(extra_study_details$mv_ci[1], 2), ", ", round(extra_study_details$mv_ci[2], 2), "]")
      # bottom_text <- paste0("Max conservative effect size: ", extra_study_details$max_cons_effect, "\n",
      #                       "Percent not overlapping zero: ", round(extra_study_details$percent_not_zero * 100, 1), "%\n",
      #                       "Multivariate effect size: ", round(extra_study_details$mv_estimate, 2), " [", round(extra_study_details$mv_ci[1], 2), ", ", round(extra_study_details$mv_ci[2], 2), "]")
    }

  } else {

    summary_info$title_text <- paste0(summary_info$grouping_var_title, ": ", extra_study_details$group_level, "   |  ",
                         "Reference Space: ", extra_study_details$ref)

    # if field cons_mv_estimate exists in extra_study_details_multi, add to bottom text # TODO: currently not defined when using group_data
    
    if ("mv_estimate" %in% names(extra_study_details)) { # meta
      
      summary_info$bottom_text <- paste0(summary_info$bottom_text,"\n",
                             "Multivariate effect size: ", round(extra_study_details$mv_estimate, 2), " [", round(extra_study_details$mv_ci[1], 2), ", ", round(extra_study_details$mv_ci[2], 2), "]")
      # bottom_text <- paste0("Max conservative effect size: ", extra_study_details$max_cons_effect, "\n",
      #                       "Percent not overlapping zero: ", round(extra_study_details$percent_not_zero * 100, 1), "%\n",
      #                       "Multivariate effect size: ", round(extra_study_details$mv_estimate, 2), " [", round(extra_study_details$mv_ci[1], 2), ", ", round(extra_study_details$mv_ci[2], 2), "]")
    
    } else if ("max_cons_mv_estimate" %in% names(extra_study_details)) { # overlapping
      
      summary_info$bottom_text <- paste0(summary_info$bottom_text,"\n",
                            "Max conservative multivariate effect size: ", round(extra_study_details$max_cons_mv_estimate, 2))
      # bottom_text <- paste0("Max conservative effect size: ", extra_study_details$max_cons_effect, "\n",
      #                       "Percent not overlapping zero: ", round(extra_study_details$percent_not_zero * 100, 1), "%\n",
      #                       "Max conservative multivariate effect size: ", round(extra_study_details$max_cons_mv_estimate, 2))
    # } else {
      # bottom_text <- paste0("Max conservative effect size: ", extra_study_details$max_cons_effect, "\n",
      #                       "Percent not overlapping zero: ", round(extra_study_details$percent_not_zero * 100, 1), "%")
    
    }
    
  }

  return(summary_info)
}

#########################################################################
# Combine summary info if multiple overlapping plots so there's one description per panel

combine_summary_info <- function(summary_info) {

  summary_info2 <- list()
  summary_info2$title_text <- summary_info[[1]]$title_text
  # append num studies
  summary_info2$title_text <- paste0(summary_info2$title_text, " (", length(summary_info), " studies)")

  # for summary_info2$bottom_text, get summary_info[:]$bottom_text strings for each study and parse max cons effect size, perce not overlapping zero, and multivar effect size. Take average
  max_cons_effect__overlapping <- max(as.numeric(sapply(summary_info, function(x) as.numeric(sub(".*Max conservative effect size: ([^\n]+).*", "\\1", x$bottom_text)))))
  percent_not_zero__overlapping <- mean(as.numeric(sapply(summary_info, function(x) as.numeric(sub(".*Percent not overlapping zero: ([^%]+).*", "\\1", x$bottom_text)))))
  mv_estimate__overlapping <- mean(as.numeric(sapply(summary_info, function(x) as.numeric(sub(".*Max conservative multivariate effect size: ([^\n]+).*", "\\1", x$bottom_text)))))

  summary_info2$bottom_text <- paste0("Max conservative effect size: ", round(max_cons_effect__overlapping, 2), "\n",
                                      "Percent not overlapping zero: ", round(percent_not_zero__overlapping, 1), "%\n",
                                      "Multivariate effect size: ", round(mv_estimate__overlapping, 2))

  summary_info <- summary_info2

}


#########################################################################
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
#' # add_plot_description(p, pp, study_details, extra_study_details)
add_plot_description <- function(p, pp, summary_info, add_extra_text, do_minimal_title) {

  # add description-specific plot params
  pp$title_size <- 21
  pp$caption_size <- 15
  pp$title_hjust <- 0.5
  pp$title_lmargin <- -6 # adjust it a bit left of the plot y-axis
  pp$caption_hjust <- 0

  # if overlapping plots, make single summary_info
  # study_details__overlapping <- plot_data_list[[1]]$study_details
  #   extra_study_details__overlapping <- plot_data_list[[1]]$extra_study_details
  #
  #   # get summary info across all studies in group
  #   study_summary <- lapply(plot_data_list, function(x) {
  #     list(
  #       max_cons_estimate = x$extra_study_details$max_cons_estimate,
  #       percent_not_zero = x$extra_study_details$percent_not_zero,
  #       n_variables = length(x$data$cons_estimate),
  #       cons_mv_estimate = x$extra_study_details$mv_ci[1]
  #     )
  #   })
  #
  #   study_summary <- do.call(rbind, lapply(study_summary, as.data.frame))
  #
  #   # take max of all max effects and average percent nonzeros
  #   extra_study_details__overlapping$max_cons_effect <- study_summary$max_cons_estimate[which.max(abs(study_summary$max_cons_estimate))]
  #   extra_study_details__overlapping$percent_not_zero <- sum(study_summary$percent_not_zero*study_summary$n_variables)/sum(study_summary$n_variables)
  #   extra_study_details__overlapping$max_cons_mv_estimate <- max(study_summary$cons_mv_estimate)
  #
  #   p <- add_plot_description(p, pp, study_details__overlapping, extra_study_details__overlapping)
  #


  # for manuscript, remove labels from title for cleaner look
  if (do_minimal_title) {
    summary_info$title_text <- gsub("(Dataset: )|  ([|])[^:]*:", "\\2", summary_info$title_text)
    summary_info$title_text <- gsub("([|][^|]*$)", "", summary_info$title_text)
  }

  p <- p +
    ggtitle(summary_info$title_text) +
    theme(plot.title = element_text(hjust = pp$title_hjust, margin = margin(l = pp$title_lmargin, unit = "pt"),
                                    size = pp$title_size, face = "bold"),
          plot.caption = element_text(hjust = pp$caption_hjust, size = pp$caption_size),
          panel.background = element_blank(),
          plot.background = element_blank()
    )

  if (add_extra_text) {
    p <- p + labs(caption = summary_info$bottom_text)
  }

  return(p)

}


