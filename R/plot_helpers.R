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

    # plot
      p <- add_geom_layers(p, subset(plot_df, x <= below_cross_idx), pp$non_overlap_colors, pp$alpha_line, pp$alpha_ribbon)
      p <- add_geom_layers(p, subset(plot_df, x >= below_cross_idx & x <= above_cross_idx), pp$overlap_colors, pp$alpha_line, pp$alpha_ribbon)
      p <- add_geom_layers(p, subset(plot_df, x >= above_cross_idx), pp$non_overlap_colors, pp$alpha_line, pp$alpha_ribbon)

  }

  p <- p + labs(x = pp$xlabel, y = pp$ylabel) +
    scale_y_continuous(limits = pp$ylim) +
    theme_classic() +
    theme(axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.text.y = pp$axis_text_size)

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
plot_density_panel <- function(pp, plot_data_list) {

  # add density-specific plot params

  # pp$xlim <- c(-0.15, 0.15)
  pp$alpha <- 0.1
  pp$size <- 0.4
  pp$colors__sample_size <- data.frame(labels = c("<1,000", "1,000-5,000", "5,000-10,000", ">10,000", "NA"), colors = c("#82A651", "#3AB7BE", "#E7786C", "#B873F7","#B59410"), breaks_upper_lim = c(1000, 5000, 10000, 999999999, Inf))
  pp$hist_bin_width <- 0.001 # when there are all 0's, we want a very thin hist instead of density
  pp$xlabel <- "Effect Size"
  pp$ylabel <- "Density"

  # make plot object

  p <- ggplot()

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


      if (length(unique(plot_data_list[[i]]$data$cons_estimate)) == 1) {
        p <- p + geom_histogram(data = data.frame(value = plot_data_list[[i]]$data$cons_estimate, sample_size_category = sample_size_category),
                                aes(x = value, fill = sample_size_category, color = sample_size_category), alpha = pp$alpha, size = pp$size, binwidth = pp$hist_bin_width)
      } else {
        p <- p + geom_density(data = data.frame(value = plot_data_list[[i]]$data$cons_estimate, sample_size_category = sample_size_category),
                          aes(x = value, fill = sample_size_category, color = sample_size_category), alpha = pp$alpha, size = pp$size) # for unique color per study, do: fill = i, color = i
      }
  }

  p <- p + theme_minimal() + labs(x = pp$xlabel, y = pp$ylabel) +
    xlim(pp$xlim) +
    theme(legend.position = "none", axis.text.y = pp$axis_text_size, axis.text.x = pp$axis_text_size) +
    scale_fill_manual(values = pp$colors__sample_size$colors, breaks = pp$colors__sample_size$labels) +
    scale_color_manual(values = pp$colors__sample_size$colors, breaks = pp$colors__sample_size$labels)

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
plot_activation_panel <- function(pp, plot_data_list) {

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
  pp$ncol <- 40 # also controls number of ticks on colorbar
  pp$col_y <- colorspace::diverge_hsv(pp$ncol)
  # pp$n_colorbar_ticks <- 5
  pp$ycolorbar <- TRUE
  pp$mfrow <- c(3, 1)
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

      data <- plot_data_list[[i]]$data$estimate
      mask <- plot_data_list[[i]]$extra_study_details$brain_masks$mask
      nii <- create_nifti(template, data, mask)

      # set colorbar limits
      if (max(abs(data)) > pp$effect_size_thresh) {
        pp$zlim_range <- pp$effect_size_limits_big
      } else {
        pp$zlim_range <- pp$effect_size_limits_small
      }
      pp$zlim_range <- pp$zlim_range/2

      nii[nii == 0] <- NA
      nii[nii > pp$zlim_range[2]] <- pp$zlim_range[2]
      nii[nii < pp$zlim_range[1]] <- pp$zlim_range[1]

      # pdf(file = NULL) # don't plot anywhere
      # par(mar = c(1, 1, 1, 4))

      # p <- grid.grabExpr({

      if (pp$do_static_figs) {
        png("ortho_tmp.png", width = 800, height = 800)
      }

      ortho2(
        x = nii,
        y = nii,
        crosshairs = TRUE,
        NA.x = TRUE,
        col.y = pp$col_y,
        xyz = c(pp$xCoord, pp$yCoord, pp$zCoord),
        bg = pp$bg,
        text.color = pp$text_color,
        #clabels = seq(-0.1, 0.1, length.out = 30),
        ybreaks = seq(pp$zlim_range[1], pp$zlim_range[2], length.out = pp$ncol+1),
        clabels = round(seq(pp$zlim_range[1], pp$zlim_range[2], length.out = pp$ncol), 2),
        ycolorbar = pp$ycolorbar,
        mfrow = pp$mfrow
        # zlim = pp$zlim_range
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
plot_connectivity_panel <- function(pp, plot_data_list) {

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
      pooled <- FALSE
      # }
    } else if (plot_data_list[[i]]$extra_study_details$ref[[1]] == "ukb_55") {
      mapping_path <- system.file("data/parcellations/map55_ukb.csv", package = "BrainEffeX.utils")
      pooled <- FALSE
    } else {
      mapping_path <- NA
    }
    rearrange <- !pooled # only rearrange edge-level

    #       template <- plot_data_list[[i]]$study_details$ref

    data <- plot_data_list[[i]]$data$estimate
    mask <- plot_data_list[[i]]$extra_study_details$brain_masks$mask

    p <- plot_full_mat(data, rearrange = rearrange, pooled = pooled, mapping_path = mapping_path)

  }

  # TODO: any extra additions
  # p <- p + theme_minimal() + labs(x = pp$xlabel, y = pp$ylabel) +
  #   theme(legend.position = "none")

  return(p)
}

# plot_full_mat <- function(triangle_ordered, pooled = FALSE, ukb = FALSE, mapping_path = NA, save = TRUE, rearrange = TRUE, out_path = 'output', plot_name = 'matrix.png') {
plot_full_mat <- function(triangle_ordered, pooled = FALSE, ukb = FALSE, mapping_path = NA, rearrange = TRUE) {
  # takes an ordered triangle vector (without NAs) and plots the full matrix

  #TODO: look into heatmaply package for plotly interactive heatmap!
  # https://cran.r-project.org/web/packages/heatmaply/vignettes/heatmaply.html

  if (!is.na(mapping_path)) {
    # load mapping
    mapping <- read.csv(mapping_path, header = TRUE)
  }

  # if the data is pooled, the number of nodes is determined from the map
  if (pooled) {
    nrow = length(unique(mapping$category))
  } else {
    nrow = (((-1 + sqrt(1 + 8 * length(triangle_ordered))) / 2) + 1)
  }

  # mirror the triangle across the x = y line to get full matrix
  # first fill in half the matrix with the triangle data
  mat <- matrix(0, nrow = nrow, ncol = nrow)
  mat[upper.tri(mat, diag = ifelse(pooled, TRUE, FALSE))] <- triangle_ordered
  full_mat <- mat + t(mat) #- diag(diag(triangle_ordered))

  # rearrange if necessary
  if (rearrange) {
    full_mat <- full_mat[mapping$oldroi, mapping$oldroi]
  }

  # melt the matrix for ggplot
  melted <- melt(full_mat)
  colnames(melted) <- c("Var1", "Var2", "value")

  # determine the title of the plot based on the number of nodes
  plot_title = ifelse((nrow == 268 & !pooled), "Studies with Shen 268 atlas", ifelse((nrow == 55 & !pooled), "Studies with UKB 55 nodes", ifelse((pooled & !ukb), "Studies with Shen 268 atlas (pooled)", "UKB pooled by Shen 268 node overlap")))

  heatmap_plot <- ggplot(melted, aes(Var1, Var2, fill = value)) +

    labs(fill = "Cohen's d",
         title = plot_title,
         x = "", y = "") +

    geom_tile() +
    scale_fill_gradient2(limits = c(min(melted$value), max(melted$value)),
                         low = "blue", mid = "white", high = "red", midpoint = 0) +
    theme_minimal() +
    theme(axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = margin(.5, .5, .5, .5, "lines"),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

  if (!is.na(mapping_path)) {
    if (!pooled) {

      for (i in 1:(nrow(mapping) - 1)) {
        if (mapping$category[i] != mapping$category[i + 1]) {
          heatmap_plot <- heatmap_plot + geom_vline(xintercept = i + 0.5, color = "black", size = 0.3) +
            geom_hline(yintercept = i+0.5, color = "black")
        }
      }

      # Calculate the positions of the labels
      label_positions <- c(1, which(mapping$category[-1] != mapping$category[-length(mapping$category)]) + 1, length(mapping$category) + 1)
      label_positions <- (label_positions[-1] + label_positions[-length(label_positions)]) / 2
      label_strings <- mapping$label[label_positions]

      # Add labels to each mapping category
      heatmap_plot <- heatmap_plot + annotate("text", x = label_positions, y = -6, label = label_strings, angle = 90, hjust = 1, vjust=0.5, size=3.5) + coord_cartesian(clip="off")
      heatmap_plot <- heatmap_plot + annotate("text", x = -10, y = label_positions, label = label_strings, angle = 0, hjust = 0.5, vjust=1, size=3.5)
    }

    if (pooled) {
      # for pooled data, add black lines to separate every cell of the matrix
      # label each row and column as the networks
      for (i in 1:(nrow)) {
        heatmap_plot <- heatmap_plot + geom_vline(xintercept = i+0.5, color = "black", size = 0.3) +
          geom_hline(yintercept = i+0.5, color = "black")

        heatmap_plot <- heatmap_plot + annotate("text", x = i, y = -1, label = unique(mapping$label)[i], angle = 90, hjust = 1, vjust=0.5, size=3.5) + coord_cartesian(clip="off")
        heatmap_plot <- heatmap_plot + annotate("text", x = -1, y = i, label = unique(mapping$label)[i], angle = 0, hjust = 0.5, vjust=1, size=3.5)
      }
    }
  }


  # Add axis labels to the heatmap
  if (!is.na(mapping_path)) {
    heatmap_plot <- heatmap_plot + labs(x = "Network", y = "Network")
  } else if (is.na(mapping_path)) {
    heatmap_plot <- heatmap_plot + labs(x = "UKB 55 Node", y = "UKB 55 Node")
  }

  return(heatmap_plot)


}


#########################################################################
# Summary info


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
#' # add_plot_description(p, study_details, extra_study_details)
add_plot_description <- function(p, pp, study_details, extra_study_details) {

  # add description-specific plot params
  pp$title_size <- 21
  pp$caption_size <- 10
  pp$title_hjust <- 0.5
  pp$title_lmargin <- -6 # adjust it a bit left of the plot y-axis
  pp$caption_hjust <- 0

  pp$grouping_var_title <- switch(extra_study_details$grouping_var, # TODO: move w other pp but beware that singles may not have defined
                                  "none" = "None",
                                  "orig_stat_type" = "Statistic",
                                  "category" = "Outcome Measure")

  # set text

  bottom_text <- paste0("Max conservative effect size: ", extra_study_details$max_cons_effect, "\n",
                        "Percent not overlapping zero: ", round(extra_study_details$percent_not_zero * 100, 1), "%")

  if (extra_study_details$grouping_var == 'none') {

    title_text <- paste0("Dataset: ", study_details$dataset, "   |  ",
                         "Test: ", study_details$orig_stat_type, ": ", study_details$test_component_1, ", ", study_details$test_component_2, "   |   ",
                         "Sample Size: ", extra_study_details$n_title, "   |  ",
                         "Map: ", study_details$map_type)

    bottom_text <- paste0(bottom_text,"\n",
                          "Multivariate effect size: ", round(extra_study_details$mv_estimate, 2), " [", round(extra_study_details$mv_ci[1], 2), ", ", round(extra_study_details$mv_ci[2], 2), "]")
    # bottom_text <- paste0("Max conservative effect size: ", extra_study_details$max_cons_effect, "\n",
    #                       "Percent not overlapping zero: ", round(extra_study_details$percent_not_zero * 100, 1), "%\n",
    #                       "Multivariate effect size: ", round(extra_study_details$mv_estimate, 2), " [", round(extra_study_details$mv_ci[1], 2), ", ", round(extra_study_details$mv_ci[2], 2), "]")

  } else {

    title_text <- paste0(pp$grouping_var_title, ": ", extra_study_details$group_level, "   |  ",
                         "Reference Space: ", extra_study_details$ref)

    # if field cons_mv_estimate exists in extra_study_details_multi, add to bottom text # TODO: currently not defined when using group_data
    if ("max_cons_mv_estimate" %in% names(extra_study_details)) {
      bottom_text <- paste0(bottom_text,"\n",
                            "Max conservative multivariate effect size: ", round(extra_study_details$max_cons_mv_estimate, 2))
      # bottom_text <- paste0("Max conservative effect size: ", extra_study_details$max_cons_effect, "\n",
      #                       "Percent not overlapping zero: ", round(extra_study_details$percent_not_zero * 100, 1), "%\n",
      #                       "Max conservative multivariate effect size: ", round(extra_study_details$max_cons_mv_estimate, 2))
    # } else {
      # bottom_text <- paste0("Max conservative effect size: ", extra_study_details$max_cons_effect, "\n",
      #                       "Percent not overlapping zero: ", round(extra_study_details$percent_not_zero * 100, 1), "%")
    }
  }

  if (pp$plot_detail_style == 'manuscript') { # for manuscript, remove labels from title
    title_text <- gsub("(Dataset: )|  ([|])[^:]*:", "\\2", title_text)
    title_text <- gsub("([|][^|]*$)", "", title_text)
  }

  p <- p +
    ggtitle(title_text) +
    labs(caption = bottom_text) +
    theme(plot.title = element_text(hjust = pp$title_hjust, margin = margin(l = pp$title_lmargin, unit = "pt"),
          size = pp$title_size, face = "bold"),
          plot.caption = element_text(hjust = pp$caption_hjust, size = pp$caption_size))

  return(p)
}


