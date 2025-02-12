####################################################################
#create plots
#
# prereqs:
# > library(devtools) # for install
# > install(utils_script_local) # see script path defined below
#
####################################################################

## Load libraries

utils_script_local <- "/Users/stephanienoble/Library/CloudStorage/GoogleDrive-s.noble@northeastern.edu/My Drive/Lab/xMore/Software/scripts/R/myscripts/effect_size/BrainEffeX_utils/"
load_all(utils_script_local) # load from local scripts - for testing
#library(BrainEffeX.utils) # load from package

library(metafor)
library(ggpubr)


## User-defined paths & parameters

# plot params

pooling <- 'none'     # 'none', 'net'
motion <- 'none'      # 'none', 'stat_control', "full_residualization", "thresholding"

make_plots <- TRUE
save_plots <- TRUE

plot_type <- 'density' # c('density', 'simci')
add_plt_description <- TRUE

plot_combination_style <- 'single' # c('single','overlapping','meta')
grouping_var <- 'orig_stat_type'    # 'none', 'category', 'orig_stat_type' # used for both meta-analysis and overlap plots - TODO: separate out?
effect_size_type <- 'd'

# directories

this_data_dir <- "/Users/stephanienoble/Library/CloudStorage/GoogleDrive-s.noble@northeastern.edu/My Drive/Lab/xMore/Software/scripts/R/myscripts/effect_size/BrainEffeX/data/"
out_dir_basename <- "/Users/stephanienoble/Library/CloudStorage/GoogleDrive-s.noble@northeastern.edu/My Drive/Lab/Tasks-Ongoing/K99/Effect_Size/manuscript/figures/plots/"


## Set up strings

combo_name <- paste0('pooling.', pooling, '.motion.', motion, '.mv.none')
mv_combo_name <- paste0('pooling.', pooling, '.motion.', motion, '.mv.multi')


## Load data

if (!exists("v")) {
  v <- load_data(this_data_dir)
}


## Run meta-analysis, if specified

# TODO: move to combine_gl with other more intensive processing / stat estimates & run all relevantmeta beforehand

if (plot_combination_style == 'meta') {
  if (!("data_group" %in% names(v)) || (previous_meta_grouping_var != grouping_var)) {
    v <- meta_analysis(v, v$brain_masks, combo_name, grouping_var = grouping_var)
    previous_meta_grouping_var <- grouping_var
  }
}


## Set up unique identifiers for each plot

plot_info__idx <- list() # each row = list of study(s) in data or data_group to include in each plot
# for single plots: each row = 1 entry per study to index into v$data
# for meta-analysis plots: each row = 1 entry per category to index v$data_group
# for overlapping plots: each row = list of indices per group (x map type) to index into v$data

plot_info__grouping_var <- list() # each row = grouping variable (same value repeated for each plot)
plot_info__group_level <- list() # each row = level within grouping variable
plot_info__ref <- list() # each row = ref(s) used for a study or grouping variable

if (plot_combination_style == 'single') {  # name by study

  for (i in 1:length(v$data)) {
    plot_info__idx[[names(v$data)[[i]]]] <- i
    plot_info__grouping_var[[names(v$data)[[i]]]] <- "none"  # overwrite any other grouping var if doing single plots single
    plot_info__group_level[[names(v$data)[[i]]]] <- NA
    plot_info__ref[[names(v$data)[[i]]]] <- v$study$ref[i]
  }

} else if (plot_combination_style == 'meta') { # name by average of grouping var

  for (i in 1:length(v$data_group)) {
    plot_info__idx[[names(v$data_group)[[i]]]] <- i
    plot_info__grouping_var[[names(v$data_group)[[i]]]] <- grouping_var
    plot_info__group_level[[names(v$data_group)[[i]]]] <- v$study_group$group_level[i]
    plot_info__ref[[names(v$data_group)[[i]]]] <- v$study_group$ref[i]
  }

} else if (plot_combination_style == 'overlapping') { # overlapping individual plots

  if (grouping_var == 'category') {
    study_group_name <- v$study$category
  } else if (grouping_var == 'orig_stat_type') {
    study_group_name <- v$study$orig_stat_type
  }

  all_group_names <- unique(study_group_name)
  all_map_types <- unique(v$study$map_type)

  for (this_map_type in all_map_types) {
    for (this_group_name in all_group_names) {
      idx <- which(study_group_name == this_group_name & v$study$map_type == this_map_type)
      plot_info__idx[[paste0(this_group_name, '.', this_map_type)]] <- idx
      plot_info__grouping_var[[paste0(this_group_name, '.', this_map_type)]] <- grouping_var
      plot_info__group_level[[paste0(this_group_name, '.', this_map_type)]] <- this_group_name
      plot_info__ref[[paste0(this_group_name, '.', this_map_type)]] <- unique(v$study$ref[idx])
    }
  }
}

plot_info <- data.frame(
  idx = I(plot_info__idx),
  grouping_var = unlist(plot_info__grouping_var),
  group_level = unlist(plot_info__group_level),
  ref = I(plot_info__ref),
  row.names = names(plot_info__idx),
  stringsAsFactors = FALSE
)


## Make Plots

panel_list <- list()

for (i in 1:length(plot_info$idx)) { # loop over panels - this_study_or_group is the name of the group or study

  this_study_or_group <- rownames(plot_info)[i]
  this_plot_info <- plot_info[this_study_or_group,]

  pd_list <- list()
  n_studies_in_pd_list <- 1

  # 1. Prep

  for (j in plot_info$idx[[i]]) {

    # change metadata based on whether using meta-analysis

    if (plot_combination_style == 'meta') {

      name <- names(v$data_group[j])
      data <- v$data_group[[j]]
      study_details <- list()

    } else {

      name <- names(v$data[j])
      data <- v$data[[j]]
      study_details <- v$study[j, ]

    }

    if (combo_name %in% names(data)) { # if combo_name exists in data (e.g., not all studies have net)

      # prep

      pd <- prep_data_for_plot(data = data, name = name, study_details = study_details, combo_name = combo_name, mv_combo_name = mv_combo_name, estimate = effect_size_type, plot_info = this_plot_info)

      pd_list[[n_studies_in_pd_list]] <- pd
      n_studies_in_pd_list <- n_studies_in_pd_list + 1

    }
  }

  # 2. Plot

  if (make_plots) {
    if (length(pd_list) > 0) { # plot only if pd_list isn't empty
      panel_list[[i]] <- create_plots(pd_list, plot_type = plot_type, add_description = add_plt_description)
    }
  }

}


if (make_plots) {

  # General plot parameters
  # TODO: figure out what we want to set up here vs. to pass or set up in
  # create_plots, which gets passed to plot_sim_ci, etc.
  # Should at least set all panel / canvas dimensions here
  pp <- list()
  pp$width_per_panel <- 6
  pp$height_per_panel <- 5
  pp$res <- 300
  pp$units <- "in"
  pp$ncol <- 1
  pp$nrow <- length(panel_list)

  if (save_plots) {

    # filename
    # /odir/meta/net/density - motion-none.png
    # out_dir <- ...
    # fn <- paste0(this_study_or_group, '_', n_studies_in_pd_list, '.png')
    # out_name = paste0(out_dir, '/', fn)

    out_dir <- paste0(out_dir_basename, plot_combination_style, '/', pooling, '/')
    out_name <- paste0(out_dir, plot_type, '- motion-', motion, '.png')

    cat("Saving plots to...\n", out_name, "\n", sep = "")

    if (!dir.exists(out_dir)) {
      dir.create(out_dir, recursive = TRUE)
    }

    png(out_name, width = pp$width * pp$ncol, height = pp$height * pp$nrow, res = pp$res, units = pp$units) # TODO: this is tied to
  }

  # plot multiple panels

  multi_plot <- ggarrange(plotlist = panel_list, ncol=pp$ncol, nrow=pp$row)
  multi_plot <- annotate_figure(multi_plot,
                                top = text_grob(paste0(plot_type, ' (motion=', motion,")"), color = "black", face = "bold", size = 11))

  print(multi_plot)

  # TESTING:
  # dfs_lst <- split(mtcars, ~factor(cyl))
  # plots_lst <- lapply(1:3, \(plt) {
  #        ggplot(dfs_lst[[plt]], aes(wt, mpg)) +
  #              geom_point()
  # })
  # ggarrange(plotlist = plots_lst, ncol=1)


  if (save_plots) {
    dev.off()
  }
}
