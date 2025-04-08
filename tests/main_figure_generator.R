####################################################################
#create plots
#
# prereqs:
# > library(devtools) # for install
# > install(utils_script_local) # see script path defined below
#
####################################################################

## Load libraries

# utils_script_local <- "/Users/steph/Library/CloudStorage/GoogleDrive-s.noble@northeastern.edu/My\ Drive/Lab/xMore/Software/scripts/R/myscripts/effect_size/misc/BrainEffeX_utils_cns/"
utils_script_local <- "/Users/stephanienoble/Library/CloudStorage/GoogleDrive-s.noble@northeastern.edu/My Drive/Lab/xMore/Software/scripts/R/myscripts/effect_size/misc/BrainEffeX_utils_cns/"
load_all(utils_script_local) # load from local scripts - for testing
#library(BrainEffeX.utils) # load from package

library(metafor)
library(ggpubr) # requires svglite
# library(png)


## User-defined paths & parameters

# plot params

# meta
# single r_sq

all_effect_size_types <- c('d','r_sq') # 'd' or 'r_sq' c('d', 'r_sq', 'd.full_res')

all_motion <- c('none', 'regression', 'threshold') #       # c('none', 'regression', 'threshold') # TODO: stat_control -> "...regression...$d", full_residualization -> "...regression...$d.full_res"
all_pooling <- c('none', 'net') #  # c('none','net')

all_plot_combination_styles <- c('single','meta')   # c('single','meta')
all_grouping_var <- c('category', 'orig_stat_type')          # c('none', 'category', 'orig_stat_type') # used only for meta & overlap plots - TODO: separate out?

all_plot_types <- c('simci')      # c('simci')

make_plots <- TRUE
save_plots <- TRUE
save_logs <- FALSE
add_plt_description <- TRUE # text at bottom of screen
rearrange_by_stat_type <- TRUE # for single plots

# directories

# this_data_dir <- "/Users/steph/Library/CloudStorage/GoogleDrive-s.noble@northeastern.edu/My Drive/Lab/xMore/Software/scripts/R/myscripts/effect_size/BrainEffeX/data/"
# out_dir_basename <- "/Users/steph/Library/CloudStorage/GoogleDrive-s.noble@northeastern.edu/My Drive/Lab/Tasks-Ongoing/K99/Effect_Size/manuscript/figures/plots/cns/"

this_data_dir <- "/Users/stephanienoble/Library/CloudStorage/GoogleDrive-s.noble@northeastern.edu/My Drive/Lab/xMore/Software/scripts/R/myscripts/effect_size/BrainEffeX/data/"
out_dir_basename <- "/Users/stephanienoble/Library/CloudStorage/GoogleDrive-s.noble@northeastern.edu/My Drive/Lab/Tasks-Ongoing/K99/Effect_Size/manuscript/figures/plots/cns/"



## Loop over plot types and styles

for (plot_combination_style in all_plot_combination_styles) {
for (plot_type in all_plot_types) {
for (pooling in all_pooling) {
for (motion in all_motion) {
for (grouping_var in all_grouping_var) {
for (effect_size_type in all_effect_size_types) {

print(paste0('Doing plot_combination_style: ', plot_combination_style, ' | plot_type: ', plot_type, ' | pooling: ', pooling, ' | motion: ', motion, ' | grouping_var: ', grouping_var))


## Set up strings

combo_name <- paste0('pooling.', pooling, '.motion.', motion, '.mv.none')
mv_combo_name <- paste0('pooling.', pooling, '.motion.', motion, '.mv.multi')

# Correct args

if (plot_combination_style == 'single' & grouping_var != 'none') {
  grouping_var <- 'none'
  cat("Warning: grouping_var set to 'none' for single plots\n")
}

## Load data

if (!exists("v")) {
  v <- load_data(this_data_dir)
}


## Run meta-analysis, if specified

# TODO: move to combine_gl with other more intensive processing / stat estimates & run all relevantmeta beforehand
run_meta <- FALSE
try_meta_file <- FALSE

if (plot_combination_style == 'meta') {

  # setup string and filename
  meta_str <- paste0('meta_',grouping_var)
  meta_fn_dir <- system.file("meta/", package = "BrainEffeX.utils") # TODO: set this somewhere else
  meta_fn <- file.path(meta_fn_dir, "v.RData")

  # check if this var contains the meta for this grouping var / combo
  if (!(meta_str %in% names(v))) { # check if this grouping var already exists in data
    try_meta_file <- TRUE
  } else if (!(combo_name %in% names(v[[meta_str]]$data[[1]]))) { # check if this combo_name has been run
    try_meta_file <- TRUE
  }

  # TODO: will also have to catch the case where d is defined but r_sq is not

  # check if saved meta file contains the meta for this grouping var / combo
  if (try_meta_file) {
    if (file.exists(meta_fn)) { # try to read pre-saved meta-analysis
      load(meta_fn)
      if (!(meta_str %in% names(v))) { # check again for grouping var
        run_meta <- TRUE
      } else if (!(combo_name %in% names(v[[meta_str]]$data[[1]]))) { # check again for combo
        run_meta <- TRUE
      }
    } else { # no file
      run_meta <- TRUE
    }

  }
}

if (run_meta) {
  v <- meta_analysis(v, v$brain_masks, combo_name, grouping_var = grouping_var)
  save(v, file = meta_fn)
}







## Set up unique identifiers for each plot

plot_info__idx <- list() # each row = list of study(s) in data to include in each plot
# for single plots: each row = 1 entry per study to index into v$data
# for meta-analysis plots: each row = 1 entry per category to index v[[meta_str]]$data
# for overlapping plots: each row = list of indices per group (x map type) to index into v$data

plot_info__grouping_var <- list() # each row = grouping variable (same value repeated for each plot)
plot_info__group_level <- list() # each row = level within grouping variable
plot_info__ref <- list() # each row = ref(s) used for a study or grouping variable

if (plot_combination_style == 'single') {  # name by study

  all_study_names <- names(v$data)

  for (i in 1:length(v$data)) {
    plot_info__idx[[all_study_names[[i]]]] <- i
    plot_info__grouping_var[[all_study_names[[i]]]] <- "none"  # overwrite any other grouping var if doing single plots
    plot_info__group_level[[all_study_names[[i]]]] <- NA
    plot_info__ref[[all_study_names[[i]]]] <- v$study$ref[i]
  }


  # sort all rows of plot_info__idx by orig_stat_type, with studies sharing same stat type next to each other
  if (rearrange_by_stat_type) {
    orig_stat_type_order <- c(which(v$study$orig_stat_type == 'r'), which(v$study$orig_stat_type == 't2'), which(v$study$orig_stat_type == 't'))
    plot_info__idx <- plot_info__idx[orig_stat_type_order]
    plot_info__grouping_var <- plot_info__grouping_var[orig_stat_type_order]
    plot_info__group_level <- plot_info__group_level[orig_stat_type_order]
    plot_info__ref <- plot_info__ref[orig_stat_type_order]
  }


} else if (plot_combination_style == 'meta') { # name by average of grouping var

  for (i in 1:length(v[[meta_str]]$data)) {
    plot_info__idx[[names(v[[meta_str]]$data)[[i]]]] <- i
    plot_info__grouping_var[[names(v[[meta_str]]$data)[[i]]]] <- grouping_var
    plot_info__group_level[[names(v[[meta_str]]$data)[[i]]]] <- v[[meta_str]]$study$group_level[i]
    plot_info__ref[[names(v[[meta_str]]$data)[[i]]]] <- v[[meta_str]]$study$ref[i]
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
rm(plot_info__idx, plot_info__grouping_var, plot_info__group_level, plot_info__ref)



## Make Plots

panel_list_spatial <- list() # list of panels
panel_list_simci <- list() # list of panels
panel_list <-list()
log_list <- list() # list of logs

for (i in 1:length(plot_info$idx)) { # loop over panels - this_study_or_group is the name of the group or study

  this_study_or_group <- rownames(plot_info)[i]
  this_plot_info <- plot_info[this_study_or_group,]

  
  pd_list_spatial <- list() # list of plot info for single panel
  pd_list_simci <- list() # list of plot info for single panel
  pd_list <- list()
  ld_list <- list() # list of log info for single panel

  n_studies_in_pd_list <- 1

  # 1. Prep

  for (j in plot_info$idx[[i]]) {

    # change metadata based on whether using meta-analysis

    if (plot_combination_style == 'meta') {

      # name <- names(v$data_group[j])
      data <- v[[meta_str]]$data[[j]]
      study_details <- list()
      brain_masks <- v[[meta_str]]$brain_masks[[j]]$pooling.none.motion.none.mv.none # TODO: this is because we explicitly set this for meta but not for single studies - assuming motion type shouldn't affect the mask and always using an external mask for pooling

    } else {

      # name <- names(v$data[j])
      data <- v$data[[j]]
      study_details <- v$study[j, ]
      brain_masks <- v$brain_masks[[j]]
    }

    if (combo_name %in% names(data)) { # if combo_name exists in data (e.g., not all studies have net)
      if (any(!is.na(data[[combo_name]][[effect_size_type]])) > 0) {  # data is not just NA

      # prep

      # if (plot_type == 'spatial') {
        # TODO: we probably don't even need a dedicated function for the spatial plots, just pass the relevant info
        pd_spatial <- prep_data_for_spatial_plot(data = data, brain_masks = brain_masks, study_details = study_details, combo_name = combo_name, mv_combo_name = mv_combo_name, estimate = effect_size_type, plot_info = this_plot_info)
      # } else {
        pd_simci <- prep_data_for_plot(data = data, study_details = study_details, combo_name = combo_name, mv_combo_name = mv_combo_name, estimate = effect_size_type, plot_info = this_plot_info)
      # }

      
      pd_list_spatial[[n_studies_in_pd_list]] <- pd_spatial
      pd_list_simci[[n_studies_in_pd_list]] <- pd_simci
      ld_list[[n_studies_in_pd_list]] <- get_summary_info(pd_simci$study_details, pd_simci$extra_study_details)
      
      n_studies_in_pd_list <- n_studies_in_pd_list + 1

      }
    }
  }


  # 2. Plot & Log

  if (make_plots) {
    if (length(pd_list_spatial) > 0) { # plot only if pd_list isn't empty

      # set up plot
      if (length(ld_list) > 1) {
        log_list[[i]] <- combine_summary_info(ld_list)
      } else {
        log_list[[i]] <- ld_list[[1]]
      }

      panel_list_simci[[i]] <- create_plots(pd_list_simci, plot_type = 'simci', add_description = add_plt_description, log_list[[i]])
      panel_list_spatial[[i]] <- create_plots(pd_list_spatial, plot_type = 'spatial', add_description = add_plt_description, log_list[[i]])
      
    }
  }

}

# panel_master_list_orig <- panel_list
# panel_master_list <- c(panel_list_simci, panel_list_spatial)
# panel_master_list <- panel_list_spatial

if (make_plots) {

  # General plot parameters
  # TODO: figure out what we want to set up here vs. to pass or set up in
  # create_plots, which gets passed to plot_sim_ci, etc.
  # Should at least set all panel / canvas dimensions here
  pp <- list()
  pp$width_per_panel <- 7 
  pp$height_per_panel <- 6 
  pp$res <- 100
  pp$units <- "in"
  pp$ncol <- 2
  pp$nrow <- 1
  pp$title_size <- 20

  for (i in 1:length(panel_list_simci)) {
  
    # plot multiple panels
  
    t <- list(panel_list_simci[[i]], panel_list_spatial[[i]])
    t_master_title <- t[[1]]$labels$title
    t[[1]]$labels$title <- ""
    t[[2]]$labels$title <- ""
    
    multi_plot <- ggarrange(plotlist = t, ncol = pp$ncol, nrow = pp$nrow)
    multi_plot <- annotate_figure(multi_plot,
                                  top = text_grob(t_master_title, face = "bold", size = pp$title_size))
    
    # print(multi_plot)
    
    # multi_plot <- ggarrange(plotlist = panel_master_list, ncol=pp$ncol, nrow=pp$row)
    # multi_plot <- ggarrange(plotlist = panel_master_list, ncol=pp$ncol, nrow=pp$row)
    # multi_plot <- annotate_figure(multi_plot,
    #                               top = text_grob(paste0(plot_type, ' (motion=', motion,")"), color = "black", face = "bold", size = 11))
  
  
  
    if (save_plots) {
      # if (save_plots) { # TODO: let's use ggsave(fn) instead of this png(fn) and below dev.off()
        
        # set up dir and file names

        if (plot_combination_style == 'meta') {
          grouping_var_str <- paste0('_', grouping_var)
        } else {
          grouping_var_str <- ''
        }
      
        study_name <- names(plot_info$idx)[[i]]
        out_dir <- paste0(out_dir_basename, effect_size_type, '/motion_', motion, '/pooling_', pooling, '/', plot_combination_style, grouping_var_str, '/')
        # out_name <- paste0(out_dir, study_name, '.svg')
        out_name <- paste0(out_dir, study_name, '.png')
        
        if (!dir.exists(out_dir)) {
          dir.create(out_dir, recursive = TRUE)
        }
        
        cat("Saving plots to...\n", out_name, "\n", sep = "")
        
        ggsave(out_name, plot = multi_plot, width = pp$width_per_panel * pp$ncol, height = pp$height_per_panel * pp$nrow, units = pp$units, dpi = pp$res, bg = "white", device = "png")
        # ggsave(out_name, plot = multi_plot, width = pp$width_per_panel * pp$ncol, height = pp$height_per_panel * pp$nrow, units = pp$units, device = "svg")
        
        # svg(out_name, width = pp$width * pp$ncol, height = pp$height * pp$nrow)
        # png(out_name, width = pp$width * pp$ncol, height = pp$height * pp$nrow, res = pp$res, units = pp$units)
        
        if (save_logs) {
          log_fn <- paste0(out_dir, plot_combination_style__fn, '_', study_name,'.txt')
          writeLines(unlist(lapply(log_list, function(x) c(x$title_text, x$bottom_text, ""))), log_fn)
        }
        
      
      
      # dev.off()
    }
  }

}


## close loop over plot types and styles
} # effect_size_type
} # pooling
} # plot_combination_style
} # plot_type
} # motion
} # grouping_var


# # correct meta
# 
# all_effect_size_types <- c('d') # 'd' or 'r_sq' c('d', 'r_sq', 'd.full_res')
# all_motion <- c('none', 'regression', 'threshold') #       # c('none', 'regression', 'threshold') # TODO: stat_control -> "...regression...$d", full_residualization -> "...regression...$d.full_res"
# all_grouping_var <- c('category', 'orig_stat_type')          # c('none', 'category', 'orig_stat_type') # used only for meta & overlap plots - TODO: separate out?
# 
# mv <- 'none'
# pooling <- 'net'
# 
# v_orig <- v
# 
# for (effect_size_type in all_effect_size_types) {
# for (motion in all_motion) {
# for (grouping_var in all_grouping_var) {
# for (i in 1:length(v$data)) {
#   
#   combo_name <- paste0('pooling.', pooling, '.motion.', motion, '.mv.', mv)
#   
#   if (length(v$data[[i]][[combo_name]][['d']]) > 55) {
#     print(i)
#     v$data[[i]][[combo_name]]$d <- v$data[[i]][[combo_name]]$d[1:55]
#     v$data[[i]][[combo_name]]$sim_ci_lb <- v$data[[i]][[combo_name]]$sim_ci_lb[1:55]
#     v$data[[i]][[combo_name]]$sim_ci_ub <- v$data[[i]][[combo_name]]$sim_ci_ub[1:55]
#     v$data[[i]][[combo_name]]$r_sq <- v$data[[i]][[combo_name]]$r_sq[1:55]
#     v$data[[i]][[combo_name]]$r_sq_sim_ci_lb <- v$data[[i]][[combo_name]]$r_sq_sim_ci_lb[1:55]
#     v$data[[i]][[combo_name]]$r_sq_sim_ci_ub <- v$data[[i]][[combo_name]]$r_sq_sim_ci_ub[1:55]
#   }
# }
# }
# }
# }
  

