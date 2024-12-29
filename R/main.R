# create plots

# plot params
do_single <- FALSE
group_by <- 'none'
combo_name <- "pooling.none.motion.none.mv.none"
mv_combo_name <- "pooling.none.motion.none.mv.multi"
this_data_dir <- "/Users/stephanienoble/Library/CloudStorage/GoogleDrive-s.noble@northeastern.edu/My Drive/Lab/xMore/Software/scripts/R/myscripts/effect_size/BrainEffeX/data/"
out_dir <- "/Users/stephanienoble/Library/CloudStorage/GoogleDrive-s.noble@northeastern.edu/My Drive/Lab/Tasks-Ongoing/K99/Effect_Size/manuscript/figures/pics3/"

idx_single <- 1

# v <- load_data(this_data_dir)

# take the average
if (group_by != 'none') {
  v <- group_data(v, v$brain_masks, combo_name)
}

if (do_single) {
  
  if (group_by == 'none') {
    pd <- prep_data_for_plot(data = v$data[[idx_single]], name = names(v$data[idx_single]), study_details = v$study[idx_single, ], group_by='none', combo_name = combo_name, mv_combo_name = mv_combo_name)
  } else {
    pd <- prep_data_for_plot(data = v$d_group[[idx_single]], name = names(v$d_group[idx_single]), study_details = v$study[idx_single, ], group_by='category', combo_name = combo_name, mv_combo_name = mv_combo_name)
  }
  
  plot_density(pd)
  # plot_sim_ci2(pd)
  
  
} else {
  
  # let's choose a group to plot within
  # idx_multiple <- which(v$study$category == "demographic")
  
  # loop over each group
  
  # create list of idx for each category, and also for each stat type (v$study$orig_stat_type)
  
  all_categories <- unique(v$study$category)
  all_stat_types <- unique(v$study$orig_stat_type)
  all_map_types <- unique(v$study$map_type)
  
  idx_multiple_list_of_lists <- list()
  for (this_category in all_categories) {
    idx_multiple <- which(v$study$category == this_category)
    idx_multiple_list_of_lists[[this_category]] <- idx_multiple
  }
  for (this_stat_type in all_stat_types) {
    idx_multiple <- which(v$study$orig_stat_type == this_stat_type)
    idx_multiple_list_of_lists[[this_stat_type]] <- idx_multiple
  }
  
  # list unique group for each combination of stat_type and map_type
  for (this_stat_type in all_stat_types) {
    for (this_map_type in all_map_types) {
      idx_multiple <- which(v$study$orig_stat_type == this_stat_type & v$study$map_type == this_map_type)
      idx_multiple_list_of_lists[[paste0(this_stat_type, '.', this_map_type)]] <- idx_multiple
    }
  }
  
  # make out dir
  out_dir_overlapping <- paste0(out_dir,'overlapping/')
  dir.create(out_dir_overlapping, showWarnings = FALSE)
  
  for (this_group in names(idx_multiple_list_of_lists)) {
    
    idx_multiple <- idx_multiple_list_of_lists[[this_group]]
    
    pd_list <- list()
    it <- 1
    
    for (i in idx_multiple) {
      
      if (group_by == 'none') {
        pd <- prep_data_for_plot(data = v$data[[i]], name = names(v$data[i]), study_details = v$study[i, ], group_by='none', combo_name = combo_name, mv_combo_name = mv_combo_name)
      } else {
        pd <- prep_data_for_plot(data = v$d_group[[i]], name = names(v$d_group[i]), study_details = v$study[i, ], group_by='category', combo_name = combo_name, mv_combo_name = mv_combo_name)
      }
      
      pd_list[[it]] <- pd
      fn <- paste0('density_by_', this_group, '_', it, '.png')
      it <- it + 1
    }
    
    # if pd_list isn't empty
    if (length(pd_list) > 0) {
      plot_overlapping_densities(pd_list, save = TRUE, out_path = out_dir_overlapping, file_name = fn)
      # plot_overlapping_sim_ci2(pd)
    }
    
  }
  
}



