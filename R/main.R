# create plots

## Plot params

do_multi <- FALSE
pooling <- 'none'     # 'none', 'net'
motion <- 'none'      # 'none', 'stat_control', "full_residualization", "thresholding"
add_plt_description <- TRUE
group_by <- 'none'    # 'none', 'category', 'orig_stat_type'

this_data_dir <- "/Users/stephanienoble/Library/CloudStorage/GoogleDrive-s.noble@northeastern.edu/My Drive/Lab/xMore/Software/scripts/R/myscripts/effect_size/BrainEffeX/data/"
out_dir_basename <- "/Users/stephanienoble/Library/CloudStorage/GoogleDrive-s.noble@northeastern.edu/My Drive/Lab/Tasks-Ongoing/K99/Effect_Size/manuscript/figures/pics/"


## Setup

# naming
combo_name <- paste0('pooling.', pooling, '.motion.', motion, '.mv.none')
mv_combo_name <- paste0('pooling.', pooling, '.motion.', motion, '.mv.multi')

if (pooling == 'net') {
  net_str=" - net"
} else {
  net_str=""
}


## Load data

# v <- load_data(this_data_dir)


## Setup Cont'd
# if group_by, average data within grouping
if (group_by != 'none') {
  v <- group_data(v, v$brain_masks, combo_name)
  group_by_str <- paste0(' - avg_by_', group_by)
} else {
  group_by_str <- ''
}

# for multi, create list of idx for each category, and also for each stat type (v$study$orig_stat_type)
idx_per_plot <- list()
idx_per_plot__master_grouping <- list()
if (do_multi) {
  multi_str <- 'multi'
  
  all_categories <- unique(v$study$category)
  all_stat_types <- unique(v$study$orig_stat_type)
  all_map_types <- unique(v$study$map_type)
  
  for (this_category in all_categories) {
    idx <- which(v$study$category == this_category)
    idx_per_plot[[this_category]] <- idx
    idx_per_plot__master_grouping[[this_category]] <- "Category"
  }
  # for (this_stat_type in all_stat_types) {
  #   idx <- which(v$study$orig_stat_type == this_stat_type)
  #   idx_per_plot[[this_stat_type]] <- idx
  #   idx_per_plot__master_grouping[[this_stat_type]] <- "Statistic"
  # }
  for (this_stat_type in all_stat_types) {
    for (this_map_type in all_map_types) { # each combination of stat_type and map_type
      idx <- which(v$study$orig_stat_type == this_stat_type & v$study$map_type == this_map_type)
      idx_per_plot[[paste0(this_stat_type, '.', this_map_type)]] <- idx
      idx_per_plot__master_grouping[[paste0(this_stat_type, '.', this_map_type)]] <- "Statistic x Map Type"
    }
  }
} else {
  

      for (i in 1:length(v$data)) {
        idx_per_plot[[names(v$data)[[i]]]] <- i
        idx_per_plot__master_grouping[[names(v$data)[[i]]]] <- "Study"
      }
  multi_str <- 'single'
}

# if (group_by != 'none') {
#   idx_per_plot <- list()
#   idx_per_plot__master_grouping <- list()
#   for (this_grouping in names(v$d_group)) {
#     if (combo_name %in% names(v$d_group[[this_grouping]])) {
#       for (i in 1:length(v$d_group)) {
#         idx_per_plot[[this_grouping]] <- c(i)
#         idx_per_plot__master_grouping[[this_grouping]] <- group_by
#       }
#     }
#   }
#   multi_str <- 'grouped'
# }


## Make Plots

for (this_group in names(idx_per_plot)) {
  
  idx <- idx_per_plot[[this_group]]
  
  pd_list <- list()
  n_studies_in_pd_list <- 1
  
  for (i in idx) {
    
    if (group_by == 'none') {
      data <- v$data[[i]]
      name <- names(v$data[i])
      study_details <- v$study[i, ]
      
    } else {
      
      data <- v$d_group[[i]]
      name <- names(v$d_group[i])
      study_details <- list()
      study_details$group <- this_group
      study_details$ref <- "TBD" # TODO:
      # study_details$ref <- v$study_group$ref[[i]] # TODO - test this
      
    }
    
    # prep + plot
    if (combo_name %in% names(data)) { # if combo_name exists in data (e.g., not all studies have net)
      
      pd <- prep_data_for_plot(data = data, name = name, study_details = study_details, group_by=group_by, combo_name = combo_name, mv_combo_name = mv_combo_name)
      pd_list[[n_studies_in_pd_list]] <- pd
      
      fn <- paste0('density_', this_group, '_', n_studies_in_pd_list, '.png')
      
      n_studies_in_pd_list <- n_studies_in_pd_list + 1
    }
  }
  
  # save if pd_list isn't empty
  
  if (length(pd_list) > 0) {
    
    # make out_dir_basename
    out_dir <- paste0(out_dir_basename,multi_str,'/',idx_per_plot__master_grouping[[this_group]],net_str,group_by_str,'/')
    dir.create(out_dir, recursive = TRUE)
    
    plot_density(pd_list, add_description = add_plt_description, save = TRUE, out_path = out_dir, file_name = fn)
    # plot_overlapping_sim_ci2(pd_list)
  }
  
}
  





  
  
  
  


# OLD - separate


  
# } else { # single plot
#   
#   
#   ## NEWER - IN PROGRESS
#   
#   for (this_study in names(idx_per_plot)) {
#     
#     if (group_by == 'none') {
#       data <- v$data[[this_study]]
#       name <- names(v$data[this_study])
#       study_details <- v$study[this_study, ]
#     } else {
#       data <- v$d_group[[this_study]]
#       name <- names(v$d_group[this_study])
#       study_details <- list()
#       study_details$group <- this_study
#       study_details$ref <- "TBD" # TODO - see above
#     }
#     
#     # prep + plot
#     if (combo_name %in% names(data)) { # if combo_name exists in data (e.g., not all studies have net)
#       
#       pd <- prep_data_for_plot(data = data, name = name, study_details = study_details, group_by=group_by, combo_name = combo_name, mv_combo_name = mv_combo_name)
#       
#       fn <- paste0('density_', this_study, '.png')
#       out_dir <- paste0(out_dir_basename,multi_str,'/',idx_per_plot__master_grouping[[this_study]],'/')
#       dir.create(out_dir, recursive = TRUE)
#       plot_density(pd, add_description = add_plt_description, save = TRUE, out_path = out_dir, file_name = fn)
#       
#     }
#     
#   }
#   
#   # ## OLD:
#   # 
#   # if (group_by == 'none') {
#   #   data <- v$data[[idx_single]]
#   #   name <- names(v$data[idx_single])
#   #   study_details <- v$study[idx_single, ]
#   # } else {
#   #   data <- v$d_group[[idx_single]]
#   #   name <- names(v$d_group[idx_single])
#   #   study_details <- v$study_group[[idx_single]] # TODO: test
#   # }
#   # 
#   # fn <- paste0('density_', idx_single, '.png')
#   # out_dir <- paste0(out_dir_basename,multi_str,'/',idx_per_plot__master_grouping[[this_group]],'/')
#   # dir.create(out_dir, recursive = TRUE)
#   # 
#   # # prep + plot
#   # pd <- prep_data_for_plot(data = data, name = name, study_details = study_details, group_by=group_by, combo_name = combo_name, mv_combo_name = mv_combo_name)
#   # plot_density(pd, add_description = add_plt_description, save = TRUE, out_path = out_dir, file_name = fn)
#   # 
#   # # plot_sim_ci2(pd)
#   
# }
#   
#   
# 
