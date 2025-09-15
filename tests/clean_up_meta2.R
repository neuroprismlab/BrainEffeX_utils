# clean up meta 2 - rename all "multi_x" to "multi"

# load('v.RData')

grouping_vars <- c('meta_category') #,'meta_orig_stat_type')

for (i in seq_along(grouping_vars)) {
  
  meta_strings <- names(v[[grouping_vars[i]]]$data)
  # meta_strings <- meta_strings[grepl(problematic_reference, meta_strings)]
  
  for (j in seq_along(meta_strings)) {
    
    print(paste0('Processing ', meta_strings[j]))
    
    combo_strings <- names(v[[grouping_vars[i]]]$data[[meta_strings[j]]])
    
    # Find all fields ending with .mv.multi and those starting with .mv.multi. (e.g., abc.mv.multi.r)
    base_fields <- combo_strings[grepl("\\.mv\\.multi$", combo_strings)]
    trailing_fields <- combo_strings[grepl("\\.mv\\.multi\\.", combo_strings)]
    
    # Extract the base prefix for trailing fields (e.g., abc.mv.multi.r -> abc.mv.multi)
    trailing_bases <- gsub("(\\.mv\\.multi)\\..*$", "\\1", trailing_fields)
    
    # For each trailing field, check if a base field exists and compare
    for (k in seq_along(trailing_fields)) {
      base_name <- trailing_bases[k]
      trailing_name <- trailing_fields[k]
      
      if (base_name %in% base_fields) {
        data_base <- v[[grouping_vars[i]]]$data[[meta_strings[j]]][[base_name]]
        data_trailing <- v[[grouping_vars[i]]]$data[[meta_strings[j]]][[trailing_name]]
        comparison <- identical(data_base, data_trailing)
        cat(sprintf("Comparing %s and %s: %s\n", base_name, trailing_name, comparison))
        
        if (comparison) {
          
          # If they are identical, remove the trailing field
          if (length(v[[grouping_vars[i]]]$data[[meta_strings[j]]][[base_name]]$d) > 1) {
            cat("WARNING: Fine to remove, but problem - too long")
            # we're not removing the trailing one bc the base one has issues
          } else {
            # remove trailing one
            v[[grouping_vars[i]]]$data[[meta_strings[j]]][[trailing_name]] <- NULL
            cat(sprintf("Removed %s from %s\n", trailing_name, meta_strings[j]))
          }
          
        } else {
          
          # print both for comparison
          l_base <- length(v[[grouping_vars[i]]]$data[[meta_strings[j]]][[base_name]]$d)
          l_trail <- length(v[[grouping_vars[i]]]$data[[meta_strings[j]]][[trailing_name]]$d)

          if (l_base == 1) {
            str(v[[grouping_vars[i]]]$data[[meta_strings[j]]][[base_name]])
          } else {
            cat(v[[grouping_vars[i]]]$data[[meta_strings[j]]][[base_name]]$d[1])
            cat(paste0(", ", l_base, " elements"))
          }
          
          if (l_trail == 1) {
            str(v[[grouping_vars[i]]]$data[[meta_strings[j]]][[trailing_name]])
          } else {
            cat(v[[grouping_vars[i]]]$data[[meta_strings[j]]][[trailing_name]]$d[1])
            cat(paste0(", ", l_trail, " elements "))
          }
          
          # remove trailing one
          v[[grouping_vars[i]]]$data[[meta_strings[j]]][[trailing_name]] <- NULL

          
          
        }
        
      } else {
        
        # rename field to base name
        v[[grouping_vars[i]]]$data[[meta_strings[j]]][[base_name]] <- v[[grouping_vars[i]]]$data[[meta_strings[j]]][[trailing_name]]
        v[[grouping_vars[i]]]$data[[meta_strings[j]]][[trailing_name]] <- NULL
        
      }
      
    }
    
  }
  
}

grouping_var <- 'category'
v[[meta_str]]$study <- data.frame(group = character(0), ref = character(0), name = character(0))
all_categories <- unique(v$study$category)
all_refs <- unique(v$study$ref)
for (i in seq_along(all_categories)) {
  for (j in seq_along(all_refs)) {
    matching_idx__study <- which(v$study[[grouping_var]] == all_categories[i] & v$study$ref == all_refs[j])
    if (length(matching_idx__study) != 0) {
      v[[meta_str]]$study <- rbind(v[[meta_str]]$study, data.frame(group_level = all_categories[i], ref = all_refs[j], name = paste0(v$study$category[i], "_reference_", all_refs[j])))
    }
  }
}
# v$meta_category$data$cognitive_reference_voxel <- NULL


# any(!is.na(v$meta_category$data$`sex (demographic)_reference_shen_268`$pooling.none.motion.none.mv.multi.r$d))
# save(v, file = 'v.RData')

