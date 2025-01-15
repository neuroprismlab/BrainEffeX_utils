#' Load and Process Data for the BrainEffeX Shiny Application
#'
#' This function loads preprocessed data files, including brain masks, template 
#' NIfTI files, and associated study data. It processes the data to ensure compatibility 
#' with downstream applications. The data directory should contain a plotting subdirectory.
#'
#' @param data_dir A character string specifying the directory where the data files are stored. 
#'                 Defaults to `"data/"`.
#'
#' @return A named list containing the following elements:
#' \describe{
#'   \item{study}{A data frame with all character entries converted to lowercase.}
#'   \item{brain_masks}{A list of brain masks with lowercase names.}
#'   \item{data}{Effect maps for each study.}
#'   \item{template}{A template NIfTI object for visualization}
#'   \item{anatomical}{An anatomical NIfTI object for visualization.}
#' }
#'
#' @examples
#' \dontrun{
#'   loaded_data <- load_data("data/")
#'   str(loaded_data$study)
#' }
#'
#' @import oro.nifti
#' @export
load_data <- function(data_dir = "data/") {
  # Identify the data file matching the pattern
  data_file <- list.files(path = data_dir, pattern = "combined_data_", recursive = TRUE)
  
  # Print status for debugging
  message("Loading data...")
  print(str(data_file))
  
  # Load the RData file: assumes it contains brain_masks, data (formerly sim_ci), and study
  load(paste0(data_dir, data_file))
  
  # Load the template NIfTI file for visualization
  template <- readNIfTI(paste0(data_dir, "plotting/template_nifti"), verbose = FALSE)
  
  # Load the anatomical NIfTI file for visualization
  anatomical <- readNIfTI(paste0(data_dir, "plotting/MNI152_T1_2mm_Brain.nii.gz"), verbose = FALSE)

  # Load phen_key data dictionary data
  phen_keys <- read.csv(paste0(data_dir, 'plotting/phen_key.csv'))
  
  # Process 'study' data: convert all character columns to lowercase
  study <- data.frame(lapply(study, function(x) {
    if (is.character(x)) {
      return(tolower(x))
    } else {
      return(x)
    }
  }))
  
  # Ensure all list names in 'data' are lowercase
  names(data) <- tolower(names(data))
  
  # Ensure all list names in 'brain_masks' are lowercase
  names(brain_masks) <- tolower(names(brain_masks))
  
  # Return the processed data as a named list
  return(list(
    study = study,
    brain_masks = brain_masks,
    data = data,
    template = template,
    anatomical = anatomical,
    phen_keys = phen_keys
  ))
}
