###Create Dynamic Table for Submission Guidance
##Eilish Mac



#Install Packages/libraries
install.packages(dplyr)
install.packages(stringr)
install.packages(readr)
install.packages(tibble)
install.packages (janitor)
library(dplyr)
library(stringr)
library(readr)
library(tibble)
library (janitor)

# Function to find the latest file based on modification time
find_latest_file <- function(file_path, file_name_pattern, suffix_pattern = ".csv") {
  files <- list.files(file_path, pattern = suffix_pattern, full.names = TRUE)
  files <- files[str_detect(files, file_name_pattern)]
  
  if(length(files) == 0) {
    return(NULL)  # No matching files found
  }
  
  # Get file info to find the latest by modification time
  file_info <- file.info(files)
  latest_file <- rownames(file_info)[which.max(file_info$mtime)]
  
  return(latest_file)}


### set your folder path where files are located
path_loaded <- '/conf/EIC/Data Submission/Reference Files/Loaded/'

# MEASURE Doc - Find latest EIC_MEAS_ file and read specific columns
### pattern to identify your MEAS files
meas_pattern <- "EIC_MEAS_" 
# Find latest MEAS file
latest_meas_file <- find_latest_file(path_loaded, meas_pattern)
if(!is.null(latest_meas_file)) {
  
  #Rread and clean column names
  meas_data <- read_csv(latest_meas_file,skip = 1) %>% 
    clean_names() %>% 
    select(measure_id, measure_name, measure_frequency, column_number, column_name)
  
} else {
  warning("No MEAS file found!")
}

#Find Ref Point file
ref_point_pattern <- "EIC_REFPOINT_"
latest_ref_point_file <- find_latest_file(path_loaded, ref_point_pattern)
if(!is.null(latest_ref_point_file)) {
  
  #Read and clean column names  
  ref_point_data <- read_csv(latest_ref_point_file, skip = 1) %>% 
    clean_names() %>%
    rename(measure_id = measureid) %>% 
    select(measure_id, refpoint)
} else {
  warning("No REF file found!")
}



#Find Nurse Family Measure file
nurse_fam_meas_pattern <- "NURSEFAMMEAS_"
latest_nurse_fam_meas_file <- find_latest_file(path_loaded, nurse_fam_meas_pattern)
if(!is.null(latest_nurse_fam_meas_file)) {
  
  #Read and clean column names  
  nursefam_data <- read_csv(latest_nurse_fam_meas_file, skip = 1) %>% 
    clean_names() %>%
    select(measure_id, nurse_family, start_date)
} else {
  warning("No REF file found!")
}
#Join data frames together
if (exists("meas_data") && exists("ref_point_data") && exists("nursefam_data")) {
  joined_data <- meas_data %>%
    left_join(ref_point_data, by = "measure_id") %>%
    left_join(nursefam_data, by = "measure_id")
  
  # Save file out
  output_folder <- path_loaded
  output_filename <- "dynamic_table.csv" 
  output_filepath <- file.path(output_folder, output_filename)
  
  # Use full file path here!
  write_csv(joined_data, output_filepath)
  message("Joined data saved.")
} else {
  warning("One or more data frames are missing. Check input files.")
}

system('git config --global user.name "Eilish Mac"')
system('git config --global user.email "eilishmackinnon@phs.scot"') 