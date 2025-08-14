###Create Dynamic Table for Submission Guidance
##Eilish Mac



#Install Packages/libraries
install.packages(dplyr)
install.packages(stringr)
install.packages(readr)
install.packages(tibble)
install.packages (janitor)
install.packages (tidyr)
install.packages(flextable)
library(flextable)
library(tidyr)
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


# Folder path where files are located
path_loaded <- '/conf/EIC/Data Submission/Reference Files/Loaded/'

# Find latest EIC_NURSEFAMMEAS_ file and read specific columns
# pattern to identify your MEAS files
nurse_fam_meas_pattern <- "EIC_NURSEFAMMEAS_" 
# Find latest Nurse_Fam_Meas file
latest_nurse_fam_meas_file <- find_latest_file(path_loaded, nurse_fam_meas_pattern)
if(!is.null(latest_nurse_fam_meas_file)) 
  
  # Read and clean column names
  nurse_fam_meas_data <- read_csv(latest_nurse_fam_meas_file,skip = 1) %>% 
  clean_names() %>%
  mutate(value = "x") %>%
  pivot_wider(
    names_from = nurse_family,
    values_from = value,
    values_fill = ""
  ) %>%
  arrange(measure_id)


# Flextable for prettiness
flextable_format <- function(data) {
  data %>%
    flextable() |>
    bold(part = "header") %>%
    bg(bg = "#43358B", part = "header") %>%
    color(color = "white", part = "header") %>%
    align(align = "left", part = "header") %>%
    valign(valign = "center", part = "header") %>%
    valign(valign = "top", part = "body") %>%
    colformat_num(big.mark = ",") %>%
    fontsize(size = 12, part = "all") %>%
    font(fontname = "Arial", part = "all") %>%
    border(border = fp_border_default(color = "#000000", width = 0.5), part = "all") |>
    autofit()
}
# Remove unwanted column and tidy
nurse_fam_meas_data <- nurse_fam_meas_data|>
  select(-end_date) |>
  rename_with(~ str_replace_all(., "_", " ") %>%
                str_to_title(), .cols = everything())

# Apply the formatting
ft <- flextable_format(nurse_fam_meas_data)

# Print the formatted flextable in RStudio Viewer or RMarkdown
ft


# Find latest EIC_REFPOINT_ file and read specific columns
# pattern to identify your REFPOINT files
refpoint_pattern <- "EIC_REFPOINT_" 
# Find latest Ref_Point file
latest_ref_point_file <- find_latest_file(path_loaded, refpoint_pattern)
if(!is.null(latest_ref_point_file)) 
  
  # Read and clean column names
  ref_point_data <- read_csv(latest_ref_point_file,skip = 1) %>% 
  clean_names() %>%
  select(measureid, refpoint) %>%
  distinct()


# Flextable for prettiness
flextable_format <- function(data) {
  data %>%
    flextable() |>
    bold(part = "header") %>%
    bg(bg = "#43358B", part = "header") %>%
    color(color = "white", part = "header") %>%
    align(align = "left", part = "header") %>%
    valign(valign = "center", part = "header") %>%
    valign(valign = "top", part = "body") %>%
    colformat_num(big.mark = ",") %>%
    fontsize(size = 12, part = "all") %>%
    font(fontname = "Arial", part = "all") %>%
    border(border = fp_border_default(color = "#000000", width = 0.5), part = "all") |>
    autofit()
}
# Remove unwanted column and tidy
ref_point_data <- ref_point_data|>
  rename_with(~ str_replace_all(., "_", " ") %>%
                str_to_title(), .cols = everything())

# Apply the formatting
ft <- flextable_format(ref_point_data)

# Print the formatted flextable in RStudio Viewer or RMarkdown
ft



# Find latest EIC_MEAS_ file and read specific columns
# pattern to identify your MEAS files
meas_pattern <- "EIC_MEAS_" 
# Find latest Nurse_Fam_Meas file
latest_meas_file <- find_latest_file(path_loaded, meas_pattern)
if(!is.null(latest_meas_file)) 
  
  
  # Read and clean column names
  meas_data <- read_csv(latest_meas_file,skip = 1) %>% 
  clean_names() %>%
  select(measure_id, measure_name, measure_frequency, column_number,column_name, data_type)


# Flextable for prettiness
flextable_format <- function(data) {
  data %>%
    flextable() |>
    bold(part = "header") %>%
    bg(bg = "#43358B", part = "header") %>%
    color(color = "white", part = "header") %>%
    align(align = "left", part = "header") %>%
    valign(valign = "center", part = "header") %>%
    valign(valign = "top", part = "body") %>%
    colformat_num(big.mark = ",") %>%
    fontsize(size = 12, part = "all") %>%
    font(fontname = "Arial", part = "all") %>%
    border(border = fp_border_default(color = "#000000", width = 0.5), part = "all") |>
    autofit()
}
# Remove unwanted column and tidy
meas_data <- meas_data|>
  rename_with(~ str_replace_all(., "_", " ") %>%
                str_to_title(), .cols = everything())

# Apply the formatting
ft <- flextable_format(meas_data)

# Print the formatted flextable in RStudio Viewer or RMarkdown
ft

