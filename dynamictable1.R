

#v2
#Install Packages/libraries
install.packages(dplyr)
install.packages(stringr)
install.packages(readr)
install.packages(tibble)
install.packages (janitor)
install.packages (tidyr)
install.packages(flextable)
install.packages(officer)
library(officer)
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
  
  file_info <- file.info(files)
  latest_file <- rownames(file_info)[which.max(file_info$mtime)]
  
  return(latest_file)
}


# File paths
path_loaded <- "/conf/EIC/Data Submission/Reference Files/Loaded/"
archive_nurse_fam_path_loaded <- "/conf/EIC/Data Submission/Reference Files/R Process/Measure Lookups/"


# Read latest nurse_fam_meas data
nurse_fam_meas_pattern <- "EIC_NURSEFAMMEAS_"
latest_nurse_fam_meas_file <- find_latest_file(path_loaded, nurse_fam_meas_pattern)

if (!is.null(latest_nurse_fam_meas_file)) {
  nurse_fam_meas_data <- read_csv(latest_nurse_fam_meas_file, skip = 1) %>% 
    clean_names()
}

archive_nurse_fam_meas_pattern <- "Archived nurse family rows"
latest_archive_nurse_fam_meas_file <- find_latest_file(archive_nurse_fam_path_loaded, archive_nurse_fam_meas_pattern)

if (!is.null(latest_archive_nurse_fam_meas_file)) {
  archive_nurse_fam_meas_data <- read_csv(latest_archive_nurse_fam_meas_file) %>% 
    clean_names() %>%
    distinct(measure_id, nurse_family)
} else {
  archive_nurse_fam_meas_data <- tibble(measure_id = character(), nurse_family = character())  # empty tibble if archive not found
}

# Now you can safely use nurse_fam_meas_data and archive_nurse_fam_meas_data
nurse_fam_meas_data <- nurse_fam_meas_data %>%
  anti_join(archive_nurse_fam_meas_data, by = c("measure_id", "nurse_family")) %>%
  group_by(measure_id, nurse_family) %>%
  summarise(.groups = "drop") %>%
  mutate(value = "x") %>%
  pivot_wider(
    names_from = nurse_family,
    values_from = value,
    values_fill = ""
  ) %>%
  arrange(measure_id)

# Measures to remove
measures_to_remove <- c("PLE1", "PLE2", "ALR", "MLR", "NOABS", "SAR", "COVID", "OAR", "SLR", "SSUEO", "SSUBA", "SSUA", "SSUB", "VAC", "CR1")
nurse_fam_meas_data <- nurse_fam_meas_data|>
  rename_with(~ str_replace_all(., "_", " ") %>%
                str_to_title(), .cols = everything()) |>
  rename("NHS24" = "Nhs24") |>
  filter(!`Measure Id` %in% measures_to_remove)

# Flextable formatting function
flextable_format <- function(data) {
  data %>%
    flextable() %>%
    bold(part = "header") %>%
    bg(bg = "#43358B", part = "header") %>%
    color(color = "white", part = "header") %>%
    align(align = "left", part = "header") %>%
    valign(valign = "center", part = "header") %>%
    valign(valign = "top", part = "body") %>%
    colformat_num(big.mark = ",") %>%
    fontsize(size = 12, part = "all") %>%
    font(fontname = "Arial", part = "all") %>%
    border(border = fp_border_default(color = "#000000", width = 0.1), part = "all")}



# Apply flextable formatting
ft_nurse_fam <- flextable_format(nurse_fam_meas_data)
ft_nurse_fam

# === Insert into existing Word doc at bookmark ===
doc_path <- file.path(path_loaded, "20250702-CAIR-Submission-Guidance-V3.3.docx")
doc <- read_docx(doc_path)


# Insert table into Word document at bookmark in landscape
doc <- doc %>%
  cursor_bookmark("Appendix5") %>%
  body_add_par("", style = "Normal") %>%
  body_end_section_landscape() %>%           # Switch to landscape orientation
  body_add_flextable(ft_nurse_fam)


# Save the updated Word document
print(doc, target = file.path(path_loaded, "updated submission guidance.docx"))

