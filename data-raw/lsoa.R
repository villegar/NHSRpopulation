# Load packages ----
# library(readxl)
# library(tidyverse)
# library(here)
# library(janitor)

# Source -----

# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates
# Helper functions -----

tidy_lsoa_data <- function(data,
                           la_year,
                           lsoa_year,
                           est_year,
                           genre) {
  data %>%
    # pivot the wide data into long format
    tidyr::pivot_longer(cols = dplyr::starts_with("x"),
                        names_to = "age",
                        values_to = "n") %>%
    # update ages to keep numbers only
    dplyr::mutate(age = as.numeric(gsub("\\D", "", age))) %>%
    # select a subset of the variables
    dplyr::select(lsoa_code,
                  lsoa_name,
                  la_code = paste0("la_code_", la_year, "_boundaries"),
                  la_name = paste0("la_name_", la_year, "_boundaries"),
                  age,
                  n) %>%
    # insert variables of the LA, LSOA and estimation year:
    dplyr::mutate(lsoa_year = lsoa_year,
                  la_year = la_year,
                  est_year = est_year,
                  gender = genre) %>%
    dplyr::relocate(lsoa_year,
                    lsoa_code,
                    lsoa_name,
                    la_year,
                    la_code,
                    la_name,
                    age,
                    gender,
                    est_year,
                    n)
}

load_data_frames <- function(URL,
                             la_year,
                             lsoa_year = la_year,
                             est_year = la_year,
                             dl = tempfile("lsoa_pop", tempdir()),
                             skip = 4,
                             zip = TRUE) {
  # download dataset
  download.file(URL, dl, mode = "wb")
  # set up output directory
  out_dir <- paste0(tempdir(), "//lsoa_pop")
  if (!dir.exists(out_dir))
    dir.create(out_dir, recursive = TRUE)
  if (zip) {
    # unzip dataset
    utils::unzip(dl, junkpaths = TRUE, exdir = out_dir)
    # list contents
    files <- dir(out_dir, full.names = TRUE, pattern = "xlsx$")
  } else {
    files <- dl
  }

  # list sheets
  lsoa_pop_sheets <- readxl::excel_sheets(files)

  # estimates for female
  df_raw_f <- readxl::read_excel(
    files,
    sheet = lsoa_pop_sheets[grepl("Females", lsoa_pop_sheets)],
    skip = skip
  ) %>%
    janitor::clean_names() %>% # clean variable names
    tidy_lsoa_data(la_year, lsoa_year, est_year, "f")

  # estimates for male
  df_raw_m <- readxl::read_excel(
    files,
    sheet = lsoa_pop_sheets[grepl("Males", lsoa_pop_sheets)],
    skip = skip
  ) %>%
    janitor::clean_names() %>% # clean variable names
    tidy_lsoa_data(la_year, lsoa_year, est_year, "m")

  # join data and arrange by code
  lsoa <- df_raw_f %>%
    dplyr::bind_rows(df_raw_m) %>%
    dplyr::arrange(lsoa_year, lsoa_code, est_year, gender)

  # delete temporary files
  unlink(dl, recursive = TRUE)
  unlink(out_dir, recursive = TRUE)

  return(lsoa)
}

# LSOA 2019 scores ----
lsoa_2019 <- load_data_frames("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2019sape22dt2/sape22dt2mid2019lsoasyoaestimatesunformatted.zip",
                              la_year = 2019,
                              lsoa_year = 2019,
                              est_year = 2019)
# LSOA 2020 scores ----
lsoa_2020 <- load_data_frames("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2020sape23dt2/sape23dt2mid2020lsoasyoaestimatesunformatted.xlsx",
                              la_year = 2018,
                              lsoa_year = 2020,
                              est_year = 2020,
                              zip = FALSE)

lsoa <- lsoa_2019 # original version (as of 2023-11-19)
usethis::use_data(lsoa, overwrite = TRUE)

