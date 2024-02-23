{
  # This is an equivalent but user-friendly version of 00_Run-script, with Step 1-4 being ommitted
  # The purpose is to allow users without much coding experience to produce HEBs (Step 5)
  # Step 5. Illustrate ----
  rm(list = ls())

  library(rstudioapi)
  library(dplyr)
  library(stringr)
  library(svDialogs)

  directory <- dirname(getSourceEditorContext()$path) %>% str_extract(pattern = "^(.*?)mapping_analysis")

  {
    data_path <- file.path(directory, "00_Data xlsx/")
    funcon_path <- file.path(directory, "03_Best-script/00_FunCon/")
    preprocessing_path <- file.path(directory, "03_Best-script/00_Data-Preprocessing/")
    script_path <- file.path(directory, "03_Best-script/01_Hierarchical-Edge-Bundling/")
    result_path <- file.path(directory, "04_Results/")
    descriptives_path <- file.path(directory, "05_Descriptives/")
  }


  while (TRUE) {
    steps <- c("Trend of the whole literature", "Trend of some entries", "Compare two entries")
    scripts <- c("05_Connection-Group.R", "05_Connection-GroupFiltered.R", "05_Connection-Filtered.R")
    lastresult <- dlg_list(choices = steps, title = "What do you want to do?")$res
    if (length(lastresult) == 0) {
      if (okCancelBox("Pick one!") == FALSE) {
        break
      }
    } else if (length(lastresult) == 1) {
      if (okCancelBox(c("Your choice:", lastresult)) == FALSE) {
        break
      }

      # Load necessary packages ----
      source(
        local = FALSE, echo = TRUE, print.eval = TRUE, verbose = FALSE,
        file = file.path(directory, "03_Best-script", "00_Data-Preprocessing", "00_Script_Packages.R")
      )
      # Run script ----
      print(scripts[which(steps == lastresult)])
      source(
        local = FALSE, echo = TRUE, print.eval = TRUE, verbose = FALSE,
        file = file.path(script_path, scripts[which(steps == lastresult)])
      )
    }
  }
}

