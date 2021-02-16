rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run.
cat("\f") # clear console when working in RStudio

# load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.

# load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library("magrittr") #Pipes
library("ggplot2")  # graphs
requireNamespace("dplyr")

# declare-globals ----------------------------------------------------------
path_file <- "./data-public/raw/Tableau 10 Training Practice Data.xlsx"
# define-custom-functions --------------------------------------------------

# load-data ----------------------------------------------------------------
sheet_names <- readxl::excel_sheets(path_file)

dto <- list()
for(sheet_i in sheet_names){
   # i <- sheet_names[1]
  dto[[sheet_i]] <- readxl::read_xlsx(path_file,sheet = sheet_i)
  }

# inspect-data -------------------------------------------------------------
dto$`18 - Motor Vehicle Deaths`


# tweak-data ---------------------------------------------------------------
ds <- dto$`18 - Motor Vehicle Deaths`

names(ds)

# basic-table --------------------------------------------------------------


# basic-graph --------------------------------------------------------------


# save-to-disk -------------------------------------------------------------

