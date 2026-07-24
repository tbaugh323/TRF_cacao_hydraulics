#### Wrangling and working with sap flow data ####
# Sap flow data already processed in different project by Justin Beslity
library(tidyverse)
library(dygraphs)
theme_set(theme_bw())

#### Reading ####

tree_2_files <- list.files("data/sap_flow/raw_sapflow/R1171_files/")
tree_2_raw <- grep("R1171", tree_2_files)
tree_2_raw <- c(tree_2_files[grep("R1171", tree_2_files)])
tree_2_all <- data.frame()

for (i in 1:length(tree_2_raw)) {
  
  # Extracting the label for each sensor
  id <- tree_2_raw[i]
  tag <- id |> str_extract("(?<=_).*(?=\\-)")
  
  hold <- read_csv(paste0("data/sap_flow/raw_sapflow/R1171_files/", tree_2_raw[i])) |> 
    mutate(location = tag) |> 
    relocate(Datetime, location)
  
  tree_2_all <- bind_rows(hold, tree_2_all)
}

tree_2_clean <- tree_2_all |> 
  mutate(Date = as.Date(Datetime),
         TreeID = "BioR1171",
         Datetime = lubridate::force_tz(Datetime, "America/Phoenix")) |> 
  rename(Tree_ID = TreeID)

# Writing out IN LOCAL TIME
write_csv(tree_2_clean, "data/sap_flow/raw_sapflow/R1171-postprocess.csv")

