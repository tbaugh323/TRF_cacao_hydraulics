#### Wrangling and working with sap flow data ####
# Sap flow data already processed in different project by Justin Beslity
library(tidyverse)
library(dygraphs)
theme_set(theme_bw())

#### R1132 ####

R1132_files <- list.files("data/sap_flow/raw_sapflow/R1132_files/")
R1132_raw <- grep("postprocess", R1132_files)
R1132_raw <- c(R1132_files[grep("postprocess", R1132_files)])
R1132_all <- data.frame()

for (i in 1:length(R1132_raw)) {
  
  # Extracting the label for each sensor
  id <- R1132_raw[i]
  tag <- sub("-.*", "", id)
  
  hold <- read_csv(paste0("data/sap_flow/raw_sapflow/R1132_files/", R1132_raw[i])) |> 
    mutate(location = tag) |> 
    relocate(Datetime, location)
  
  R1132_all <- bind_rows(hold, R1132_all)
}

R1132_clean <- R1132_all |> 
  mutate(Date = as.Date(Datetime),
         TreeID = "BioR1132",
         Datetime = lubridate::force_tz(Datetime, "America/Phoenix")) |> 
  rename(Tree_ID = TreeID)

# Writing out IN LOCAL TIME
write_csv(R1132_clean, "data/sap_flow/raw_sapflow/R1132-all-postprocess.csv")

#### R1192 ####

R1192_files <- list.files("data/sap_flow/raw_sapflow/R1192_files/")
R1192_raw <- grep("postprocess", R1192_files)
R1192_raw <- c(R1192_files[grep("postprocess", R1192_files)])
R1192_all <- data.frame()

for (i in 1:length(R1192_raw)) {
  
  # Extracting the label for each sensor
  id <- R1192_raw[i]
  tag <- sub("-.*", "", id)
  
  hold <- read_csv(paste0("data/sap_flow/raw_sapflow/R1192_files/", R1192_raw[i])) |> 
    mutate(location = tag) |> 
    relocate(Datetime, location)
  
  R1192_all <- bind_rows(hold, R1192_all)
}

R1192_clean <- R1192_all |> 
  mutate(Date = as.Date(Datetime),
         TreeID = "BioR1192",
         Datetime = lubridate::force_tz(Datetime, "America/Phoenix")) |> 
  rename(Tree_ID = TreeID)

# Writing out IN LOCAL TIME
write_csv(R1192_clean, "data/sap_flow/raw_sapflow/R1192-all-postprocess.csv")

#### R1171 ####

R1171_files <- list.files("data/sap_flow/raw_sapflow/R1171_files/")
R1171_raw <- grep("postprocess", R1171_files)
R1171_raw <- c(R1171_files[grep("postprocess", R1171_files)])
R1171_all <- data.frame()

for (i in 1:length(R1171_raw)) {
  
  # Extracting the label for each sensor
  id <- R1171_raw[i]
  tag <- sub("-.*", "", id)
  tag <- sub("^.*?_", "", tag)
  
  hold <- read_csv(paste0("data/sap_flow/raw_sapflow/R1171_files/", R1171_raw[i])) |> 
    mutate(Datetime = as.POSIXct(Datetime, format = "%m/%d/%Y %H:%M"),
           location = tag) |> 
    relocate(Datetime, location)
  
  R1171_all <- bind_rows(hold, R1171_all)
}

R1171_clean <- R1171_all |> 
  mutate(Date = as.Date(Datetime),
         TreeID = "BioR1171",
         Datetime = lubridate::force_tz(Datetime, "America/Phoenix")) |> 
  rename(Tree_ID = TreeID)

# Writing out IN LOCAL TIME
write_csv(R1171_clean, "data/sap_flow/raw_sapflow/R1171-all-postprocess.csv")

#### R1170 ####

R1170_files <- list.files("data/sap_flow/raw_sapflow/R1170_files/")
R1170_raw <- grep("postprocess", R1170_files)
R1170_raw <- c(R1170_files[grep("postprocess", R1170_files)])
R1170_all <- data.frame()

for (i in 1:length(R1170_raw)) {
  
  # Extracting the label for each sensor
  id <- R1170_raw[i]
  tag <- sub("-.*", "", id)
  
  hold <- read_csv(paste0("data/sap_flow/raw_sapflow/R1170_files/", R1170_raw[i])) |> 
    mutate(location = tag) |> 
    relocate(Datetime, location)
  
  R1170_all <- bind_rows(hold, R1170_all)
}

R1170_clean <- R1170_all |> 
  mutate(Date = as.Date(Datetime),
         TreeID = "BioR1170",
         Datetime = lubridate::force_tz(Datetime, "America/Phoenix")) |> 
  rename(Tree_ID = TreeID)

# Writing out IN LOCAL TIME
write_csv(R1132_clean, "data/sap_flow/raw_sapflow/R1170-all-postprocess.csv")


#### Merging together ####

R1171 <- read_csv("data/sap_flow/raw_sapflow/R1171-all-postprocess.csv") |> 
  mutate(Datetime = as.POSIXct(Datetime, tz = "America/Phoenix")) |> 
  rename(date = Date) |> 
  mutate(Tree_ID = paste0(Tree_ID, "_", location)) |> 
  dplyr::select(Datetime, date, Tree_ID, VhrmHRM5, VhrmHRM15, VhrmHRM25, 
                VhrmHRM35, Year, Month, Day, Hour, Minute, Second)

R1132 <- read_csv("data/sap_flow/raw_sapflow/R1132-all-postprocess.csv") |> 
  mutate(Datetime = lubridate::force_tz(Datetime, tzone = "America/Phoenix")) |> 
  mutate(date = as.Date(Datetime)) |> 
  dplyr::select(Datetime, date, Tree_ID, VhrmHRM5, VhrmHRM15, VhrmHRM25, 
                VhrmHRM35, Year, Month, Day, Hour, Minute, Second)

R1192 <- read_csv("data/sap_flow/raw_sapflow/R1192-all-postprocess.csv") |> 
  mutate(Datetime = lubridate::force_tz(Datetime, tzone = "America/Phoenix")) |> 
  mutate(date = as.Date(Datetime)) |> 
  dplyr::select(Datetime, date, , Tree_ID, VhrmHRM5, VhrmHRM15, VhrmHRM25, 
                VhrmHRM35, Year, Month, Day, Hour, Minute, Second)

R1170 <- read_csv("data/sap_flow/raw_sapflow/R1170-all-postprocess.csv") |> 
  mutate(Datetime = lubridate::force_tz(Datetime, tzone = "America/Phoenix")) |> 
  mutate(date = as.Date(Datetime),) |> 
  dplyr::select(Datetime, date, , Tree_ID, VhrmHRM5, VhrmHRM15, VhrmHRM25, 
                VhrmHRM35, Year, Month, Day, Hour, Minute, Second)

# Writing this out IN LOCAL TIME
sv_all <- rbind(R1171, R1132, R1192, R1170) |> 
  rename(dt = Datetime)

write_csv(sv_all, "data/sap_flow/sv_all.csv")
