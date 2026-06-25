#### Putting together LICOR-600 files ####
library(tidyverse)

# List all relevant datafiles
all_600_files <- list.files("data/licor600_data/")

# Initialize dataframe
all_measurements <- data.frame(Date = NA, Time = NA)

for (i in 1:length(all_600_files)) {
  # Read in each file, delete extra headers
  hold <- read_csv(paste0("data/licor600_data/", all_600_files[i]))
  hold <- hold[!grepl("lciSerialNumber", hold$configName),]
  hold <- hold[!grepl("configName", hold$configName),]
  
  # If the file got messed up and had a million rows, delete them
  if (length(colnames(hold)) > 200) {
    hold <- hold[,-(109:378)]
  }
  
  # If the file got messed up and the date is wrong, fix that
  if (grepl("/", hold$Date[1], fixed = TRUE)) {
    hold <- hold |> 
      mutate(`Date` = as.Date(`Date`, format = "%m/%d/%Y"))
  }
  
  # Label each measurement, format date and time
  hold_clean <- hold |> 
    mutate(individual = c(rep(1, 6), rep(2, 6), rep(1, 6), rep(2, 6), rep(3, 12), rep(4, 12)),
           canopy = c(rep("lower", 12), rep("upper", 12), rep("lower", 6), rep("upper", 6), rep("lower", 6), rep("upper", 6)),
           Date = as.Date(Date),
           Time = hms::as_hms(Time))
  
  # File got weird for this one and recorded the time wrong
  if (all_600_files[i] == "2026-06-23_early.csv") {
    hold_clean <- hold_clean |> 
      mutate(Time = Time - (3 * 3600 + 600),
             Time = hms::as_hms(Time))
  }
  
  # I suck and messed up the order on these ones
  if (all_600_files[i] == "2026-06-22_morning.csv" | all_600_files[i] == "2026-06-23_morning.csv" | all_600_files[i] == "2026-06-23_early.csv") {
    hold_clean <- hold_clean |> 
      mutate(individual = c(rep(1, 6), rep(2, 6), rep(2, 6), rep(1, 6), rep(3, 12), rep(4, 12)),
             canopy = c(rep("lower", 12), rep("upper", 12), rep("lower", 6), rep("upper", 6), rep("lower", 6), rep("upper", 6)),
             dark = as.character(dark))
  }
  
  # More of that
  if (all_600_files[i] == "2026-06-24_morning.csv") {
    hold_clean <- hold_clean |> 
      mutate(individual = c(rep(3, 6), rep(3, 6), rep(2, 6), rep(2, 6), rep(1, 6), rep(4, 6), rep(4, 6), rep(1, 6)),
             canopy = c(rep("lower", 6), rep("upper", 6), rep("lower", 6), rep("upper", 6), rep("upper", 6), rep("lower", 6), rep("upper", 6), rep("lower", 6)),
             dark = as.character(dark))
  }
  
  # Also messed up the order but had a good reason for it (predawn)
  if (all_600_files[i] == "2026-06-25_early.csv") {
    hold_clean <- hold_clean |> 
      mutate(individual = c(rep(1, 6), rep(1, 6), rep(2, 6), rep(2, 6), rep(3, 6), rep(3, 6), rep(4, 6), rep(4, 6)),
             canopy = c(rep("lower", 6), rep("upper", 6), rep("lower", 6), rep("upper", 6), rep("lower", 6), rep("upper", 6), rep("lower", 6), rep("upper", 6)),
             dark = as.character(dark))
  }
  
  # Finish cleaning up raw files (make dt, label periods)
  hold_clean <- hold_clean |> 
    mutate(dt = as.POSIXct(paste(Date, Time)),
           period = case_when(Time >= hms::as_hms("4:00:00") & Time < hms::as_hms("7:00:00") ~ "early",
                              Time >= hms::as_hms("7:00:00") & Time < hms::as_hms("10:00:00") ~ "morning",
                              Time >= hms::as_hms("10:00:00") & Time < hms::as_hms("14:00:00") ~ "noon",
                              Time >= hms::as_hms("14:00:00") ~ "afternoon"),
           month = as.numeric(substr(Date, 6, 7)),
           day = as.numeric(substr(Date, 9, 10)),
           condition = case_when(month == 06 & day <= 27 ~ "predrought",
                                 month == 06 & day > 27 ~ "drought",
                                 month == 07 & day <= 11 ~ "drought",
                                 month == 07 & day > 11 ~ "recovery")) |> 
    relocate(dt, individual, canopy, period, condition) |> 
    mutate(across(c(Observation:leaf_width, Fo:batt, rh_adj:Ble, flash_intensity:z_flr), as.numeric))
  
  # Put together
  all_measurements <- bind_rows(hold_clean, all_measurements)
}

# Write it out
write_csv(all_measurements, "data/all_licor600.csv")
