#### Wrangling LICOR600 data for T cacao ####
library(tidyverse)
theme_set(theme_bw())
set.seed(323)

#### Reading ####

all_measurements <- read_csv("data/all_licor600.csv") |> 
  mutate(dt = as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%OS"))
# Works better if you just run the wrangle-concatenate script

# Going to use the default LICOR config for reproducability

clean_all_data <- all_measurements |> 
  filter(configAuthor == "LI-COR Default") |> 
  mutate(canopy = ifelse(canopy == "lower", "Lower", "Upper"),
         individual = as.character(individual))

#### Making cleaner data ####

# Summarizing by individual
sum_by_individual <- clean_all_data |> 
  group_by(individual, canopy, period, Date) |> 
  summarize(gsw_m = mean(gsw, na.rm = TRUE),
            gsw_sd = sd(gsw, na.rm = TRUE),
            gbw_m = mean(gbw, na.rm = TRUE),
            gtw = mean(gtw, na.rm = TRUE),
            E_apparent_m = mean(E_apparent, na.rm = TRUE),
            VPcham_m = mean(VPcham, na.rm = TRUE),
            VPref_m = mean(VPref, na.rm = TRUE),
            VPleaf_m = mean(VPleaf, na.rm = TRUE),
            VPDleaf_m = mean(VPDleaf, na.rm = TRUE),
            Fs_m = mean(Fs, na.rm = TRUE),
            `Fm'_m` = mean(`Fm'`, na.rm = TRUE),
            `Fm'_sd` = sd(`Fm'`, na.rm = TRUE),
            rh_s_m = mean(rh_s, na.rm = TRUE),
            rh_r_m = mean(rh_r, na.rm = TRUE),
            Tleaf_m = mean(Tleaf, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(time = case_when(period == "early" ~ hms::as_hms("6:00:00"),
                          period == "morning" ~ hms::as_hms("8:00:00"),
                          period == "midday" ~ hms::as_hms("12:00:00"),
                          period == "afternoon" ~ hms::as_hms("18:30:00")),
         dt = as.POSIXct(paste(Date, time))) |> 
  relocate(dt, Date, time)

# Summarizing without individuals (by canopy and time only)

sum_by_canopy <- smaller_measurements |> 
  mutate(gsw_raw = gsw,
         gsw_zeroed = ifelse(gsw <= 0, 0, gsw),
         gsw_rem = ifelse(gsw <= 0, NA, gsw)) |> 
  group_by(canopy, period, Date) |> 
  summarize(gsw_raw_m = mean(gsw_raw, na.rm = TRUE),
            gsw_zeroed_m = mean(gsw_zeroed, na.rm = TRUE),
            gsw_rem_m = mean(gsw_rem, na.rm = TRUE),
            gsw_sd = sd(gsw, na.rm = TRUE),
            gbw_m = mean(gbw, na.rm = TRUE),
            gtw = mean(gtw, na.rm = TRUE),
            E_apparent_m = mean(E_apparent, na.rm = TRUE),
            VPcham_m = mean(VPcham, na.rm = TRUE),
            VPref_m = mean(VPref, na.rm = TRUE),
            VPleaf_m = mean(VPleaf, na.rm = TRUE),
            VPDleaf_m = mean(VPDleaf, na.rm = TRUE),
            Fs_m = mean(Fs, na.rm = TRUE),
            `Fm'_m` = mean(`Fm'`, na.rm = TRUE),
            `Fm'_sd` = sd(`Fm'`, na.rm = TRUE),
            rh_s_m = mean(rh_s, na.rm = TRUE),
            rh_r_m = mean(rh_r, na.rm = TRUE),
            Tleaf_m = mean(Tleaf, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(time = case_when(period == "early" ~ hms::as_hms("6:00:00"),
                          period == "morning" ~ hms::as_hms("8:00:00"),
                          period == "midday" ~ hms::as_hms("12:00:00"),
                          period == "afternoon" ~ hms::as_hms("18:30:00")),
         dt = as.POSIXct(paste(Date, time))) |> 
  pivot_longer(cols = c(gsw_raw_m, gsw_zeroed_m, gsw_rem_m),
               names_to = "gsw_var",
               values_to = "gsw_m") |> 
  relocate(dt, Date, time, canopy, period, gsw_var, gsw_m)

# Marking rain events
rain_df <- data.frame(date = c(as.Date(c("2026-06-23", "2026-06-25", "2026-06-30"))),
                      time = c(rep("00:00:00", 3))) |> 
  mutate(dt = as.POSIXct(paste(date, time)))

rects_rain <- data.frame(date = c(as.Date(c("2026-06-23", "2026-06-25", "2026-06-30"))),
                         time_start = c(rep("00:00:00", 3)),
                         time_end = c(rep("2:00:00", 3))) |> 
  mutate(dt_start = as.POSIXct(paste(date, time_start)),
         dt_end = as.POSIXct(paste(date, time_end)),
         date = as.Date(date))

# Bonus... rects for all times of day
nuber <- length(unique(smaller_measurements$Date))

rects <- data.frame(date = rep(unique(smaller_measurements$Date), 5),
                    period = c(rep("early", nuber),
                               rep("morning", nuber),
                               rep("midday", nuber),
                               rep("afternoon", nuber),
                               rep("night", nuber)),
                    time_start = c(rep("4:00:00", nuber),
                                   rep("7:00:00", nuber),
                                   rep("10:00:00", nuber),
                                   rep("14:00:00", nuber),
                                   rep("20:00:00", nuber)),
                    time_end = c(rep("7:00:00", nuber),
                                 rep("10:00:00", nuber),
                                 rep("14:00:00", nuber),
                                 rep("20:00:00", nuber),
                                 rep("4:00:00", nuber))) |> 
  mutate(dt_start = as.POSIXct(paste(date, time_start)),
         dt_end = case_when(period == "night" ~ as.POSIXct(paste(as.Date(date + 1), time_end)),
                            TRUE ~ as.POSIXct(paste(as.Date(date), time_end))),
         date = as.Date(date))

