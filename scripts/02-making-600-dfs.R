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

instrument <- all_measurements |> 
  filter(configAuthor != "LI-COR Default") |> 
  select(dt, configAuthor) |> 
  rename(instrument = configAuthor)

clean_all_data <- merge(clean_all_data, instrument, by = "dt")

#### Making cleaner data ####

# Summarizing by individual
sum_by_individual <- clean_all_data |> 
  group_by(instrument, individual, canopy, period, Date, condition) |> 
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
            Tleaf_m = mean(Tleaf, na.rm = TRUE),
            flow_m = mean(flow, na.rm = TRUE),
            flow_s_m = mean(flow_s, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(time = case_when(period == "early" ~ hms::as_hms("6:00:00"),
                          period == "morning" ~ hms::as_hms("8:00:00"),
                          period == "midday" ~ hms::as_hms("12:00:00"),
                          period == "afternoon" ~ hms::as_hms("18:30:00")),
         dt = as.POSIXct(paste(Date, time))) |> 
  relocate(dt, Date, time)

# Summarizing without individuals (by canopy and time only)

sum_by_canopy <- clean_all_data |> 
  mutate(gsw_raw = gsw,
         gsw_zeroed = ifelse(gsw <= 0, 0, gsw),
         gsw_rem = ifelse(gsw <= 0, NA, gsw)) |> 
  group_by(instrument, canopy, period, Date, condition) |> 
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
            Tleaf_m = mean(Tleaf, na.rm = TRUE),
            flow_m = mean(flow, na.rm = TRUE),
            flow_s_m = mean(flow_s, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(time = case_when(period == "early" ~ hms::as_hms("6:00:00"),
                          period == "morning" ~ hms::as_hms("8:00:00"),
                          period == "midday" ~ hms::as_hms("12:00:00"),
                          period == "afternoon" ~ hms::as_hms("18:30:00")),
         dt = as.POSIXct(paste(Date, time))) |> 
  pivot_longer(cols = c(gsw_raw_m, gsw_zeroed_m, gsw_rem_m),
               names_to = "gsw_var",
               values_to = "gsw_m") |> 
  relocate(dt, Date, time, canopy, period, condition, gsw_var, gsw_m)

deltas <- sum_by_canopy |> 
  select(instrument, Date, canopy, period, gsw_var, gsw_m) |> 
  filter(period != "afternoon", gsw_var == "gsw_raw_m") |> select(-gsw_var) |> 
  pivot_wider(names_from = period, values_from = gsw_m) |> 
  mutate(delta1 = morning - early,
         delta2 = midday - morning)

# Marking rain events
rain_df <- data.frame(date = c(as.Date(c("2026-06-23", "2026-06-25", "2026-06-30", "2026-07-16", "2026-07-21", "2026-07-23"))),
                      time = c(rep("00:00:00", 6))) |> 
  mutate(dt = as.POSIXct(paste(date, time)))

rects_rain <- data.frame(date = c(as.Date(c("2026-06-23", "2026-06-25", "2026-06-30", "2026-07-16", "2026-07-21", "2026-07-23"))),
                         time_start = c(rep("00:00:00", 6)),
                         time_end = c(rep("2:00:00", 6))) |> 
  mutate(dt_start = as.POSIXct(paste(date, time_start)),
         dt_end = as.POSIXct(paste(date, time_end)),
         date = as.Date(date))

# Defining drought

rects_drought <- data.frame(date_start = c(as.Date(c("2026-06-22", "2026-07-01", "2026-07-16"))),
                            date_end = c(as.Date(c("2026-07-01", "2026-07-16", "2026-07-24"))),
                            period = c("predrought", "drought", "recovery"))

# rects_drought <- rects_drought |> 
#   filter(period == "drought") |> 
#   pivot_longer(!period, names_to = "label", values_to = "date") |> 
#   mutate(label = case_when(label == "date_end" ~ "Drought end",
#                            label == "date_start" ~ "Drought start"))

# Bonus... rects for all times of day
nuber <- length(unique(clean_all_data$Date))

rects <- data.frame(date = rep(unique(clean_all_data$Date), 5),
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

#### Writing out products ####

write_csv(clean_all_data, "data/clean_all_licor600.csv")
write_csv(sum_by_individual, "data/licor600_by_individual.csv")
write_csv(sum_by_canopy, "data/licor600_by_canopy.csv")

