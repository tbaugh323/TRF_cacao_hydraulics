#### Calculating metrics of tree water deficit ####
library(tidyverse)
theme_set(theme_bw())

#### Setting up big datafile ####

dendro_files <- list.files("data/dendro_data/clean_dendros/")
all_dendros <- data.frame(date = as.Date(NA), datetime = as.POSIXct(NA))

# Go through all clean dendro data
for (i in 1:length(dendro_files)) {
  # Read in file
  current_sensor <- read_csv(paste0("data/dendro_data/clean_dendros/", dendro_files[i]))
  
  # Cut to when the sensor was moved to the correct tree
  current_sensor <- current_sensor |> 
    filter(TIME >= as.POSIXct("2026-06-22 10:00:00"))
  
  # Calculate tree water deficit (normalized)
  current_sensor_twd <- current_sensor |> 
    mutate(twd = max(current_sensor$displacement.um.fixed) - displacement.um.fixed,
           twd_norm = twd / max(twd))
  
  # Calculate daily amplitude
  current_sensor_full <- current_sensor_twd |> 
    group_by(day, month, year) |> 
    summarize(daily_max = max(displacement.um.fixed),
              daily_min = min(displacement.um.fixed),
              twd_min = min(twd_norm)) |> 
    # daily_sd = sd(displacement.um.fixed),
    # daily_mean = mean(displacement.um.fixed),
    # daily_first = first(displacement.um.fixed),
    # daily_last = last(displacement.um.fixed)) |> 
    ungroup() |> 
    mutate(amplitude = daily_max - daily_min,
           date = as.Date(paste(year, month, day), format = "%Y %m %d")) |> 
    merge(current_sensor_twd, by = c("day", "month", "year")) |> 
    select(date, datetime, daily_max, daily_min, amplitude, twd, twd_norm, twd_min, instance, Temperature, Humidity, VPD, Tree_ID, minute, hour, displacement.um.fixed, hour, minute, second)
  
  twd_pd_df <- current_sensor_full |> 
    mutate(time = hms::as_hms(datetime)) |> 
    filter(time >= hms::as_hms("05:30:00") & time <= hms::as_hms("6:00:00")) |> 
    group_by(Tree_ID, date) |> 
    summarize(twd_pd = mean(twd_norm)) |> 
    ungroup() |> 
    dplyr::select(date, twd_pd)
  
  current_sensor_full <- merge(current_sensor_full, twd_pd_df, by = "date")
  
  # Put together each tree
  all_dendros <- bind_rows(current_sensor_full, all_dendros)
}
# clean
all_dendros <- all_dendros |> 
  filter(!is.na(Tree_ID))

#### Visualize to check ####

# Looking at cleaned displacement
all_dendros |> 
  ggplot(aes(x = datetime, y = displacement.um.fixed)) +
  geom_vline(aes(xintercept = as.POSIXct("2026-06-26 10:00:00")), color = "red") +
  geom_vline(data = rain_df, aes(xintercept = dt), linetype = 2, linewidth = 0.4, color = "royalblue4") +
  geom_point() +
  # scale_y_continuous(limits = c(0, 10000)) +
  facet_wrap(~ Tree_ID, scales = "free_y")
# yayyY!!!

# Looking at tree water deficit
all_dendros |> 
  ggplot() +
  geom_vline(data = rain_df, aes(xintercept = dt), linetype = 2, linewidth = 0.4, color = "royalblue4") +
  # geom_line(aes(x = datetime, y = VPD * 4), color = "forestgreen", alpha = 0.5) +
  geom_point(aes(x = datetime, y = twd_norm), color = "gray60", size = 0.6) +
  geom_point(aes(x = date, y = twd_min), color = "salmon") +
  scale_y_continuous(sec.axis = sec_axis(~./4, "vpd")) +
  facet_wrap(~ Tree_ID) +
  theme(panel.grid = element_blank())

# Looking at amplitude change each day
all_dendros |> 
  ggplot(aes(x = date, y = amplitude)) +
  geom_vline(data = rain_df, aes(xintercept = dt), linetype = 2, linewidth = 0.4, color = "royalblue4") +
  geom_point() +
  facet_wrap(~ Tree_ID)
# yayyy!!!

# comparing twd_min and twd_pd (they should be the same)
all_dendros |> 
  ggplot(aes(x = twd_min, y = twd_pd)) +
  geom_abline(aes(slope = 1, intercept = 0), linetype = "dashed", color = "gray70") +
  geom_point(aes(color = date, shape = Tree_ID), size = 2.5)
# not always the same hmm

#### Write out concatenated data ####

write_csv(all_dendros, "data/dendro_data/all_dendros.csv")


