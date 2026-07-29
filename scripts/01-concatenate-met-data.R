#### Put together met data files ####
library(tidyverse)
library(plantecophys)
theme_set(theme_bw())

# Looking to check
met2 <- read_csv("data/met_data/Met-2_R1192-Processed.csv") |> 
  mutate(DateTime_MST = lubridate::force_tz(DateTime_MST, tzone = "MST")) |> 
  mutate(VPD = RHtoVPD(RH_SHT31_., AirTemp_SHT31_C))

met2 |> 
  ggplot(aes(x = DateTime_MST, y = AirTemp_SHT31_C)) +
  geom_point()

met2 |> 
  ggplot(aes(x = DateTime_MST, y = VPD)) +
  geom_point() +
  geom_vline(data = rects_rain, aes(xintercept = date),
             linetype = "dashed", linewidth = 1, color = "navy", alpha = 0.6)

met8 <- read_csv("data/met_data/Met-8_R1171-Processed.csv") |> 
  mutate(DateTime_MST = lubridate::force_tz(DateTime_MST, tzone = "MST")) |> 
  mutate(VPD = RHtoVPD(RH_SHT31_., AirTemp_SHT31_C))

met8 |> 
  ggplot(aes(x = DateTime_MST, y = AirTemp_SHT31_C)) +
  geom_point() +
  geom_vline(data = rects_rain, aes(xintercept = date),
             linetype = "dashed", linewidth = 1, color = "navy", alpha = 0.6)

met8 |> 
  ggplot(aes(x = DateTime_MST, y = VPD)) +
  geom_point() +
  geom_vline(data = rects_rain, aes(xintercept = date),
             linetype = "dashed", linewidth = 1, color = "navy", alpha = 0.6)

# Summarizing to quarter hourly
met_2_sum <- met2 |> 
  mutate(Tree_ID = "BioR1192") |> 
  mutate(DateTime_MST = lubridate::round_date(DateTime_MST, unit = "15 mins"),
         DateTime_UTC = lubridate::round_date(DateTime_MST, unit = "15 mins")) |> 
  group_by(DateTime_MST, DateTime_UTC, Tree_ID) |> 
  summarize(TA = mean(AirTemp_SHT31_C),
            RH = mean(RH_BME280_.),
            VPD = mean(VPD),
            n = n()) |> 
  ungroup()

met_8_sum <- met8 |> 
  mutate(Tree_ID = "BioR1171") |> 
  mutate(DateTime_MST = lubridate::round_date(DateTime_MST, unit = "15 mins"),
         DateTime_UTC = lubridate::round_date(DateTime_MST, unit = "15 mins")) |> 
  group_by(DateTime_MST, DateTime_UTC, Tree_ID) |> 
  summarize(TA = mean(AirTemp_SHT31_C),
            RH = mean(RH_BME280_.),
            VPD = mean(VPD),
            n = n()) |> 
  ungroup()

# Joining
all_met <- rbind(met_2_sum, met_8_sum)

write_csv(all_met, "data/met_data/all_met.csv")

