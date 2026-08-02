#### Seeing how response variables change with env conditions ####
library(tidyverse)
theme_set(them_bw())

#### Reading in data ####

all_dendros <- read_csv("data/dendro_data/all_dendros.csv") |> 
  mutate(datetime = as.POSIXct(datetime, tz = "MST"))

sv_all <- read_csv("data/sap_flow/sv_all.csv")

all_met <- read_csv("data/met_data/all_met.csv") |> 
  mutate(DateTime_MST = as.POSIXct(DateTime_MST, tz = "MST"))

all_met_datetime <- all_met |> 
  rename(datetime = DateTime_MST,
         Level_ID = Tree_ID) |> 
  dplyr::select(datetime, Level_ID, TA, RH, VPD)

all_met_all_dendros <- merge(all_met_datetime, all_dendros |> dplyr::select(-VPD), by = c("datetime"))

#### Visualizing TWD ####

all_dendros |> 
  # filter(date == as.Date("2026-07-08") | date == as.Date("2026-07-09")) |> 
  ggplot(aes(x = Temperature, y = twd_norm)) +
  geom_point(aes(color = datetime)) +
  scale_color_viridis_c(option = "viridis", trans = scales::time_trans()) +
  facet_wrap(~ Tree_ID)

# Showing difference in amplitude change VS TWD accumulation
all_dendros |> 
  filter(Tree_ID == "BioR1170" | Tree_ID == "BioR1171") |> 
  filter(date == as.Date("2026-07-07") | 
           date == as.Date("2026-07-08") | 
           date == as.Date("2026-07-09") | 
           date == as.Date("2026-07-10")) |> 
  ggplot(aes(x = Temperature, y = twd_norm)) +
  geom_point(aes(color = datetime), size = 4) +
  scale_color_viridis_c(option = "viridis", trans = scales::time_trans()) +
  facet_wrap(~ Tree_ID) +
  labs(x = expression(paste("Temperature (", degree, "C)")), 
       y = expression(paste(TWD[norm])), color = "Date") +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 23),
        axis.title = element_text(size = 25),
        legend.text = element_text(size = 23),
        legend.title = element_text(size = 25),
        strip.text = element_text(size = 23, color = "white"),
        strip.background = element_rect(fill = "#205A3D"))

all_dendros |> 
  filter(Tree_ID == "BioR1170" | Tree_ID == "BioR1171") |> 
  filter(date == as.Date("2026-07-15") | 
           date == as.Date("2026-07-16") | 
           date == as.Date("2026-07-17") |
           date == as.Date("2026-07-18")) |> 
  ggplot(aes(x = Temperature, y = twd_norm)) +
  geom_point(aes(color = datetime), size = 4) +
  geom_point(data = all_dendros |> 
               filter(datetime == as.POSIXct("2026-07-16 00:00:00"), 
                      Tree_ID == "BioR1170" | Tree_ID == "BioR1171"), 
             aes(x = Temperature, y = twd_norm), 
             color = "cornflowerblue", size = 10, alpha = 0.7) +
  scale_color_viridis_c(option = "viridis", trans = scales::time_trans()) +
  facet_wrap(~ Tree_ID) +
  labs(x = expression(paste("Temperature (", degree, "C)")), 
       y = expression(paste(TWD[norm])), color = "Date") +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 23),
        axis.title = element_text(size = 25),
        legend.text = element_text(size = 23),
        legend.title = element_text(size = 25),
        strip.text = element_text(size = 23, color = "white"),
        strip.background = element_rect(fill = "#205A3D"))

all_dendros |> 
  filter(Tree_ID == "BioR1170" | Tree_ID == "BioR1171") |> 
  filter(date == as.Date("2026-07-18") | 
           date == as.Date("2026-07-19") | 
           date == as.Date("2026-07-20")) |> 
  ggplot(aes(x = Temperature, y = twd_norm)) +
  geom_point(aes(color = datetime)) +
  scale_color_viridis_c(option = "viridis", trans = scales::time_trans()) +
  facet_wrap(~ Tree_ID) +
  labs(x = expression(paste("Temperature (", degree, "C)")), 
       y = expression(paste(TWD[norm])), color = "Date") +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        strip.text = element_text(size = 13))

all_dendros |> 
  filter(Tree_ID == "BioR1170" | 
           Tree_ID == "BioR1171" | 
           Tree_ID == "BioR1192") |> 
  ggplot(aes(x = datetime)) +
  geom_line(aes(y = twd_norm, color = VPD)) +
  geom_point(aes(y = twd_norm, color = VPD)) +
  scale_color_viridis_c(option = "rocket") +
  facet_wrap(~ Tree_ID, ncol = 1) +
  labs(x = "Date", y = expression(paste(TWD[norm])), color = "VPD") +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        strip.text = element_text(size = 13))

all_met_all_dendros |> 
  filter(Tree_ID == "BioR1170" | 
           Tree_ID == "BioR1171") |> 
  filter(Level_ID == "BioR1171") |> 
  filter(date >= as.Date("2026-07-05") & date <= as.Date("2026-07-20")) |> 
  ggplot(aes(x = datetime)) +
  geom_vline(aes(xintercept = as.Date("2026-07-16")), color = "navy", 
             linewidth = 2, alpha = 0.4) +
  geom_line(aes(y = twd_norm, color = VPD)) +
  geom_point(aes(y = twd_norm, color = VPD), size = 2) +
  scale_color_viridis_c(option = "rocket") +
  facet_wrap(~ Tree_ID) +
  labs(x = "Date", y = expression(paste(TWD[norm])), color = "VPD") +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 23),
        axis.title = element_text(size = 25),
        legend.text = element_text(size = 23),
        legend.title = element_text(size = 25),
        strip.text = element_text(size = 23, color = "white"),
        strip.background = element_rect(fill = "#205A3D"))

#### Visualizing sap velocity ####

all_met <- read_csv("data/met_data/all_met.csv")

sv_vpd <- left_join(sv_all, all_met |> 
                      rename(dt = DateTime_MST), by = c("Tree_ID", "dt"))

sv_vpd |> 
  filter(Tree_ID_location != "BioR1171_M", Tree_ID != "BioR1171_NB_location") |> 
  filter(date >= as.Date("2026-06-23")) |> 
  filter(VhrmHRM5 <= 40) |> 
  ggplot() +
  geom_line(aes(x = dt, y = VhrmHRM5, color = TA), linewidth = 0.5) +
  geom_point(aes(x = dt, y = VhrmHRM5, color = TA), size = 0.5) +
  scale_color_viridis_c(option = "rocket") +
  facet_wrap(~ Tree_ID)

sv_vpd |> 
  filter(Tree_ID_location != "BioR1171_M", Tree_ID_location != "BioR1171_NB") |> 
  filter(date == as.Date("2026-07-08")) |> 
  filter(VhrmHRM5 <= 40) |> 
  ggplot(aes(x = TA, y = VhrmHRM5)) +
  geom_point(aes(color = dt)) +
  scale_color_viridis_c(option = "viridis", trans = scales::time_trans()) +
  facet_wrap(~ Tree_ID)

# what the heck is going on

#### Just met ####

all_met |> 
  mutate(condition = case_when(DateTime_MST <= as.POSIXct("2026-07-02 00:00:00") ~ "Predrought",
                               DateTime_MST >= as.POSIXct("2026-07-16 00:00:00") ~ "Recovery",
                               TRUE ~ "Drought")) |> 
  mutate(Tree_ID = case_when(Tree_ID == "BioR1171" ~ "Level_3",
                             Tree_ID == "BioR1192" ~ "Level_2")) |>
  pivot_wider(names_from = Tree_ID, values_from = c(TA, RH, VPD)) |> 
  filter(!is.na(VPD_Level_3) & !is.na(VPD_Level_2)) |> 
  ggplot(aes(x = VPD_Level_2, y = VPD_Level_3, color = condition)) +
  geom_abline(aes(slope = 1, intercept = 0), linetype = "dashed", linewidth = 0.5) +
  geom_point(alpha = 0.3, size = 2) +
  xlim(0, 12) +
  ylim(0, 12) +
  scale_color_manual(values = c("springgreen4", "lightsalmon3", "yellowgreen")) +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 18))

all_met |> 
  mutate(condition = case_when(DateTime_MST <= as.POSIXct("2026-07-02 00:00:00") ~ "Predrought",
                               DateTime_MST >= as.POSIXct("2026-07-16 00:00:00") ~ "Recovery",
                               TRUE ~ "Drought")) |> 
  mutate(Tree_ID = case_when(Tree_ID == "BioR1171" ~ "Level_3",
                             Tree_ID == "BioR1192" ~ "Level_2")) |> 
  pivot_wider(names_from = Tree_ID, values_from = c(TA, RH, VPD)) |>
  filter(!is.na(VPD_Level_3) & !is.na(VPD_Level_2)) |> 
  mutate(difference = VPD_Level_3 - VPD_Level_2) |> 
  mutate(VPD_bin = case_when(VPD_Level_3 < 1 ~ "<1",
                             VPD_Level_3 >= 1 & VPD_Level_3 < 2.5 ~ "1-2.5",
                             VPD_Level_3 >= 2.5 & VPD_Level_3 < 4 ~ "2.5-4",
                             VPD_Level_3 >= 4 & VPD_Level_3 < 5.5 ~ "4-5.5",
                             VPD_Level_3 >= 5.5 & VPD_Level_3 < 7 ~ "5.5-7",
                             VPD_Level_3 >= 7 & VPD_Level_3 < 8.5 ~ "7-8.5",
                             VPD_Level_3 >= 8.5 & VPD_Level_3 < 10 ~ "8.5-10",
                             VPD_Level_3 > 10 ~ ">10")) |> 
  ggplot(aes(x = factor(VPD_bin, levels = c("<1", "1-2.5", "2.5-4", "4-5.5", "5.5-7", "7-8.5", "8.5-10", ">10")), y = difference)) +
  geom_boxplot() +
  labs(x = "Level 3 VPD (kPa)", y = "Level 3 VPD - Level 2 VPD") +
  theme(panel.grid = element_blank())


