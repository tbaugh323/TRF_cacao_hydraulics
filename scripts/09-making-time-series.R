#### Making big time series plot ####
# Figure posters in here also
library(tidyverse)
library(patchwork)
theme_set(theme_bw())

trees <- c("BioR1192", "BioR1171", "BioR1170", "BioR1191")

rects_rain <- read_csv("data/met_data/rects_rain.csv")

#### Met subplot ####
all_met <- read_csv("data/met_data/all_met.csv") |> 
  mutate(DateTime_MST = as.POSIXct(DateTime_MST, tz = "MST"))

met_timeseries <- all_met |> 
  mutate(Tree_ID = case_when(Tree_ID == "BioR1171" ~ "Level 3",
                             Tree_ID == "BioR1192" ~ "Level 2")) |> 
  ggplot(aes(x = DateTime_MST, y = TA)) +
  geom_point(aes(color = VPD), size = 1.7) +
  scale_color_viridis_c(option = "rocket") +
  labs(x = "Date", y = expression(paste("Temperature (", degree, "C)")), 
       color = "VPD") +
  facet_wrap(~ Tree_ID, ncol = 1) +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 34),
        legend.text = element_text(size = 28),
        legend.title = element_text(size = 34),
        strip.text = element_text(size = 30, color = "white"),
        strip.background = element_rect(fill = "#205A3D"))

#### TWD subplot ####

all_dendros <- read_csv("data/dendro_data/all_dendros.csv") |> 
  mutate(datetime = as.POSIXct(datetime, tzone = "MST")) |> 
  mutate(condition = case_when(date <= as.Date("2026-06-30") ~ "Predrought",
                               date > as.Date("2026-06-30") & date <= as.Date("2026-07-15") ~ "Drought",
                               date > as.Date("2026-07-15") ~ "Recovery"))

dendro_timeseries <- all_dendros |> 
  filter(date >= as.Date("2026-06-21")) |> 
  filter(Tree_ID %in% trees) |> 
  mutate(displacement_mm = displacement.um.fixed / 1000) |> 
  ggplot(aes(x = datetime, y = twd_norm)) +
  geom_vline(data = rects_rain, aes(xintercept = date),
             linetype = "dashed", linewidth = 1, color = "navy", alpha = 0.6) +
  geom_point(aes(x = datetime, y = displacement_mm / 3), size = 1.4, color = "gray70") +
  geom_point(aes(color = factor(Tree_ID)), alpha = 0.9, size = 2) +
  scale_color_manual(values = c("#7570b3", "#d95f02", "#1b9e77")) +
  scale_y_continuous(sec.axis = sec_axis(~ . * 3, expression(paste("Displacement (", Delta, "d) (mm)")))) +
  facet_wrap(~ Tree_ID, ncol = 1) +
  labs(x = "Date", y = expression(paste(TWD[norm]))) +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 23),
        axis.title = element_text(size = 25),
        legend.position = "none",
        strip.text = element_text(size = 23, color = "white"),
        axis.title.y.right = element_text(color = "gray40"),
        strip.background = element_rect(fill = "#205A3D"),
        axis.title.x = element_blank())

#### SV subplot ####

sv_all <- read_csv("data/sap_flow/sv_all.csv") |> 
  mutate(dt = as.POSIXct(dt, tz = "MST")) |> 
  mutate(location = sub(".*?_", "", Tree_ID)) |> 
  mutate(location = case_when(location == "BioR1132" | location == "BioR1192" ~ NA,
                              TRUE ~ location)) |> 
  mutate(Tree_ID = sub("_.*", "", Tree_ID),
         Tree_ID_location = case_when(is.na(location) ~ Tree_ID,
                                      TRUE ~ paste0(Tree_ID, "_", location)))

sv_timeseries <- sv_all |>
  filter(date >= as.Date("2026-06-21")) |> 
  filter(Tree_ID %in% trees) |> 
  filter(Tree_ID_location != "BioR1171_M",
         Tree_ID_location != "BioR1171_NB",
         Tree_ID_location != "BioR1171_L",
         Tree_ID_location != "BioR1171_T",
         Tree_ID_location != "BioR1171_SB") |> 
  ggplot(aes(x = dt, y = VhrmHRM5)) +
  geom_vline(data = rects_rain, aes(xintercept = date),
             linetype = "dashed", linewidth = 1, color = "navy", alpha = 0.6) +
  geom_hline(aes(yintercept = 0), color = "gray40") +
  geom_point(aes(color = Tree_ID), size = 2) +
  scale_color_manual(values = c("#d95f02", "#1b9e77")) +
  facet_wrap(~ Tree_ID_location, ncol = 1) +
  labs(x = "Date", y = expression(paste("Sap velocity at 5 mm (", V["s,5"], ") (cm/hr)"))) +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 23),
        axis.title = element_text(size = 25),
        legend.position = "none",
        strip.text = element_text(size = 23, color = "white"),
        strip.background = element_rect(fill = "#205A3D"),
        axis.title.x = element_blank())

#### WP subplot ####

wp_PDMD_long <- read_csv("data/water_potential/wp_PDMD_long.csv")
wp <- read_csv("data/water_potential/all_wp.csv")

wp_no_canopy <- wp |> 
  group_by(date, individual, level, period, condition) |> 
  summarize(wp_m = mean(wp),
            wp_sd = sd(wp)) |> 
  ungroup() |> 
  mutate(dt = case_when(period == "PD" ~ as.POSIXct(paste(date, "4:30:00")),
                        period == "MD" ~ as.POSIXct(paste(date, "11:00:00"))))

wp_timeseries <- wp_no_canopy |> 
  mutate(individual = case_when(individual == 1 ~ "BioR1192",
                                individual == 2 ~ "BioR1171",
                                individual == 3 ~ "BioR1170",
                                individual == 4 ~ "BioR1191")) |> 
  ggplot(aes(x = dt, y = wp_m)) +
  geom_line(aes(color = factor(individual), group = interaction(individual, period)), 
            linewidth = 1) +
  geom_errorbar(aes(ymin = wp_m - wp_sd, ymax = wp_m + wp_sd, color = factor(individual))) +
  geom_point(aes(color = factor(individual), shape = period), size = 4) +
  scale_color_manual(values = c("#7570b3", "#d95f02", "#e7298a", "#1b9e77")) +
  facet_wrap(~ individual, ncol = 1) +
  labs(x = "Date", y = expression(paste(Psi)), shape = "Period") +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.position = "none",
        strip.text = element_text(size = 13),
        strip.background = element_rect(fill = "white"))

#### gs subplot ####

sum_by_individual <- read_csv("data/licor600_data/licor600_by_individual.csv")

sum_by_individual |> 
  mutate(individual = case_when(individual == 1 ~ "BioR1192",
                                individual == 2 ~ "BioR1171",
                                individual == 3 ~ "BioR1170",
                                individual == 4 ~ "BioR1191")) |> 
  ggplot(aes(x = dt, y = gsw_m)) +
  geom_hline(aes(yintercept = 0), color = "gray40") +
  geom_point(aes(color = individual), size = 2) +
  scale_color_manual(values = c("#7570b3", "#d95f02", "#e7298a", "#1b9e77")) +
  facet_wrap(~ individual, ncol = 1) +
  labs(x = "Date", y = expression(paste(g[s], " (mol ", m^-2, s^-1, ")"))) +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13),
        strip.text = element_text(size = 13),
        strip.background = element_rect(fill = "white"))
# blahhhh

#### Putting them together

dendro_timeseries / sv_timeseries / wp_timeseries

# probably better for exporting purposes to look at them individually

met_timeseries
dendro_timeseries
sv_timeseries
wp_timeseries


