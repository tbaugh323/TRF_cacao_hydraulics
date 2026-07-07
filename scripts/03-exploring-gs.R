#### Plotting and exploring LI-600 stomatal conductance data ####
library(tidyverse)
library(patchwork)
library(cowplot)
library(RColorBrewer)
library(openintro)
theme_set(theme_bw())
set.seed(323)

#### Visualizing ####

# Seeing what method works best for fixing stomatal conductance measurements
sum_by_canopy |> 
  mutate(gsw_var = case_when(gsw_var == "gsw_raw_m" ~ "Raw points",
                             gsw_var == "gsw_rem_m" ~ "Removed negatives",
                             gsw_var == "gsw_zeroed_m" ~ "Zeroed negatives")) |> 
  # filter(Date == as.Date("2026-06-24")) |> 
  ggplot() +
  # geom_rect(data = rects, aes(xmax = dt_start,
  #                             xmin = dt_end,
  #                             ymin = -Inf,
  #                             ymax = Inf,
  #                             fill = period), alpha = 0.5) +
  geom_hline(aes(yintercept = 0), linewidth = 1, linetype = "dotted", color = "gray50") +
  geom_vline(data = rain_df, aes(xintercept = dt), linetype = 2, linewidth = 1, color = "royalblue4") +
  geom_errorbar(aes(x = dt, y = gsw_m, ymin = gsw_m - gsw_sd, ymax = gsw_m + gsw_sd, color = canopy, shape = canopy), position = position_dodge(width = 5000), width = 10000) +
  geom_point(aes(x = dt, y = gsw_m, color = canopy, shape = canopy), position = position_dodge(width = 5000), size = 3) +
  scale_color_manual(values = c("skyblue2", "tomato")) +
  scale_fill_manual(values = c("salmon2", "skyblue", "goldenrod2", "aquamarine4", "royalblue3")) +
  labs(y = expression(paste(g[s], " (mol ", m^-2, s^-1, ")")),
       x = "Date time",
       color = "Canopy level",
       shape = "Canopy level") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        strip.text = element_text(size = 13)) + 
  facet_wrap(~ gsw_var, ncol = 1)

# Looking at change in gs
deltas |> 
  # filter(instrument != "PSA-01670") |> 
  pivot_longer(cols = c(delta1, delta2), names_to = "delta", values_to = "delta_val") |> 
  mutate(delta = case_when(delta == "delta1" ~ "Morning - Early",
                           delta == "delta2" ~ "Midday - Morning")) |> 
  ggplot() +
  geom_rect(data = rects_drought |> filter(period == "drought"),
            aes(xmin = date_start, xmax = date_end, ymin = -Inf, ymax = Inf),
            fill = "burlywood3", alpha = 0.3) +
  geom_rect(data = rects_drought |> filter(period != "drought"),
            aes(xmin = date_start, xmax = date_end, ymin = -Inf, ymax = Inf),
            fill = "palegreen4", alpha = 0.3) +
  geom_hline(aes(yintercept = 0), linetype = "dotted", linewidth = 1, color = "gray50") +
  geom_point(aes(x = Date, y = delta_val, shape = delta, color = canopy), size = 4) +
  # scale_color_manual(values = c("salmon", "khaki", "skyblue", "plum")) +
  scale_color_manual(values = c("skyblue2", "tomato")) +
  labs(color = "Canopy level", shape = "Delta Value") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))

deltas |> 
  ggplot(aes(x = delta1, y = delta2)) +
  geom_vline(aes(xintercept = 0), linetype = 2, linewidth = 0.7) +
  geom_hline(aes(yintercept = 0), linetype = 2, linewidth = 0.7) +
  geom_point(aes(shape = canopy, color = Date), size = 4, alpha = 0.9) +
  scale_color_viridis_c(option = "turbo", trans = "date") +
  labs(x = "Morning - Early", y = "Midday - Morning", shape = "Canopy") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))

raw_timeseries <- clean_all_data |>
  mutate(condition = factor(condition, levels = c("predrought", "drought", "recovery"))) |> 
  ggplot(aes(x = Time, y = gsw)) +
  geom_hline(aes(yintercept = 0), linetype = "dotted", linewidth = 1, color = "gray50") +
  geom_point(aes(color = Date), size = 2) +
  scale_color_viridis_c(option = "turbo", trans = "date") +
  facet_wrap(~ condition) +
  labs(y = expression(paste(g[s], " (mol ", m^-2, s^-1, ")"))) +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        strip.text = element_text(size = 13))

zeroed_timeseries <- clean_all_data |>
  mutate(gsw = ifelse(gsw < 0, 0, gsw)) |> 
  mutate(condition = factor(condition, levels = c("predrought", "drought", "recovery"))) |> 
  ggplot(aes(x = Time, y = gsw)) +
  geom_hline(aes(yintercept = 0), linetype = "dotted", linewidth = 1, color = "gray50") +
  geom_point(aes(color = Date), size = 2) +
  scale_color_viridis_c(option = "turbo", trans = "date") +
  facet_wrap(~ condition) +
  labs(y = expression(paste(g[s], " (mol ", m^-2, s^-1, ")"))) +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        strip.text = element_text(size = 13))

raw_timeseries / zeroed_timeseries

