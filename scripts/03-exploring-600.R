#### Plotting and exploring LI-600 data ####
library(tidyverse)
library(patchwork)
library(cowplot)
library(RColorBrewer)
library(openintro)
theme_set(theme_bw())
set.seed(323)

#### Visualizing ####

# Stomatal conductance
gs_raw_points <- sum_by_canopy |> 
  filter(gsw_var == "gsw_raw_m") |> 
  ggplot() +
  geom_vline(data = rain_df, aes(xintercept = dt), linetype = 2, linewidth = 1, color = "royalblue4") +
  # geom_line(aes(x = dt, y = gsw_m, group = interaction(individual, Date, canopy), color = factor(individual)), alpha = 0.5, position = position_dodge(width = 5000)) +
  geom_errorbar(aes(x = dt, color = canopy, ymin = gsw_m - gsw_sd, ymax = gsw_m + gsw_sd), position = position_dodge(width = 5000), width = 10000) +
  geom_point(aes(x = dt, y = gsw_m, color = canopy, shape = canopy), position = position_dodge(width = 5000), size = 3) +
  scale_color_brewer(palette = "Dark2") +
  labs(y = expression(paste(g[s], " (mol ", m^-2, s^-1, ")")),
       x = "Date time",
       color = "Individual",
       shape = "Canopy level") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13)) + NULL
  # facet_wrap(~ Date, scales = "free")

gs_by_period <- sum_by_individual |> 
  ggplot(aes(x = factor(period, levels = c("early", "morning", "midday", "afternoon")), y = gsw_m, group = interaction(factor(period), canopy), color = canopy)) +
  geom_boxplot() +
  geom_jitter(width = 0.15, size = 2, alpha = 0.3) +
  scale_color_brewer(palette = "Set1") +
  labs(y = expression(paste(g[s], " (mol ", m^-2, s^-1, ")")),
       x = "Period",
       color = "Canopy level") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.position = "none")

gs_by_canopy <- sum_by_individual |> 
  ggplot(aes(x = canopy, y = gsw_m, color = canopy)) +
  geom_boxplot() +
  geom_jitter(width = 0.15, size = 2, alpha = 0.3) +
  scale_color_brewer(palette = "Set1") +
  labs(y = expression(paste(g[s], " (mol ", m^-2, s^-1, ")")),
       x = "Canopy level",
       color = "Canopy level") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))

# gs_raw_points / gs_by_tree + gs_by_canopy

top_gs <- plot_grid(gs_raw_points,
                     nrow = 1)
bottom_gs <- plot_grid(gs_by_period, gs_by_canopy,
                      nrow = 1)
plot_grid(top_gs, bottom_gs, nrow = 2)

# Fluorescence
fm_raw_points <- sum_by_individual |> 
  ggplot(aes(x = dt, y = `Fm'_m`, color = factor(individual), shape = canopy)) +
  # geom_line(aes(x = dt, y = `Fm'_m`, group = interaction(individual, Date, canopy), color = factor(individual)), alpha = 0.5, position = position_dodge(width = 5000)) +
  geom_errorbar(aes(ymin = `Fm'_m` - `Fm'_sd`, ymax = `Fm'_m` + `Fm'_sd`), position = position_dodge(width = 5000), width = 10000) +
  geom_point(position = position_dodge(width = 5000), size = 3) +
  scale_color_brewer(palette = "Dark2") +
  labs(y = "Maximum fluorescence in light",
       x = "Date time",
       color = "Individual",
       shape = "Canopy level") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13)) + NULL
  # facet_wrap(~ Date, scales = "free")

fm_by_period <- sum_by_individual |> 
  ggplot(aes(x = factor(period, levels = c("early", "morning", "midday", "afternoon")), y = `Fm'_m`, group = interaction(factor(period), canopy), color = canopy)) +
  geom_boxplot() +
  geom_jitter(width = 0.15, size = 2, alpha = 0.3) +
  scale_color_brewer(palette = "Set1") +
  labs(y = "Maximum fluorescence in light",
       x = "Period") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.position = "none")

fm_by_canopy <- sum_by_individual |> 
  ggplot(aes(x = canopy, y = `Fm'_m`, color = canopy)) +
  geom_boxplot() +
  geom_jitter(width = 0.15, size = 2, alpha = 0.3) +
  scale_color_brewer(palette = "Set1") +
  labs(y = "Maximum fluorescence in light",
       x = "Canopy level",
       color = "Canopy level") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))

top_fm <- plot_grid(fm_raw_points,
                     nrow = 1)
bottom_fm <- plot_grid(fm_by_period, fm_by_canopy,
                      nrow = 1)
plot_grid(top_fm, bottom_fm, nrow = 2)

# Seeing what method works best for fixing stomatal conductance measurements
sum_by_canopy |> 
  # filter(Date == as.Date("2026-06-24")) |> 
  ggplot() +
  geom_rect(data = rects, aes(xmax = dt_start,
                              xmin = dt_end,
                              ymin = -Inf,
                              ymax = Inf,
                              fill = period), alpha = 0.5) +
  geom_errorbar(aes(x = dt, y = gsw_m, ymin = gsw_m - gsw_sd, ymax = gsw_m + gsw_sd, color = canopy, shape = canopy), position = position_dodge(width = 5000), width = 10000) +
  geom_point(aes(x = dt, y = gsw_m, color = canopy, shape = canopy), position = position_dodge(width = 5000), size = 3) +
  scale_color_manual(values = c("chocolate2", "seagreen")) +
  scale_fill_manual(values = c("salmon2", "skyblue2", "goldenrod2", "aquamarine4", "royalblue3")) +
  labs(y = expression(paste(g[s], " (mol ", m^-2, s^-1, ")")),
       x = "Date time",
       color = "Individual",
       shape = "Canopy level") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13)) + 
  facet_wrap(~ gsw_var, ncol = 1)





#### Testing ####

# Assessing normality of gs
qqnormsim(gsw, smaller_measurements)

gs_sum <- smaller_measurements |> 
  group_by(canopy) |> 
  summarize(gsw_m = mean(gsw)) |> 
  ungroup()
t.test(gs_sum$gsw_m, mu = 0)

# Assessing normality of Fm'
qqnormsim(`Fm'`, smaller_measurements)

fm_sum <- smaller_measurements |> 
  group_by(canopy) |> 
  summarize(fm_m = mean(`Fm'`)) |> 
  ungroup()
t.test(fm_sum$fm_m, mu = 0)


