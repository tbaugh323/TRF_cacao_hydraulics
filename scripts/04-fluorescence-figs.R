#### Plotting and exploring LI-600 fluorescence data ####
library(tidyverse)
library(patchwork)
library(cowplot)
library(RColorBrewer)
library(openintro)
theme_set(theme_bw())
set.seed(323)

#### Visualizing ####

# Looking at most recent data
clean_all_data |> 
  filter(Date == as.Date("2026-07-22")) |>
  ggplot(aes(color = factor(individual))) +
  # geom_errorbar(aes(x = dt, y = gsw_m, ymin = gsw_m - gsw_sd, ymax = gsw_m + gsw_sd), width = 1000) +
  geom_point(aes(x = dt, y = `Fm'`, shape = canopy), size = 3) +
  scale_color_brewer(palette = "Dark2") +
  labs(y = "Maximum fluorescence in light",
       x = "Date time",
       color = "Individual",
       shape = "Canopy level") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))
# facet_wrap(~ individual, ncol = 1)

# Grid plots
fm_raw_points <- sum_by_canopy |> 
  ggplot(aes(x = dt, y = `Fm'_m`)) +
  geom_vline(data = rain_df, aes(xintercept = dt), linetype = 2, linewidth = 1, color = "royalblue4") +
  # geom_line(aes(x = dt, y = `Fm'_m`, group = interaction(individual, Date, canopy), color = factor(individual)), alpha = 0.5, position = position_dodge(width = 5000)) +
  geom_errorbar(aes(x = dt, color = canopy, ymin = `Fm'_m` - `Fm'_sd`, ymax = `Fm'_m` + `Fm'_sd`), position = position_dodge(width = 5000), width = 10000) +
  geom_point(aes(x = dt, y = `Fm'_m`, color = canopy, shape = canopy), position = position_dodge(width = 5000), size = 3) +
  scale_color_manual(values = c("skyblue2", "tomato")) +
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

fm_by_period <- sum_by_canopy |> 
  filter(period != "afternoon") |> 
  ggplot(aes(x = factor(period, levels = c("early", "morning", "midday", "afternoon")), y = `Fm'_m`, group = interaction(factor(period), canopy), color = canopy)) +
  geom_boxplot() +
  # geom_jitter(width = 0.15, size = 2, alpha = 0.3) +
  scale_color_manual(values = c("skyblue2", "tomato")) +
  labs(y = "Maximum fluorescence in light",
       x = "Period") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.position = "none")

fm_by_canopy <- sum_by_canopy |> 
  mutate(condition = factor(condition, levels = c("predrought", "drought", "recovery"))) |> 
  ggplot(aes(x = condition, y = `Fm'_m`, color = canopy)) +
  geom_boxplot() +
  # geom_jitter(width = 0.15, size = 2, alpha = 0.3) +
  scale_color_manual(values = c("skyblue2", "tomato")) +
  labs(y = "Maximum fluorescence in light",
       x = "Drought Condition",
       color = "Canopy level") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))

top_fm <- plot_grid(fm_raw_points,
                    nrow = 1)
bottom_fm <- plot_grid(fm_by_period, fm_by_canopy,
                       nrow = 1)
plot_grid(top_fm, bottom_fm, nrow = 2)

