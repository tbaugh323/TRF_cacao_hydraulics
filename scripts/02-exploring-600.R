#### Wrangling LICOR600 data for T cacao ####
library(tidyverse)
library(patchwork)
library(cowplot)
library(RColorBrewer)
library(openintro)
theme_set(theme_bw())
set.seed(323)

#### Reading ####

all_measurements <- read_csv("data/all_licor600.csv")
# Works better if you just run the wrangle-concatenate script

# Going to use the default LICOR config for reproducability

smaller_measurements <- all_measurements |> 
  filter(configAuthor == "LI-COR Default") |> 
  mutate(canopy = ifelse(canopy == "lower", "Lower", "Upper"),
         individual = as.character(individual))

# Summarizing by tree/canopy

sum_small_data <- smaller_measurements |> 
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
                          period == "noon" ~ hms::as_hms("12:00:00"),
                          period == "afternoon" ~ hms::as_hms("18:30:00")),
         dt = as.POSIXct(paste(Date, time))) |> 
  relocate(dt, Date, time)

#### Visualizing ####

# Stomatal conductance
gs_raw_points <- sum_small_data |> 
  ggplot(aes(x = dt, y = gsw_m, color = factor(individual), shape = canopy)) +
  # geom_line(aes(x = dt, y = gsw_m, group = interaction(individual, Date, canopy), color = factor(individual)), alpha = 0.5, position = position_dodge(width = 5000)) +
  geom_errorbar(aes(ymin = gsw_m - gsw_sd, ymax = gsw_m + gsw_sd), position = position_dodge(width = 5000), width = 10000) +
  geom_point(position = position_dodge(width = 5000), size = 3) +
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

gs_by_period <- sum_small_data |> 
  ggplot(aes(x = factor(period, levels = c("early", "morning", "noon", "afternoon")), y = gsw_m, group = interaction(factor(period), canopy), color = canopy)) +
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

gs_by_canopy <- sum_small_data |> 
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
fm_raw_points <- sum_small_data |> 
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

fm_by_period <- sum_small_data |> 
  ggplot(aes(x = factor(period, levels = c("early", "morning", "noon", "afternoon")), y = `Fm'_m`, group = interaction(factor(period), canopy), color = canopy)) +
  geom_boxplot() +
  geom_jitter(width = 0.15, size = 2, alpha = 0.3) +
  scale_color_brewer(palette = "Set1") +
  labs(y = "Maximum fluorescence in light",
       x = "Period") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.position = "none")

fm_by_canopy <- sum_small_data |> 
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


no_canopy <- sum_small_data |> 
  group_by(Date, dt, time, individual, period) |> 
  summarize(gsw.m = mean(gsw_m),
            gsw.sd = sd(gsw_m)) |> 
  ungroup()

temp <- smaller_measurements |> 
  ggplot(aes(x = dt)) +
  geom_point(aes(y = gsw, color = factor(individual), shape = canopy), size = 2.5, alpha = 0.5) +
  scale_color_brewer(palette = "Dark2") +
  geom_errorbar(data = no_canopy, aes(x = dt, ymin = gsw.m - gsw.sd, ymax = gsw.m + gsw.sd, group = interaction(individual, Date), color = factor(individual)), width = 2200, alpha = 0.7) +
  geom_line(data = no_canopy, aes(x = dt, y = gsw.m, group = interaction(individual, Date), color = factor(individual))) +
  theme(panel.grid = element_blank())


gs_raw_points / temp



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


