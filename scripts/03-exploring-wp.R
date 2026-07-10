#### Wrangling and visualizing pressure chamber measurements ####
library(tidyverse)
library(googledrive)
library(googlesheets4)
library(janitor)
library(RColorBrewer)
theme_set(theme_bw())

#### Reading ####

wp <- read_sheet("https://docs.google.com/spreadsheets/d/1LdmUZRiTcnKyBLbcdbUDEppdEh5o5ovp06zli0tCR30/edit?gid=0#gid=0") |> 
  clean_names() |> 
  mutate(time = hms::as_hms(time),
         date = as.Date(date)) |> 
  mutate(canopy = case_when(canopy == "lower" ~ "Lower canopy",
                            canopy == "upper" ~ "Upper canopy"),
         month = month(date),
         day = day(date),
         condition = case_when(month == 06 ~ "predrought",
                               month == 07 & day <= 15 ~ "drought",
                               month == 07 & day > 15 ~ "recovery")) |> 
  filter(!is.na(wp)) |> 
  filter(is.na(comments)) |> 
  mutate(dt = as.POSIXct(paste(date, time))) |> 
  relocate(dt)

# Writing out to have locally
write_csv(wp, "data/all_wp.csv")

# Summarizing
wp_sum <- wp |> 
  group_by(individual, canopy, date, period, condition) |> 
  summarize(wp_m = mean(wp),
            wp_sd = sd(wp),
            n = n()) |> 
  ungroup() |> 
  mutate(dt = case_when(period == "PD" ~ as.POSIXct(paste(date, "4:30:00")),
                        period == "MD" ~ as.POSIXct(paste(date, "11:00:00")))) |> 
  relocate(dt)

#### Visualizing ####

wp |> 
  ggplot(aes(x = dt, y = wp, color = factor(individual), shape = canopy)) +
  geom_point(size = 4) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Date time",
       y = expression(paste(Psi[PD], " (MPa)")),
       color = "Individual",
       shape = "Canopy") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13)) +
  facet_wrap(~date, scales = "free_x")

wp_sum |> 
  ggplot(aes(x = date, color = factor(individual), shape = period)) +
  geom_jitter(data = wp, aes(x = date, y = wp, color = factor(individual), shape = period), position = position_dodge(width = 0.35), alpha = 0.5, size = 3) +
  geom_errorbar(aes(ymin = wp_m - wp_sd, ymax = wp_m + wp_sd), position = position_dodge(width = 0.35), width = 0.2) +
  geom_point(aes(y = wp_m), position = position_dodge(width = 0.35), size = 4) +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(canopy ~ date, scales = "free_x", ncol = length(unique(wp_sum$date))) +
  labs(x = "Date",
       y = expression(paste(Psi, " (MPa)")),
       color = "Individual",
       shape = "Period") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))


wp_sum |> 
  ggplot(aes(x = dt, y = wp_m)) +
  geom_errorbar(aes(ymin = wp_m - wp_sd, ymax = wp_m + wp_sd, color = factor(individual)), position = position_dodge(width = 70000), width = 50000) +
  geom_point(aes(color = factor(individual), shape = period), position = position_dodge(width = 70000), size = 4) +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~ canopy, ncol = 1) +
  labs(x = "Date",
       y = expression(paste(Psi, " (MPa)")),
       color = "Individual",
       shape = "Period") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))

wp_sum |> 
  ggplot() +
  geom_rect(data = rects_drought |> filter(period == "drought"),
            aes(xmin = date_start, xmax = date_end, ymin = -Inf, ymax = Inf),
            fill = "burlywood3", alpha = 0.3) +
  geom_rect(data = rects_drought |> filter(period != "drought"),
            aes(xmin = date_start, xmax = date_end, ymin = -Inf, ymax = Inf),
            fill = "palegreen4", alpha = 0.3) +
  geom_vline(data = rects_rain, aes(xintercept = date), linetype = "dashed", linewidth = 1, color = "royalblue4") +
  geom_errorbar(aes(x = dt, y = wp_m, ymin = wp_m - wp_sd, ymax = wp_m + wp_sd, color = factor(individual)), position = position_dodge(width = 70000), width = 100000) +
  geom_point(aes(x = dt, y = wp_m, color = factor(individual), shape = period), position = position_dodge(width = 70000), size = 4) +
  facet_wrap(~canopy, ncol = 1) +
  # scale_color_manual(values = c("mediumseagreen", "chocolate2", "slateblue3", "deeppink2")) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Date",
       y = expression(paste(Psi, " (MPa)")),
       color = "Individual",
       shape = "Period") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))

wp_super_sum <- wp_sum |> 
  group_by(dt, date, period, canopy, condition) |> 
  summarize(wp.m = mean(wp_m),
            wp.sd = sd(wp_m),
            n = n()) |> ungroup()

wp_super_sum |> 
  ggplot(aes(x = factor(condition, levels = c("predrought", "drought")), y = wp.m, group = interaction(canopy, period, condition), color = interaction(period, canopy))) +
  geom_boxplot() +
  labs(x = "Condition", y = expression(paste(Psi, " (MPa)")), color = "Period, Canopy level") +
  scale_color_manual(values = c("tomato", "plum3", "skyblue2", "khaki3")) +
  theme(panel.grid = element_blank())

test_aov <- aov(wp.m ~ canopy + condition, wp_super_sum)
summary(test_aov)
# Significant difference between conditions

