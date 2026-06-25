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
         date = as.Date(date),
         dt = as.POSIXct(paste(date, time))) |> 
  relocate(dt)

# Summarizing
wp_sum <- wp |> 
  group_by(individual, canopy, date) |> 
  summarize(wp_m = mean(wp),
            wp_sd = sd(wp),
            n = n()) |> 
  ungroup()

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
        legend.text = element_text(size = 13))

wp_sum |> 
  ggplot(aes(x = date, color = factor(individual), shape = canopy)) +
  geom_jitter(data = wp, aes(x = date, y = wp, color = factor(individual), shape = canopy), position = position_dodge(width = 0.35), alpha = 0.5, size = 3) +
  geom_errorbar(aes(ymin = wp_m - wp_sd, ymax = wp_m + wp_sd), position = position_dodge(width = 0.35), width = 0.2) +
  geom_point(aes(y = wp_m), position = position_dodge(width = 0.35), size = 4) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Date",
       y = expression(paste(Psi[PD], " (MPa)")),
       color = "Individual",
       shape = "Canopy") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))


