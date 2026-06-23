#### Looking at test data from LICOR-600 ####
# Data collected on 2026-06-18
# roughly was ascending with time
library(tidyverse)
library(hms)
theme_set(theme_bw())

#### Reading in data ####
test1 <- read_csv("data/test1.csv") |> 
  filter(configAuthor == "LI-COR Default") |> 
  mutate(Date = as.Date(Date))

test2 <- read_csv("data/test2.csv") |> 
  filter(configAuthor == "LI-COR Default") |> 
  mutate(Date = as.Date(Date))

test3 <- read_csv("data/test3.csv") |> 
  filter(configAuthor == "LI-COR Default") |> 
  mutate(Date = as.Date(Date))

test4 <- read_csv("data/test4.csv") |> 
  filter(configAuthor == "LI-COR Default") |> 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
test4 <- test4[,1:108]

#### Merging ####

all_data <- rbind(test1, test2, test3, test4) |> 
  mutate(super_obs = row_number(),
         Time = as_hms(Time)) |> 
  relocate(super_obs)

all_data$plant_id <- c(rep(1, 4), rep(2, 5), rep(3, 4), rep(4, 2), rep(5, 5), rep(6, 5), rep(7, 5))
all_data <- all_data |> 
  relocate(plant_id) |> 
  mutate(across(c(Observation:leaf_width, Fo:batt, rh_adj:Ble, flash_intensity:z_flr), as.numeric))

#### Visualizing ####

# Vars over time
all_data |> 
  ggplot(aes(x = Time, y = gsw)) + 
  geom_point(aes(color = factor(plant_id)), size = 2) +
  theme(panel.grid = element_blank())

all_data |> 
  ggplot(aes(x = Time, y = Fs)) +
  geom_point(aes(color = factor(plant_id)), size = 2) +
  theme(panel.grid = element_blank())

all_data |> 
  ggplot(aes(x = Time, y = VPDleaf)) +
  geom_point(aes(color = factor(plant_id)), size = 2) +
  theme(panel.grid = element_blank())

all_data |> 
  ggplot(aes(x = Time, y = Tleaf)) +
  geom_point(aes(color = factor(plant_id)), size = 2) +
  theme(panel.grid = element_blank())

# Vars over met

all_data |> 
  ggplot(aes(x = Tleaf, y = gsw)) +
  geom_point(aes(color = factor(plant_id)), size = 2) +
  theme(panel.grid = element_blank())

all_data |> 
  ggplot(aes(x = VPDleaf, y = gsw)) +
  geom_point(aes(color = factor(plant_id)), size = 2) +
  theme(panel.grid = element_blank())

all_data |> 
  ggplot(aes(x = rh_s, y = gsw)) +
  geom_point(aes(color = factor(plant_id)), size = 2) +
  theme(panel.grid = element_blank())

