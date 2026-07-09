#### Testing analysis between sapflow and other vars ####
library(tidyverse)
library(patchwork)
library(cowplot)
theme_set(theme_bw())

#### Reading and visualizing ####
all_wp <- read_csv("data/all_wp.csv")
all_licor600 <- read_csv("data/all_licor600.csv")
tree_2_all <- read_csv("data/sap_flow/tree_2/tree_2_all.csv")

tree_2_all |> 
  ggplot(aes(x = Datetime, y = VhrmHRM5)) +
  geom_line(aes(color = location))

tree_2_dy <- tree_2_all |> 
  select(Datetime, location, VhrmHRM5) |> 
  pivot_wider(names_from = location,
              values_from = VhrmHRM5)
tree_2_dy <- xts::xts(tree_2_dy[,-1], order.by = tree_2_dy$Datetime)

dygraph(tree_2_dy) |> 
  dySeries("T") |>
  dySeries("SB") |>
  dySeries("NB") |>
  dySeries("MB") |>
  dySeries("M") |>
  dySeries("LB") |>
  dySeries("L") |>
  dySeries("EB") |>
  dyAxis("y", label = "VhrmHRM5") |> 
  dyRangeSelector()

#### Finding overlap with manual data ####

gs_list <- unique(all_licor600$Date)
sv_list <- unique(tree_2_all$Date)
overlap_list <- gs_list[gs_list %in% sv_list]

tree_2_small <- tree_2_all |> filter(Date %in% overlap_list) |> 
  rename(dt = Datetime) |> 
  select(dt, location, VhrmHRM5, VhrmHRM15, VhrmHRM25, VhrmHRM35) |> 
  pivot_wider(names_from = location,
              values_from = c(VhrmHRM5, VhrmHRM15, VhrmHRM25, VhrmHRM35))
gs_small <- all_licor600 |> filter(Date %in% overlap_list) |> 
  filter(configAuthor == "LI-COR Default") |> 
  mutate(dt = round_date(dt, unit = "30 minutes"))

sv_gs <- full_join(tree_2_small, gs_small, by = "dt")

sv_gs |> 
  ggplot() +
  geom_point(aes(x = dt, y = VhrmHRM5_M), color = "forestgreen") +
  geom_point(aes(x = dt, y = gsw * 30), color = "chocolate1") +
  scale_y_continuous("Sap flow velocity at 5 cm",
                     sec.axis = sec_axis(~ . * 0.3, name = expression(paste(g[s], " (mol ", m^-2, s^-1, ")")))) +
  theme(panel.grid = element_blank())

first <- sv_gs |> 
  filter(canopy == "lower") |> 
  ggplot(aes(x = VhrmHRM5_LB, y = gsw)) +
  geom_point(aes(color = Date)) +
  scale_color_viridis_c(option = "turbo", trans = "date") +
  theme(panel.grid = element_blank())

second <- sv_gs |> 
  filter(canopy == "upper") |> 
  ggplot(aes(x = VhrmHRM5_EB, y = gsw)) +
  geom_point(aes(color = Date)) +
  scale_color_viridis_c(option = "turbo", trans = "date") +
  theme(panel.grid = element_blank())

second / first
