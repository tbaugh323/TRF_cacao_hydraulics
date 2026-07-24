#### Testing analysis between sapflow and other vars ####
library(tidyverse)
library(patchwork)
library(cowplot)
library(dygraphs)
theme_set(theme_bw())

#### Reading ####
# this one is IN MST
wp_PDMD_long <- read_csv("data/water_potential/wp_PDMD_long.csv") |> 
  mutate(Tree_ID = case_when(individual == 1 ~ "BioR1192",
                             individual == 2 ~ "BioR1171",
                             individual == 3 ~ "BioR1170",
                             individual == 4 ~ "BioR1192"),
         dt = as.POSIXct(dt, tz = "MST"))
# this one is IN UTC
R1171 <- read_csv("data/sap_flow/raw_sapflow/R1171-postprocess.csv") |> 
  rename(date = Date) |> 
  mutate(Datetime = as.POSIXct(Datetime, tz = "MST"),
         date = as.Date(Datetime))

#### Finding overlap with manual water potential data ####

wp_list <- unique(wp_PDMD_long$date)
sv_list <- unique(R1171$date)
overlap_list <- wp_list[wp_list %in% sv_list]

R1171_small <- R1171 |> filter(date %in% overlap_list) |> 
  rename(dt = Datetime) |> 
  dplyr::select(dt, date, location, VhrmHRM5, VhrmHRM15, VhrmHRM25, VhrmHRM35) |> 
  pivot_wider(names_from = location,
              values_from = c(VhrmHRM5, VhrmHRM15, VhrmHRM25, VhrmHRM35))

wp_small <- wp_PDMD_long |> filter(date %in% overlap_list, Tree_ID == "BioR1171")

sv_wp <- full_join(R1171_small, wp_small, by = c("dt", "date"))

sv_wp |> 
  ggplot() +
  geom_line(aes(x = dt, y = VhrmHRM5_M, group = date), color = "forestgreen") +
  geom_point(aes(x = dt, y = VhrmHRM5_M), color = "forestgreen") +
  geom_point(aes(x = dt, y = wp_m * 8, color = period), size = 2, alpha = 0.7) +
  scale_color_manual(values = c("orchid3", "chocolate1")) +
  scale_y_continuous("Sap flow velocity at 5 cm",
                     sec.axis = sec_axis(~ . / 8, name = expression(paste(Psi)),
                                         breaks = seq(0, -3.5, -0.5))) +
  theme(panel.grid = element_blank())

first <- sv_wp |> 
  filter(canopy == "Upper canopy") |> 
  ggplot(aes(x = VhrmHRM5_EB, y = wp_m)) +
  ggtitle("Upper canopy") +
# geom_line(aes(group = interaction(period)), alpha = 0.5) +
  geom_point(aes(color = period, shape = period), size = 4) +
  scale_color_manual(values = c("orchid3", "chocolate1")) +
  labs(x = "East Branch Sap flow velocity at 5 cm",
       y = expression(paste(Psi)),
       color = "Period", shape = "Period") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        plot.title = element_text(size = 15))

second <- sv_wp |> 
  filter(canopy == "Lower canopy") |> 
  ggplot(aes(x = VhrmHRM5_LB, y = wp_m)) +
  ggtitle("Lower canopy") +
  # geom_line(aes(group = interaction(period)), alpha = 0.5) +
  geom_point(aes(color = period, shape = period), size = 4) +
  scale_color_manual(values = c("orchid3", "chocolate1")) +
  # scale_color_viridis_c(option = "turbo", trans = "date") +
  labs(x = "Low Branch Sap flow velocity at 5 cm",
       y = expression(paste(Psi)),
       color = "Period", shape = "Period") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        plot.title = element_text(size = 15))

first / second

# yeah we're regressing it
sv_wp_upper <- sv_wp |> filter(canopy == "Upper canopy")
linreg_u <- lm(wp_m ~ VhrmHRM5_LB, data = sv_wp_upper)
segreg_u <- segmented(linreg_u, psi = 0)
summary(segreg_u)
# Breakpoint: 5.785
# Confint: (3.13611, 8.4329)
# slope1: 0.39388
# slope2: -1.36550
# intercept1: -1.4860
# intercept2: 8.6912
davies.test(linreg_u) # there's not a segment!

changepoint_u <- 5.785
slope1_u <- 0.39388
slope2_u <- -1.36550
intercept1_u <- -1.4860
intercept2_u <- 8.6912

sv_wp_lower <- sv_wp |> filter(canopy == "Lower canopy")
linreg_l <- lm(wp_m ~ VhrmHRM5_LB, data = sv_wp_lower)
segreg_l <- segmented(linreg_l, psi = 0)
summary(segreg_l)
# Breakpoint: 1.063
# Confint: (-0.814759, 2.9405)
# slope1: 0.575250
# slope2: -0.078684
# intercept1: -1.11390
# intercept2: -0.41886
davies.test(linreg_l) # there's not a segment!

changepoint_l <- 1.063
slope1_l <- 0.575250
slope2_l <- -0.078684
intercept1_l <- -1.11390
intercept2_l <- -0.41886

third <- first +
  annotate("segment", x = -3, y = intercept1_u - 3 * slope1_u, xend = changepoint_u, yend = (slope1_u * changepoint_u) + intercept1_u) +
  annotate("segment", x = changepoint_u, y = (slope1_u * changepoint_u) + intercept1_u, xend = 8, yend = 8 * slope2_u + intercept2_u)

fourth <- second +
  annotate("segment", x = -3, y = intercept1_l - 3 * slope1_l, xend = changepoint_l, yend = (slope1_l * changepoint_l) + intercept1_l) +
  annotate("segment", x = changepoint_l, y = (slope1_l * changepoint_l) + intercept1_l, xend = 8, yend = 8 * slope2_l + intercept2_l)

first / fourth

#### Finding overlap with manual stomatal conductance data ####

# gs_list <- unique(all_licor600$Date)
# sv_list <- unique(R1171$Date)
# overlap_list <- gs_list[gs_list %in% sv_list]
# 
# R1171_small <- R1171 |> filter(Date %in% overlap_list) |> 
#   rename(dt = Datetime) |> 
#   dplyr::select(dt, location, VhrmHRM5, VhrmHRM15, VhrmHRM25, VhrmHRM35) |> 
#   pivot_wider(names_from = location,
#               values_from = c(VhrmHRM5, VhrmHRM15, VhrmHRM25, VhrmHRM35))
# gs_small <- all_licor600 |> filter(Date %in% overlap_list) |> 
#   filter(configAuthor == "LI-COR Default") |> 
#   mutate(dt = round_date(dt, unit = "30 minutes"))
# 
# sv_gs <- full_join(R1171_small, gs_small, by = "dt")
# 
# sv_gs |> 
#   ggplot() +
#   geom_line(aes(x = dt, y = VhrmHRM5_M), color = "forestgreen") +
#   geom_point(aes(x = dt, y = VhrmHRM5_M), color = "forestgreen") +
#   geom_point(aes(x = dt, y = gsw * 30), color = "chocolate1") +
#   scale_y_continuous("Sap flow velocity at 5 cm",
#                      sec.axis = sec_axis(~ . * 0.3, name = expression(paste(g[s], " (mol ", m^-2, s^-1, ")")))) +
#   theme(panel.grid = element_blank())
# 
# first <- sv_gs |> 
#   filter(canopy == "lower") |> 
#   ggplot(aes(x = VhrmHRM5_LB, y = gsw)) +
#   geom_point(aes(color = Date)) +
#   scale_color_viridis_c(option = "turbo", trans = "date") +
#   theme(panel.grid = element_blank())
# 
# second <- sv_gs |> 
#   filter(canopy == "upper") |> 
#   ggplot(aes(x = VhrmHRM5_EB, y = gsw)) +
#   geom_point(aes(color = Date)) +
#   scale_color_viridis_c(option = "turbo", trans = "date") +
#   theme(panel.grid = element_blank())
# 
# second / first
# 
