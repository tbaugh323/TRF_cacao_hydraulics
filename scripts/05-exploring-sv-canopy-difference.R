#### Testing analysis between sapflow and other vars ####
library(tidyverse)
library(patchwork)
library(cowplot)
library(dygraphs)
library(segmented)
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
  mutate(Tree_ID = "BioR1171", Tree_ID_location = paste0(Tree_ID, "_", location))
  # pivot_wider(names_from = location,
  #             values_from = c(VhrmHRM5, VhrmHRM15, VhrmHRM25, VhrmHRM35))

wp_small <- wp_PDMD_long |> filter(date %in% overlap_list, Tree_ID == "BioR1171")

sv_wp <- full_join(R1171_small, wp_small, by = c("dt", "date", "Tree_ID"))

sv_wp |> 
  ggplot() +
  geom_line(aes(x = dt, y = VhrmHRM5, group = date), color = "forestgreen") +
  geom_point(aes(x = dt, y = VhrmHRM5), color = "forestgreen") +
  geom_point(aes(x = dt, y = wp_m * 8, color = period), size = 2, alpha = 0.7) +
  facet_wrap(~ Tree_ID_location) +
  scale_color_manual(values = c("orchid3", "chocolate1")) +
  scale_y_continuous("Sap flow velocity at 5 mm (cm/hr)",
                     sec.axis = sec_axis(~ . / 8, name = expression(paste(Psi)),
                                         breaks = seq(0, -3.5, -0.5))) +
  theme(panel.grid = element_blank())

sv_wp |> 
  filter(canopy == "Upper canopy") |> 
  filter(location == "EB" | location == "LB") |> 
  mutate(location = case_when(location == "EB" ~ "Upper canopy (east branch)",
                              location == "LB" ~ "Lower canopy (lower branch)")) |> 
  ggplot(aes(x = VhrmHRM5, y = wp_m)) +
  geom_point(aes(color = period, shape = period), size = 5) +
  scale_color_manual(values = c("orchid3", "chocolate1")) +
  facet_wrap(~ factor(location, levels = c("Upper canopy (east branch)", "Lower canopy (lower branch)")), ncol = 1) +
  labs(x = "Sap flow velocity at 5 mm (cm/hr)",
       y = expression(paste(Psi)),
       color = "Period", shape = "Period") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 23),
        axis.text = element_text(size = 23),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 23),
        plot.title = element_text(hjust = 0.5, size = 25),
        strip.text = element_text(size = 23, color = "white"),
        strip.background = element_rect(fill = "#205A3D"))

# first <- sv_wp |> 
#   filter(canopy == "Upper canopy") |> 
#   ggplot(aes(x = VhrmHRM5_EB, y = wp_m)) +
#   ggtitle("Upper canopy (East Branch)") +
# # geom_line(aes(group = interaction(period)), alpha = 0.5) +
#   geom_point(aes(color = period, shape = period), size = 5) +
#   scale_color_manual(values = c("orchid3", "chocolate1")) +
#   labs(x = "Sap flow velocity at 5 mm (cm/hr)",
#        y = expression(paste(Psi)),
#        color = "Period", shape = "Period") +
#   theme(panel.grid = element_blank(),
#         axis.title = element_text(size = 23),
#         axis.text = element_text(size = 23),
#         axis.title.x = element_blank(),
#         legend.title = element_text(size = 25),
#         legend.text = element_text(size = 23),
#         plot.title = element_text(hjust = 0.5, size = 25))
# 
# second <- sv_wp |> 
#   filter(canopy == "Lower canopy") |> 
#   ggplot(aes(x = VhrmHRM5_LB, y = wp_m)) +
#   ggtitle("Lower canopy (Lower Branch)") +
#   # geom_line(aes(group = interaction(period)), alpha = 0.5) +
#   geom_point(aes(color = period, shape = period), size = 5) +
#   scale_color_manual(values = c("orchid3", "chocolate1")) +
#   # scale_color_viridis_c(option = "turbo", trans = "date") +
#   labs(x = expression(paste(V["s,5"], "(cm/hr)")),
#        y = expression(paste(Psi)),
#        color = "Period", shape = "Period") +
#   theme(panel.grid = element_blank(),
#         axis.title = element_text(size = 23),
#         axis.text = element_text(size = 23),
#         legend.title = element_text(size = 25),
#         legend.text = element_text(size = 23),
#         plot.title = element_text(hjust = 0.5, size = 25))
# 
# first / second

# yeah we're regressing it
sv_wp_upper <- sv_wp |> filter(canopy == "Upper canopy")
linreg_u <- lm(wp_m ~ VhrmHRM5_LB, data = sv_wp_upper)
segreg_u <- segmented(linreg_u, psi = 0.5)
summary(segreg_u)
# Breakpoint: 5.785
# Confint: (3.13611, 8.4329)

davies.test(linreg_u)

changepoint_u <- 5.785
slope1_u <- 0.39388
slope2_u <- -1.36550
intercept1_u <- -1.4860
intercept2_u <- 8.6912

sv_wp_lower <- sv_wp |> filter(canopy == "Lower canopy")
linreg_l <- lm(wp_m ~ VhrmHRM5_LB, data = sv_wp_lower)
segreg_l <- segmented(linreg_l)
summary(segreg_l)
# Breakpoint: 1.063
# Confint: (-0.814759, 2.9405)

davies.test(linreg_l)

changepoint_l <- 1.063
slope1_l <- 0.575250
slope2_l <- -0.078684
intercept1_l <- -1.11390
intercept2_l <- -0.41886

# third <- first +
#   annotate("segment", x = -3, y = intercept1_u - 3 * slope1_u, xend = changepoint_u, yend = (slope1_u * changepoint_u) + intercept1_u, linewidth = 1) +
#   annotate("segment", x = changepoint_u, y = (slope1_u * changepoint_u) + intercept1_u, xend = 8, yend = 8 * slope2_u + intercept2_u, linewidth = 1)
# 
# fourth <- second +
#   annotate("segment", x = -3, y = intercept1_l - 3 * slope1_l, xend = changepoint_l, yend = (slope1_l * changepoint_l) + intercept1_l, linewidth = 1) +
#   annotate("segment", x = changepoint_l, y = (slope1_l * changepoint_l) + intercept1_l, xend = 8, yend = 8 * slope2_l + intercept2_l, linewidth = 1)
# 
# first / fourth

sv_wp |> 
  filter(canopy == "Upper canopy") |> 
  filter(location == "EB" | location == "LB") |> 
  mutate(location = case_when(location == "EB" ~ "Upper canopy (east branch)",
                              location == "LB" ~ "Lower canopy (lower branch)")) |> 
  ggplot(aes(x = VhrmHRM5, y = wp_m)) +
  annotate("segment", x = -3, y = intercept1_l - 3 * slope1_l, xend = changepoint_l, yend = (slope1_l * changepoint_l) + intercept1_l, linewidth = 1) +
  annotate("segment", x = changepoint_l, y = (slope1_l * changepoint_l) + intercept1_l, xend = 8, yend = 8 * slope2_l + intercept2_l, linewidth = 1) +
  geom_point(aes(color = period, shape = period), size = 5) +
  scale_color_manual(values = c("orchid3", "chocolate1")) +
  facet_wrap(~ factor(location, levels = c("Upper canopy (east branch)", "Lower canopy (lower branch)")), ncol = 1) +
  labs(x = "Sap flow velocity at 5 mm (cm/hr)",
       y = expression(paste(Psi)),
       color = "Period", shape = "Period") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 23),
        axis.text = element_text(size = 23),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 23),
        plot.title = element_text(hjust = 0.5, size = 25),
        strip.text = element_text(size = 23, color = "white"),
        strip.background = element_rect(fill = "#205A3D"))


