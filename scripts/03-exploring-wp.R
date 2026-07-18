#### Wrangling and visualizing pressure chamber measurements ####
library(tidyverse)
library(googledrive)
library(googlesheets4)
library(janitor)
library(RColorBrewer)
library(sp)
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
                               month == 07 & day > 15 ~ "recovery"),
         level = case_when(individual > 1 ~ 3,
                           individual == 1 ~ 2)) |> 
  filter(!is.na(wp)) |> 
  filter(is.na(comments)) |> 
  mutate(dt = as.POSIXct(paste(date, time))) |> 
  relocate(dt)

# Writing out to have locally
write_csv(wp, "data/all_wp.csv")

# Summarizing
wp_sum <- wp |> 
  group_by(individual, level, canopy, date, period, condition) |> 
  summarize(wp_m = mean(wp),
            wp_sd = sd(wp),
            n = n()) |> 
  ungroup() |> 
  mutate(dt = case_when(period == "PD" ~ as.POSIXct(paste(date, "4:30:00")),
                        period == "MD" ~ as.POSIXct(paste(date, "11:00:00")))) |> 
  relocate(dt)

wp_PDMD <- wp_sum |> 
  select(-n, -dt) |> 
  pivot_wider(names_from = period,
              values_from = c(wp_m, wp_sd)) |> 
  rename(PD_m = wp_m_PD, MD_m = wp_m_MD, PD_sd = wp_sd_PD, MD_sd = wp_sd_MD)

write_csv(wp_PDMD, "data/wp_PDMD.csv")

#### Visualizing ####

# Time series faceted by day
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

# Faceted by day and also canopy
# wp_sum |>
#   ggplot(aes(x = date, color = factor(individual), shape = period)) +
#   geom_jitter(data = wp, aes(x = date, y = wp, color = factor(individual), shape = period), position = position_dodge(width = 0.35), alpha = 0.5, size = 3) +
#   geom_errorbar(aes(ymin = wp_m - wp_sd, ymax = wp_m + wp_sd), position = position_dodge(width = 0.35), width = 0.2) +
#   geom_point(aes(y = wp_m), position = position_dodge(width = 0.35), size = 4) +
#   scale_color_brewer(palette = "Dark2") +
#   facet_wrap(canopy ~ date, scales = "free_x", ncol = length(unique(wp_sum$date))) +
#   labs(x = "Date",
#        y = expression(paste(Psi, " (MPa)")),
#        color = "Individual",
#        shape = "Period") +
#   theme(panel.grid = element_blank(),
#         axis.title = element_text(size = 15),
#         axis.text = element_text(size = 13),
#         legend.title = element_text(size = 15),
#         legend.text = element_text(size = 13))

# Time series, no background colors
# wp_sum |>
#   ggplot(aes(x = dt, y = wp_m)) +
#   geom_errorbar(aes(ymin = wp_m - wp_sd, ymax = wp_m + wp_sd, color = factor(individual)), position = position_dodge(width = 70000), width = 50000) +
#   geom_point(aes(color = factor(individual), shape = period), position = position_dodge(width = 70000), size = 4) +
#   scale_color_brewer(palette = "Dark2") +
#   facet_wrap(~ canopy, ncol = 1) +
#   labs(x = "Date",
#        y = expression(paste(Psi, " (MPa)")),
#        color = "Individual",
#        shape = "Period") +
#   theme(panel.grid = element_blank(),
#         axis.title = element_text(size = 15),
#         axis.text = element_text(size = 13),
#         legend.title = element_text(size = 15),
#         legend.text = element_text(size = 13))

# Time series with background colors and rain events
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
  # geom_line(aes(x = dt, y = wp_m, color = factor(individual), group = interaction(period, individual)), position = position_dodge(width = 70000), linewidth = 0.75) +
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

# Looking at MD/PD over time
wp_PDMD |> 
  ggplot() +
  geom_abline(aes(slope = 1, intercept = 0), linetype = "dashed", linewidth = 1) +
  geom_errorbar(aes(x = PD_m, ymin = MD_m - MD_sd, ymax = MD_m + MD_sd, color = date), alpha = 0.7, width = 0.05) +
  geom_errorbar(aes(y = MD_m, xmin = PD_m - PD_sd, xmax = PD_m + PD_sd, color = date), alpha = 0.7, width = 0.05) +
  geom_point(aes(x = PD_m, y = MD_m, color = date, shape = canopy), size = 2.5) +
  scale_x_continuous(limits = c(-3, 0)) +
  scale_y_continuous(limits = c(-3, 0)) +
  scale_color_viridis_c(option = "turbo", trans = "date") +
  facet_wrap(~canopy)

# Calculating LMs
wp_PDMD_l <- wp_PDMD |> 
  filter(canopy == "Lower canopy")
wp_PDMD_u <- wp_PDMD |> 
  filter(canopy == "Upper canopy")
wp_PDMD_3 <- wp_PDMD |> 
  filter(individual == 2 | individual == 3 | individual == 4)
wp_PDMD_2 <- wp_PDMD |> 
  filter(individual == 1)
regress_l <- lm(wp_PDMD_l$MD_m ~ wp_PDMD_l$PD_m)
summary(regress_l)
regress_u <- lm(wp_PDMD_u$MD_m ~ wp_PDMD_u$PD_m)
summary(regress_u)
regress_3 <- lm(wp_PDMD_3$MD_m ~ wp_PDMD_3$PD_m)
summary(regress_3)
regress_2 <- lm(wp_PDMD_2$MD_m ~ wp_PDMD_2$PD_m)
summary(regress_2)

# Calculating hydroscapes :D
hydroscape_canopy <- wp_PDMD |> 
  group_by(canopy) |>
  filter(!is.na(MD_m), !is.na(PD_m)) |> 
  slice(chull(PD_m, MD_m)) |> 
  ungroup()
  # To do a true hydroscape (intercept 1:1 line and 0,0)
  # add_row(individual = NA, level = NA, canopy = "Upper canopy", date = NA, condition = NA, MD_m = 0, PD_m = 0, MD_sd = NA, PD_sd = NA) |> 
  # add_row(individual = NA, level = NA, canopy = "Upper canopy", date = NA, condition = NA, MD_m = regress_u$coefficients[1] / (1 - regress_u$coefficients[2]), PD_m = regress_u$coefficients[1] / (1 - regress_u$coefficients[2]), MD_sd = NA, PD_sd = NA) |> 
  # add_row(individual = NA, level = NA, canopy = "Lower canopy", date = NA, condition = NA, MD_m = 0, PD_m = 0, MD_sd = NA, PD_sd = NA) |> 
  # add_row(individual = NA, level = NA, canopy = "Lower canopy", date = NA, condition = NA, MD_m = regress_l$coefficients[1] / (1 - regress_l$coefficients[2]), PD_m = regress_l$coefficients[1] / (1 - regress_l$coefficients[2]), MD_sd = NA, PD_sd = NA)

hydroscape_coords_u <- data.frame(hydroscape_canopy$PD_m, hydroscape_canopy$MD_m, hydroscape_canopy$canopy) |> 
  filter(hydroscape_canopy.canopy == "Upper canopy") |> select(-hydroscape_canopy.canopy)
hydroscape_poly_u <- Polygon(hydroscape_coords_u, hole = F)
hydroscape_area_u <- hydroscape_poly_u@area
hydroscape_area_u # Area of upper canopy points

hydroscape_coords_l <- data.frame(hydroscape_canopy$PD_m, hydroscape_canopy$MD_m, hydroscape_canopy$canopy) |> 
  filter(hydroscape_canopy.canopy == "Lower canopy") |> select(-hydroscape_canopy.canopy)
hydroscape_poly_l <- Polygon(hydroscape_coords_l, hole = F)
hydroscape_area_l <- hydroscape_poly_l@area
hydroscape_area_l # Area of upper canopy points

hydroscape_level <- wp_PDMD |> 
  group_by(level) |>
  filter(!is.na(MD_m), !is.na(PD_m)) |> 
  slice(chull(PD_m, MD_m)) |> 
  ungroup()
# To do a true hydroscape (intercept 1:1 line and 0,0)
# add_row(individual = NA, level = 2, canopy = NA, date = NA, condition = NA, MD_m = 0, PD_m = 0, MD_sd = NA, PD_sd = NA) |> 
# add_row(individual = NA, level = 2, canopy = NA, date = NA, condition = NA, MD_m = regress_2$coefficients[1] / (1 - regress_2$coefficients[2]), PD_m = regress_2$coefficients[1] / (1 - regress_2$coefficients[2]), MD_sd = NA, PD_sd = NA) |> 
# add_row(individual = NA, level = 3, canopy = NA, date = NA, condition = NA, MD_m = 0, PD_m = 0, MD_sd = NA, PD_sd = NA) |> 
# add_row(individual = NA, level = 3, canopy = NA, date = NA, condition = NA, MD_m = regress_3$coefficients[1] / (1 - regress_3$coefficients[2]), PD_m = regress_3$coefficients[1] / (1 - regress_3$coefficients[2]), MD_sd = NA, PD_sd = NA)

hydroscape_coords_2 <- data.frame(hydroscape_level$PD_m, hydroscape_level$MD_m, hydroscape_level$level) |> 
  filter(hydroscape_level.level == 2) |> select(-hydroscape_level.level)
hydroscape_poly_2 <- Polygon(hydroscape_coords_2, hole = F)
hydroscape_area_2 <- hydroscape_poly_2@area
hydroscape_area_2 # Area of level 2 points

hydroscape_coords_3 <- data.frame(hydroscape_level$PD_m, hydroscape_level$MD_m, hydroscape_level$level) |> 
  filter(hydroscape_level.level == 3) |> select(-hydroscape_level.level)
hydroscape_poly_3 <- Polygon(hydroscape_coords_3, hole = F)
hydroscape_area_3 <- hydroscape_poly_3@area
hydroscape_area_3 # Area of level 3 points

# Plotting regressions, hydroscapes
# By canopy level
wp_PDMD |> 
  ggplot() +
  annotate("text", x = -2, y = -0.25, label = paste("Upper slope: ", round(regress_u$coefficients[2], 2)), size = 4.5, color = "tomato") +
  annotate("text", x = -2, y = -0.5, label = paste("Lower slope: ", round(regress_l$coefficients[2], 2)), size = 4.5, color = "skyblue2") +
  annotate("text", x = -2, y = -0.75, label = paste("Upper area: ", round(hydroscape_area_u, 2)), size = 4.5, color = "tomato") +
  annotate("text", x = -2, y = -1, label = paste("Lower area: ", round(hydroscape_area_l, 2)), size = 4.5, color = "skyblue2") +
  geom_abline(aes(slope = 1, intercept = 0), linetype = "dashed", linewidth = 1) +
  geom_abline(aes(slope = regress_l$coefficients[2], intercept = regress_l$coefficients[1]), color = "skyblue2") +
  geom_abline(aes(slope = regress_u$coefficients[2], intercept = regress_u$coefficients[1]), color = "tomato") +
  geom_polygon(data = hydroscape_canopy, aes(x = PD_m, y = MD_m, fill = canopy), alpha = 0.2) +  # fill = canopy
  geom_errorbar(aes(x = PD_m, ymin = MD_m - MD_sd, ymax = MD_m + MD_sd, color = canopy), alpha = 0.5, width = 0.05) +
  geom_errorbar(aes(y = MD_m, xmin = PD_m - PD_sd, xmax = PD_m + PD_sd, color = canopy), alpha = 0.5, width = 0.05) +
  geom_point(aes(x = PD_m, y = MD_m, color = canopy, shape = canopy), size = 2.5) +
  scale_x_continuous(limits = c(-3, 0)) +
  scale_y_continuous(limits = c(-3, 0)) +
  scale_color_manual(values = c("skyblue2", "tomato")) +
  scale_fill_manual(values = c("skyblue2", "tomato")) +
  labs(x = expression(paste(Psi[PD])), y = expression(paste(Psi[MD])), fill = "Canopy level", shape = "Canopy level", color = "Elevation") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))

# By rainforest level (elevation)
wp_PDMD |> 
  ggplot() +
  annotate("text", x = -2, y = -0.25, label = paste("2nd level slope: ", round(regress_2$coefficients[2], 2)), size = 4.5, color = "mediumseagreen") +
  annotate("text", x = -2, y = -0.5, label = paste("3rd level slope: ", round(regress_3$coefficients[2], 2)), size = 4.5, color = "orchid3") +
  annotate("text", x = -2, y = -0.75, label = paste("2nd level area: ", round(hydroscape_area_2, 2)), size = 4.5, color = "mediumseagreen") +
  annotate("text", x = -2, y = -1, label = paste("3rd level area: ", round(hydroscape_area_3, 2)), size = 4.5, color = "orchid3") +
  geom_abline(aes(slope = 1, intercept = 0), linetype = "dashed", linewidth = 1) +
  geom_abline(aes(slope = regress_3$coefficients[2], intercept = regress_3$coefficients[1]), color = "orchid3") +
  geom_abline(aes(slope = regress_2$coefficients[2], intercept = regress_2$coefficients[1]), color = "seagreen") +
  geom_polygon(data = hydroscape_level, aes(x = PD_m, y = MD_m, fill = factor(level)), alpha = 0.2) +  # fill = canopy
  geom_errorbar(aes(x = PD_m, ymin = MD_m - MD_sd, ymax = MD_m + MD_sd, color = factor(level)), alpha = 0.5, width = 0.05) +
  geom_errorbar(aes(y = MD_m, xmin = PD_m - PD_sd, xmax = PD_m + PD_sd, color = factor(level)), alpha = 0.5, width = 0.05) +
  geom_point(aes(x = PD_m, y = MD_m, color = factor(level), shape = canopy), size = 2.5) +
  scale_x_continuous(limits = c(-3, 0)) +
  scale_y_continuous(limits = c(-3, 0)) +
  scale_color_manual(values = c("mediumseagreen", "orchid3")) +
  scale_fill_manual(values = c("mediumseagreen", "orchid3")) +
  labs(x = expression(paste(Psi[PD])), y = expression(paste(Psi[MD])), fill = "Elevation", shape = "Canopy level", color = "Elevation") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))





#### Statsy stuff ####
wp_super_sum <- wp_sum |> 
  group_by(dt, date, period, canopy, condition) |> 
  summarize(wp.m = mean(wp_m),
            wp.sd = sd(wp_m),
            n = n()) |> ungroup()

wp_super_sum |> 
  ggplot(aes(x = factor(condition, levels = c("predrought", "drought", "recovery")), y = wp.m, group = interaction(canopy, period, condition), color = interaction(period, canopy))) +
  geom_boxplot() +
  labs(x = "Condition", y = expression(paste(Psi, " (MPa)")), color = "Period, Canopy level") +
  scale_color_manual(values = c("tomato", "plum3", "skyblue2", "khaki3")) +
  theme(panel.grid = element_blank())

test_aov <- aov(wp.m ~ canopy + condition + period, wp_super_sum)
summary(test_aov)

midday_only <- wp_super_sum |> 
  filter(period == "MD")

midday_aov <- aov(wp.m ~ canopy + condition, data = midday_only)
summary(midday_aov)
