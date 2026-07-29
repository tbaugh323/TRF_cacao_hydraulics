#### Looking at TWD and WP data together ####
library(tidyverse)
library(segmented)
library(minpack.lm)

#### Prep data ####
wp_PDMD <- read_csv("data/water_potential/wp_PDMD.csv") |> 
  mutate(Tree_ID = case_when(individual == 1 ~ "BioR1192",
                             individual == 2 ~ "BioR1171",
                             individual == 3 ~ "BioR1170",
                             individual == 4 ~ "BioR1191"))
all_dendros <- read_csv("data/dendro_data/all_dendros.csv") |> 
  mutate(datetime = as.POSIXct(datetime, tz = "America/Phoenix"))

#### Joining #### 

wp_list <- unique(wp_PDMD_long$date)
dendro_list <- unique(all_dendros$date)
overlap_list <- wp_list[wp_list %in% dendro_list]

dendro_wp <- merge(wp_PDMD, all_dendros, by = c("date", "Tree_ID")) |> 
  filter(date %in% overlap_list)

#### Calculate |λTWD[min]| the Peters way ####

dendro_wp |> 
  ggplot(aes(x = PD_m, y = twd_min)) +
  geom_point() +
  facet_wrap(~ Tree_ID)

# doing slopes
dendro_wp_lms <- data.frame(Tree_ID = NA, slope = NA, intercept = NA, r2 = NA)
for (i in 1:length(unique(dendro_wp$Tree_ID))) {
  current_tree <- dendro_wp |> 
    filter(Tree_ID == unique(dendro_wp$Tree_ID)[i])
  
  current_lm <- lm(current_tree$twd_min ~ current_tree$PD_m)
  
  dendro_wp_lms <- dendro_wp_lms |> 
    add_row(Tree_ID = unique(dendro_wp$Tree_ID)[i], 
            slope = current_lm$coefficients[2], 
            intercept = current_lm$coefficients[1], 
            r2 = summary(current_lm)$r.squared)
}
dendro_wp_lms <- dendro_wp_lms |> filter(!is.na(Tree_ID)) |> 
  rename(lambda_twd = slope)

# plotting
dendro_wp |> 
  mutate(condition = case_when(condition == "drought" ~ "Drought",
                               condition == "predrought" ~ "Predrought",
                               condition == "recovery" ~ "Recovery")) |>
  ggplot(aes(x = PD_m, y = twd_min)) +
  geom_abline(data = dendro_wp_lms, aes(slope = lambda_twd, 
                                        intercept = intercept), linewidth = 1) +
  geom_point(aes(color = factor(condition, 
                                levels = c("Predrought", "Drought", "Recovery"))), 
             size = 5.5) + # aes(color = date)
  scale_color_manual(values = c("springgreen4", "lightsalmon3", "yellowgreen")) +
  facet_wrap(~ Tree_ID) +
  labs(x = expression(paste(Psi[PD])), y = expression(paste(TWD[min])),
       color = "Condition") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 28),
        axis.text = element_text(size = 26),
        strip.text = element_text(size = 26, color = "white"),
        legend.title = element_text(size = 28),
        legend.text = element_text(size = 26),
        strip.background = element_rect(fill = "#205A3D"))

# not excellent fits, but these will be |λTWD[min]|
dendro_wp_abs <- dendro_wp_lms |> 
  mutate(lambda_twd = abs(lambda_twd))
write_csv(dendro_wp_abs, "data/dendro_data/lambda_twd_min.csv")

#### Looking at Psi[PD] to TWD[min] patterns (Ziegler stuff) ####

dendro_wp |> 
  ggplot(aes(x = PD_m, y = twd_min)) +
  geom_point(aes(color = condition, shape = Tree_ID), size = 4) +
  xlim(c(0, -2.5)) +
  facet_wrap(~ Tree_ID)

dendro_wp |> 
  mutate(condition = case_when(condition == "drought" ~ "Drought",
                               condition == "predrought" ~ "Predrought",
                               condition == "recovery" ~ "Recovery")) |>
  ggplot(aes(x = twd_min, y = PD_m)) +
  geom_point(aes(color = factor(condition, 
                                levels = c("Predrought", "Drought", "Recovery")), 
                 shape = Tree_ID), size = 4) +
  scale_color_manual(values = c("springgreen4", "lightsalmon3", "yellowgreen")) +
  labs(x = expression(paste(TWD[PD])), 
       y = expression(paste(Psi[PD])), color = "Condition") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))
# We have a two phase relationship here, not three :O

#### Regressions for Ziegler method ####

# Lots of repeated values from having the subhourly time series, get rid of that
cp_hold <- dendro_wp |> 
  dplyr::select(date, Tree_ID, canopy, twd_min, PD_m) |> 
  distinct()

# Segmented linear regression
result <- lm(PD_m ~ twd_min, data = cp_hold)

seg_result <- segmented(result, psi = 0.7)

davies.test(result) # there is a segment!

summary(seg_result)

# Estimated break point
# twd_min = 3.404
# R-squared = 0.8604  # pretty good!
# confint(seg_result)
# (3.32982, 3.47854)

# twd_min
changepoint <- 0.556385
slope1 <- -0.11498
slope2 <- -6.27190
intercept1 <- -0.40993
intercept2 <- 3.01570

dendro_wp |> 
  mutate(condition = case_when(condition == "drought" ~ "Drought",
                               condition == "predrought" ~ "Predrought",
                               condition == "recovery" ~ "Recovery")) |> 
  ggplot(aes(x = twd_min, y = PD_m)) + 
  ggtitle("Segmented linear regression") +
  annotate(geom = "segment", x = 0, y = intercept1, 
           xend = changepoint, yend = (slope1 * changepoint) + intercept1) +
  annotate(geom = "segment", x = changepoint, 
           y = (slope1 * changepoint) + intercept1, 
           xend = 0.8, yend = 0.8 * slope2 + intercept2) +
  # geom_segment(aes(x = 0, y = intercept1, xend = changepoint,
  #                  yend = (slope1 * changepoint) + intercept1)) +
  # geom_segment(aes(x = changepoint, 
  #                  y = (slope1 * changepoint) + intercept1, 
  #                  xend = 4, yend = 4 * slope2 + intercept2)) +
  geom_point(aes(color = factor(condition, 
                                levels = c("Predrought", "Drought", "Recovery")), 
                 shape = Tree_ID), size = 4) +
  scale_color_manual(values = c("springgreen4", "lightsalmon3", "yellowgreen")) +
  labs(x = expression(paste(TWD[PD])), y = expression(paste(Psi[PD])), 
       color = "Condition") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        title = element_text(size = 13))

# Flipped axes
# dendro_wp |> 
#   ggplot(aes(x = PD_m, y = twd_min)) + 
#   annotate(geom = "segment", y = 0, x = intercept1, yend = changepoint, xend = (slope1 * changepoint) + intercept1) +
#   annotate(geom = "segment", y = changepoint, x = (slope1 * changepoint) + intercept1, yend = 0.8, xend = 0.8 * slope2 + intercept2) +
#   # geom_segment(aes(x = 0, y = intercept1, xend = changepoint, yend = (slope1 * changepoint) + intercept1)) +
#   # geom_segment(aes(x = changepoint, y = (slope1 * changepoint) + intercept1, xend = 4, yend = 4 * slope2 + intercept2)) +
#   geom_point(aes(color = condition, shape = Tree_ID), size = 4) +
#   labs(y = expression(paste(TWD[PD])), x = expression(paste(Psi[PD])), color = "Condition") +
#   theme(panel.grid = element_blank(),
#         axis.title = element_text(size = 15),
#         axis.text = element_text(size = 13),
#         legend.title = element_text(size = 15),
#         legend.text = element_text(size = 13))

# Exponential regression

x <- dendro_wp$twd_min
y <- dendro_wp$PD_m
hold_df <- data.frame(x, y)
ex_start_values <- c(a = 1, b = 2)
ex_fit <- nls(y ~ a * exp(b * x),
           start = ex_start_values,
           algorithm = "port",
           control = nls.control(maxiter = 1000))
summary(ex_fit)

ggplot() +
  geom_line(data = hold_df, aes(x, predict(ex_fit, newdata = data.frame(x))),
            linewidth = 1) +
  geom_point(data = dendro_wp |> 
               mutate(condition = case_when(condition == "drought" ~ "Drought",
                                            condition == "predrought" ~ "Predrought",
                                            condition == "recovery" ~ "Recovery")),
             aes(x = twd_min, y = PD_m, 
                 color = factor(condition, 
                                levels = c("Predrought", "Drought", "Recovery")), 
                 shape = Tree_ID), size = 5) +
  # ggtitle("Exponential Regression") +
  scale_color_manual(values = c("springgreen4", "lightsalmon3", "yellowgreen")) +
  labs(x = expression(paste(TWD[min])), y = expression(paste(Psi[PD])), 
       color = "Condition") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 23),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 23),
        title = element_text(size = 23))

# Sigmoidal regression (doesn't look super good :( )
sig_start_values <- c(A = -2, b = 5, c = -0.4)
sig_fit <- nlsLM(y ~ A / (1 + exp(-b * (x + c))),
           start = sig_start_values,
           control = nls.control(maxiter = 1000, minFactor = 0.0001))
summary(sig_fit)

ggplot() +
  geom_line(data = hold_df, aes(x, predict(sig_fit, newdata = data.frame(x)))) +
  geom_point(data = dendro_wp |> 
               mutate(condition = case_when(condition == "drought" ~ "Drought",
                                            condition == "predrought" ~ "Predrought",
                                            condition == "recovery" ~ "Recovery")), 
             aes(x = twd_min, y = PD_m, color = condition, shape = Tree_ID), 
             size = 4) +
  ggtitle("Sigmoidal Regression") +
  scale_color_manual(values = c("springgreen4", "lightsalmon3", "yellowgreen")) +
  labs(x = expression(paste(TWD[min])), y = expression(paste(Psi[PD])), 
       color = "Condition") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        title = element_text(size = 13))

# Quadratic regression
quad_fit <- lm(y ~ poly(x, 2), data = hold_df)
summary(quad_fit)

ggplot(hold_df, aes(x, y)) +
  geom_line(data = hold_df, aes(x, predict(quad_fit, newdata = data.frame(x)))) +
  geom_point(data = dendro_wp |> 
               mutate(condition = case_when(condition == "drought" ~ "Drought",
                                            condition == "predrought" ~ "Predrought",
                                            condition == "recovery" ~ "Recovery")), 
             aes(x = twd_min, y = PD_m, color = condition, shape = Tree_ID), 
             size = 4) +
  scale_color_manual(values = c("springgreen4", "lightsalmon3", "yellowgreen")) +
  ggtitle("Quadratic Regression") +
  labs(x = expression(paste(TWD[min])), y = expression(paste(Psi[PD])), 
       color = "Condition") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        title = element_text(size = 13))

# Cubic regression
cub_fit <- lm(y ~ poly(x, 3), data = hold_df)
summary(cub_fit)

ggplot() +
  geom_line(data = hold_df, aes(x, predict(cub_fit, newdata = data.frame(x)))) +
  geom_point(data = dendro_wp |> 
               mutate(condition = case_when(condition == "drought" ~ "Drought",
                                            condition == "predrought" ~ "Predrought",
                                            condition == "recovery" ~ "Recovery")), 
             aes(x = twd_min, y = PD_m, color = condition, shape = Tree_ID), 
             size = 4) +
  scale_color_manual(values = c("springgreen4", "lightsalmon3", "yellowgreen")) +
  ggtitle("Cubic Regression") +
  labs(x = expression(paste(TWD[min])), y = expression(paste(Psi[PD])), 
       color = "Condition") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        title = element_text(size = 13))

# x^4 regression
x4_fit <- lm(y ~ poly(x, 2), data = hold_df)
summary(x4_fit)

ggplot() +
  geom_line(data = hold_df, aes(x, predict(x4_fit, newdata = data.frame(x)))) +
  geom_point(data = dendro_wp |> 
               mutate(condition = case_when(condition == "drought" ~ "Drought",
                                            condition == "predrought" ~ "Predrought",
                                            condition == "recovery" ~ "Recovery")), 
             aes(x = twd_min, y = PD_m, color = condition, shape = Tree_ID), 
             size = 4) +
  scale_color_manual(values = c("springgreen4", "lightsalmon3", "yellowgreen")) +
  ggtitle("x^4 Regression") +
  labs(x = expression(paste(TWD[min])), y = expression(paste(Psi[PD])), 
       color = "Condition") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        title = element_text(size = 13))
