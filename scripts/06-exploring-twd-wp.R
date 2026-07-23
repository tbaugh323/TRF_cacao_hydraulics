#### Looking at TWD and WP data together ####
library(tidyverse)
library(segmented)

#### Prep data ####
wp_PDMD <- read_csv("data/water_potential/wp_PDMD.csv") |> 
  mutate(Tree_ID = case_when(individual == 1 ~ "BioR1192",
                             individual == 2 ~ "BioR1171",
                             individual == 3 ~ "BioR1170",
                             individual == 4 ~ "BioR1191"))
all_dendros <- read_csv("data/dendro_data/all_dendros.csv")

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
    add_row(Tree_ID = unique(dendro_wp$Tree_ID)[i], slope = current_lm$coefficients[2], intercept = current_lm$coefficients[1], r2 = summary(current_lm)$r.squared)
}
dendro_wp_lms <- dendro_wp_lms |> filter(!is.na(Tree_ID)) |> 
  rename(lambda_twd = slope)

dendro_wp |> 
  ggplot(aes(x = PD_m, y = twd_min)) +
  # geom_errorbar(aes(xmin = MD_m - MD_sd, xmax = MD_m + MD_sd)) +
  geom_abline(data = dendro_wp_lms, aes(slope = lambda_twd, intercept = intercept)) +
  geom_point(aes(color = condition), size = 4) + # aes(color = date)
  # scale_color_viridis_c(option = "turbo", trans = "date") +
  facet_wrap(~ Tree_ID) +
  labs(x = expression(paste(Psi[PD])), y = expression(paste(TWD[min])),
       color = "Condition") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))

# not excellent fits, but these will be |λTWD[min]|
dendro_wp_abs <- dendro_wp_lms |> 
  mutate(lambda_twd = abs(lambda_twd))
write_csv(dendro_wp_abs, "data/dendro_data/lambda_twd_min.csv")

#### doing Ziegler stuff ####

dendro_wp |> 
  ggplot(aes(x = PD_m, y = twd_min)) +
  geom_point(aes(color = condition, shape = Tree_ID), size = 4) +
  xlim(c(0, -2.5)) +
  facet_wrap(~ Tree_ID)

dendro_wp |> 
  ggplot(aes(x = twd_min, y = PD_m)) +
  geom_point(aes(color = condition, shape = Tree_ID), size = 4) +
  labs(x = expression(paste(TWD[PD])), y = expression(paste(Psi[PD])), color = "Condition") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))
# We have a two phase relationship here, not three :O

#### Stats for Ziegler method ####

# Lots of repeated values from having the subhourly time series, get rid of that
cp_hold <- dendro_wp |> 
  dplyr::select(date, Tree_ID, canopy, twd_min, PD_m) |> 
  distinct()

result <- lm(PD_m ~ twd_min, data = cp_hold)

seg_result <- segmented(result, psi = 0.7)

davies.test(result) # there is a segment!

summary(seg_result)

# Estimated break point
# twd_min = 3.404
# R-squared = 0.8604  # pretty good!
#
# confint(seg_result)
# (3.32982, 3.47854)
#
# slope(seg_result)
# slope1: 0.016797 
# slope2: -8.178300 
#
# intercept(seg_result)
# intercept1: -0.39988
# intercept2: 27.49800

# twd_pd
changepoint <- 3.404 
slope1 <- 0.016797
slope2 <- -8.178300 
intercept1 <- -0.39988
intercept2 <- 27.49800
# twd_min
changepoint <- 0.556385
slope1 <- -0.11498
slope2 <- -6.27190
intercept1 <- -0.40993
intercept2 <- 3.01570

dendro_wp |> 
  ggplot(aes(x = twd_min, y = PD_m)) + 
  annotate(geom = "segment", x = 0, y = intercept1, xend = changepoint, yend = (slope1 * changepoint) + intercept1) +
  annotate(geom = "segment", x = changepoint, y = (slope1 * changepoint) + intercept1, xend = 0.8, yend = 0.8 * slope2 + intercept2) +
  # geom_segment(aes(x = 0, y = intercept1, xend = changepoint, yend = (slope1 * changepoint) + intercept1)) +
  # geom_segment(aes(x = changepoint, y = (slope1 * changepoint) + intercept1, xend = 4, yend = 4 * slope2 + intercept2)) +
  geom_point(aes(color = condition, shape = Tree_ID), size = 4) +
  labs(x = expression(paste(TWD[PD])), y = expression(paste(Psi[PD])), color = "Condition") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))

dendro_wp |> 
  ggplot(aes(x = PD_m, y = twd_min)) + 
  annotate(geom = "segment", y = 0, x = intercept1, yend = changepoint, xend = (slope1 * changepoint) + intercept1) +
  annotate(geom = "segment", y = changepoint, x = (slope1 * changepoint) + intercept1, yend = 0.8, xend = 0.8 * slope2 + intercept2) +
  # geom_segment(aes(x = 0, y = intercept1, xend = changepoint, yend = (slope1 * changepoint) + intercept1)) +
  # geom_segment(aes(x = changepoint, y = (slope1 * changepoint) + intercept1, xend = 4, yend = 4 * slope2 + intercept2)) +
  geom_point(aes(color = condition, shape = Tree_ID), size = 4) +
  labs(y = expression(paste(TWD[PD])), x = expression(paste(Psi[PD])), color = "Condition") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))



library(minpack.lm)

# Exponential regression
x <- dendro_wp$twd_min
y <- dendro_wp$PD_m
start_values <- c(a = 1, b = 2)
fit <- nls(y ~ a * exp(b * x),
           start = start_values,
           algorithm = "port",
           control = nls.control(maxiter = 1000))
summary(fit)

ggplot(data.frame(x, y), aes(x, y)) +
  geom_point() +
  geom_line(aes(x, predict(fit, newdata = data.frame(x)))) +
  ggtitle("Exponential Regression") +
  xlab("x") +
  ylab("y")

# Sigmoidal regression doesn't look super good :(
start_values <- c(A = -2, b = 5, c = -0.4)
fit <- nlsLM(y ~ A / (1 + exp(-b * (x + c))),
           start = start_values,
           control = nls.control(maxiter = 1000, minFactor = 0.0001))
summary(fit)

ggplot(data.frame(x, y), aes(x, y)) +
  geom_point() +
  geom_line(aes(x, predict(fit, newdata = data.frame(x)))) +
  ggtitle("Sigmoidal Regression") +
  xlab("x") +
  ylab("y")

# Quadratic regression
df <- data.frame(x, y)
fit <- lm(y ~ poly(x, 2), data = df)
summary(fit)

ggplot(df, aes(x, y)) +
  geom_point() +
  geom_line(aes(x, predict(fit, newdata = data.frame(x)))) +
  ggtitle("Quadratic Regression")

# Cubic regression
fit <- lm(y ~ poly(x, 3), data = df)
summary(fit)

ggplot(df, aes(x, y)) +
  geom_point() +
  geom_line(aes(x, predict(fit, newdata = data.frame(x)))) +
  ggtitle("Cubic Regression")

# x^4 regression
fit <- lm(y ~ poly(x, 2), data = df)
summary(fit)

ggplot(df, aes(x, y)) +
  geom_point() +
  geom_line(aes(x, predict(fit, newdata = data.frame(x)))) +
  ggtitle("x^4 Regression")
