library(tidyverse)
theme_set(theme_bw())

R1171 <- read_csv("data/sap_flow/raw_sapflow/R1171-all-postprocess.csv") |> 
  mutate(Datetime = as.POSIXct(Datetime, tz = "MST"))

R1171 |> 
  filter(Date > as.Date("2026-05-01") & Date < as.Date("2026-05-13")) |> 
  ggplot(aes(x = Datetime, y = VhrmHRM5)) +
  geom_point(size = 0.3) +
  facet_wrap(~ location)

# Fitting a sinusoidal curve to all days
coef_df <- data.frame(location = NA, a = NA, f = NA, phi = NA, d = NA)
integral_df <- data.frame(location = NA, value = NA, abs.error = NA)
prediction_df <- data.frame(blah = rep(NA, 528))
for(i in 1:length(unique(R1171$location))) {
  # Create data
  x <- R1171 |> 
    filter(Date > as.Date("2026-05-01") & Date < as.Date("2026-05-13"),
           location == unique(R1171$location)[i], !is.na(VhrmHRM5)) |> 
    mutate(not_date = row_number()) |> 
    dplyr::select(not_date)
  
  x <- as.numeric(unlist(x))
  
  y <- R1171 |> 
    filter(Date > as.Date("2026-05-01") & Date < as.Date("2026-05-13"),
           location == unique(R1171$location)[i], !is.na(VhrmHRM5)) |> 
    dplyr::select(VhrmHRM5)
  
  y <- as.numeric(unlist(y))
  
  # Create models
  model <- y ~ a * sin(2 * pi * f * x + phi) + d
  
  inits <- list(a = 2.5, f = 0.02, phi = 1.5 * pi, d = -7.5)
  
  fit <- nls(model, data = data.frame(x = x, y = y), start = inits)
  
  # Calculate stuff
  prediction <- predict(fit, data.frame(x, y))
  
  branch_func <- function(x) {
    coef(fit)[1] * sin(2 * pi * coef(fit)[2] * x + coef(fit)[3] - coef(fit)[4])
  }
  
  branch_integral <- integrate(branch_func, lower = 0, upper = 500)
  
  # Fix to save it out better
  if(length(prediction) == 527) {
    prediction <- c(prediction, NA)
  }
  if(length(prediction) == 517) {
    prediction <- c(prediction, rep(NA, 11))
  }
  
  # Save
  prediction_df[[ncol(prediction_df) + 1]] <- prediction
  coef_df <- add_row(coef_df, location = unique(R1171$location)[i], a = coef(fit)[1], f = coef(fit)[2], phi = coef(fit)[3], d = coef(fit)[4])
  integral_df <- add_row(integral_df, location = unique(R1171$location)[i], value = branch_integral$value, abs.error = branch_integral$abs.error)
}
# Clean
prediction_df <- prediction_df |> 
  dplyr::select(-blah) |>
  rename("T" = V2,
         "SB" = V3,
         "NB" = V4,
         "MB" = V5,
         "M" = V6,
         "LB" = V7,
         "L" = V8,
         "EB" = V9)
coef_df <- coef_df |> 
  filter(!is.na(location))
integral_df <- integral_df |> 
  filter(!is.na(location))

R1171_dates <- R1171 |> 
  filter(Date > as.Date("2026-05-01") & Date < as.Date("2026-05-13"),
         location == "T", !is.na(VhrmHRM5)) |> 
  dplyr::select(Datetime)

more <- prediction_df |> 
  mutate(sum = (`T` + `SB` + `NB` + `MB` + `M` + `LB` + `L` + `EB`)) |> 
  mutate(dt = R1171_dates$Datetime)

# getting all the points
y_T <- R1171 |> 
  filter(Date > as.Date("2026-05-01") & Date < as.Date("2026-05-13"),
         location == unique(R1171$location)[1], !is.na(VhrmHRM5)) |> 
  dplyr::select(VhrmHRM5)
y_T <- as.numeric(unlist(y_T))
y_SB <- R1171 |> 
  filter(Date > as.Date("2026-05-01") & Date < as.Date("2026-05-13"),
         location == unique(R1171$location)[2], !is.na(VhrmHRM5)) |> 
  dplyr::select(VhrmHRM5)
y_SB <- as.numeric(unlist(y_SB))
y_NB <- R1171 |> 
  filter(Date > as.Date("2026-05-01") & Date < as.Date("2026-05-13"),
         location == unique(R1171$location)[3], !is.na(VhrmHRM5)) |> 
  dplyr::select(VhrmHRM5)
y_NB <- as.numeric(unlist(y_NB))
y_MB <- R1171 |> 
  filter(Date > as.Date("2026-05-01") & Date < as.Date("2026-05-13"),
         location == unique(R1171$location)[4], !is.na(VhrmHRM5)) |> 
  dplyr::select(VhrmHRM5)
y_MB <- as.numeric(unlist(y_MB))
y_M <- R1171 |> 
  filter(Date > as.Date("2026-05-01") & Date < as.Date("2026-05-13"),
         location == unique(R1171$location)[5], !is.na(VhrmHRM5)) |> 
  dplyr::select(VhrmHRM5)
y_M <- as.numeric(unlist(y_M))
y_LB <- R1171 |> 
  filter(Date > as.Date("2026-05-01") & Date < as.Date("2026-05-13"),
         location == unique(R1171$location)[6], !is.na(VhrmHRM5)) |> 
  dplyr::select(VhrmHRM5)
y_LB <- as.numeric(unlist(y_LB))
y_L <- R1171 |> 
  filter(Date > as.Date("2026-05-01") & Date < as.Date("2026-05-13"),
         location == unique(R1171$location)[7], !is.na(VhrmHRM5)) |> 
  dplyr::select(VhrmHRM5)
y_L <- as.numeric(unlist(y_L))
y_EB <- R1171 |> 
  filter(Date > as.Date("2026-05-01") & Date < as.Date("2026-05-13"),
         location == unique(R1171$location)[8], !is.na(VhrmHRM5)) |> 
  dplyr::select(VhrmHRM5)
y_EB <- as.numeric(unlist(y_EB))

# all curves on top of each other (yikes)
ggplot() +
  geom_line(aes(x = more$dt, y = prediction_df$`T`), color = "darkgoldenrod1") +
  geom_line(aes(x = more$dt, y = prediction_df$`SB`), color = "royalblue2") +
  geom_line(aes(x = more$dt, y = prediction_df$`NB`), color = "springgreen3") +
  geom_line(aes(x = more$dt, y = prediction_df$`MB`), color = "palevioletred1") +
  geom_line(aes(x = more$dt, y = prediction_df$`M`), color = "chocolate1") +
  geom_line(aes(x = more$dt, y = prediction_df$`LB`), color = "plum") +
  geom_line(aes(x = more$dt, y = prediction_df$`L`), color = "tomato") +
  geom_line(aes(x = more$dt, y = prediction_df$`EB`), color = "skyblue2") +
  labs(x = "Date", y = expression(paste(V["s,5"], "(cm/hr), estimated"))) +
  theme(panel.grid = element_blank())

# just 1 day
ggplot() +
  annotate("text", x = as.POSIXct("2026-05-02 02:00:00"), y = 15, 
           label = "Top", color = "darkgoldenrod1") +
  annotate("text", x = as.POSIXct("2026-05-02 02:00:00"), y = 13, 
           label = "South branch", color = "royalblue2") +
  annotate("text", x = as.POSIXct("2026-05-02 02:00:00"), y = 11, 
           label = "North branch", color = "springgreen3") +
  annotate("text", x = as.POSIXct("2026-05-02 02:00:00"), y = 9, 
           label = "Main branch", color = "palevioletred1") +
  annotate("text", x = as.POSIXct("2026-05-02 7:00:00"), y = 15, 
           label = "Main", color = "chocolate1") +
  annotate("text", x = as.POSIXct("2026-05-02 7:00:00"), y = 13, 
           label = "Lower branch", color = "plum") +
  annotate("text", x = as.POSIXct("2026-05-02 7:00:00"), y = 11, 
           label = "Lower", color = "tomato") +
  annotate("text", x = as.POSIXct("2026-05-02 7:00:00"), y = 9, 
           label = "East branch", color = "skyblue2") +
  geom_line(aes(x = more$dt[1:50], y = prediction_df$`T`[1:50]), color = "darkgoldenrod1") +
  geom_line(aes(x = more$dt[1:50], y = prediction_df$`SB`[1:50]), color = "royalblue2") +
  geom_line(aes(x = more$dt[1:50], y = prediction_df$`NB`[1:50]), color = "springgreen3") +
  geom_line(aes(x = more$dt[1:50], y = prediction_df$`MB`[1:50]), color = "palevioletred1") +
  geom_line(aes(x = more$dt[1:50], y = prediction_df$`M`[1:50]), color = "chocolate1") +
  geom_line(aes(x = more$dt[1:50], y = prediction_df$`LB`[1:50]), color = "plum") +
  geom_line(aes(x = more$dt[1:50], y = prediction_df$`L`[1:50]), color = "tomato") +
  geom_line(aes(x = more$dt[1:50], y = prediction_df$`EB`[1:50]), color = "skyblue2") +
  labs(x = "Datetime", y = expression(paste(V["s,5"], "(cm/hr), estimated"))) +
  theme(panel.grid = element_blank())

# just 1 day with points
ggplot() +
  annotate("text", x = as.POSIXct("2026-05-02 02:00:00"), y = 15, 
           label = "Top", color = "darkgoldenrod1") +
  annotate("text", x = as.POSIXct("2026-05-02 02:00:00"), y = 13, 
           label = "South branch", color = "royalblue2") +
  annotate("text", x = as.POSIXct("2026-05-02 02:00:00"), y = 11, 
           label = "North branch", color = "springgreen3") +
  annotate("text", x = as.POSIXct("2026-05-02 02:00:00"), y = 9, 
           label = "Main branch", color = "palevioletred1") +
  annotate("text", x = as.POSIXct("2026-05-02 7:00:00"), y = 15, 
           label = "Main", color = "chocolate1") +
  annotate("text", x = as.POSIXct("2026-05-02 7:00:00"), y = 13, 
           label = "Lower branch", color = "plum") +
  annotate("text", x = as.POSIXct("2026-05-02 7:00:00"), y = 11, 
           label = "Lower", color = "tomato") +
  annotate("text", x = as.POSIXct("2026-05-02 7:00:00"), y = 9, 
           label = "East branch", color = "skyblue2") +
  geom_line(aes(x = more$dt[1:50], y = prediction_df$`T`[1:50]), linewidth = 1, color = "darkgoldenrod1") +
  geom_line(aes(x = more$dt[1:50], y = y_T[1:50]), alpha = 0.4, color = "darkgoldenrod1") +
  geom_line(aes(x = more$dt[1:50], y = prediction_df$`SB`[1:50]), linewidth = 1, color = "royalblue2") +
  geom_line(aes(x = more$dt[1:50], y = y_SB[1:50]), alpha = 0.4, color = "royalblue2") +
  geom_line(aes(x = more$dt[1:50], y = prediction_df$`NB`[1:50]), linewidth = 1, color = "springgreen3") +
  geom_line(aes(x = more$dt[1:50], y = y_NB[1:50]), alpha = 0.4, color = "springgreen3") +
  geom_line(aes(x = more$dt[1:50], y = prediction_df$`MB`[1:50]), linewidth = 1, color = "palevioletred1") +
  geom_line(aes(x = more$dt[1:50], y = y_MB[1:50]), alpha = 0.4, color = "palevioletred1") +
  geom_line(aes(x = more$dt[1:50], y = prediction_df$`M`[1:50]), linewidth = 1, color = "chocolate1") +
  geom_line(aes(x = more$dt[1:50], y = y_M[1:50]), alpha = 0.4, color = "chocolate1") +
  geom_line(aes(x = more$dt[1:50], y = prediction_df$`LB`[1:50]), linewidth = 1, color = "plum") +
  geom_line(aes(x = more$dt[1:50], y = y_LB[1:50]), alpha = 0.4, color = "plum") +
  geom_line(aes(x = more$dt[1:50], y = prediction_df$`L`[1:50]), linewidth = 1, color = "tomato") +
  geom_line(aes(x = more$dt[1:50], y = y_L[1:50]), alpha = 0.4, color = "tomato") +
  geom_line(aes(x = more$dt[1:50], y = prediction_df$`EB`[1:50]), linewidth = 1, color = "skyblue2") +
  geom_line(aes(x = more$dt[1:50], y = y_EB[1:50]), alpha = 0.4, color = "skyblue2") +
  labs(x = "Datetime", y = expression(paste(V["s,5"], "(cm/hr)"))) +
  theme(panel.grid = element_blank())

# 1 day, with labels and sum
ggplot() +
  # T
  geom_line(aes(x = more$dt[1:50], y = prediction_df$`T`[1:50]), color = "darkgoldenrod1") +
  annotate("text", x = as.POSIXct("2026-05-02 4:00:00"), y = 23, 
           label = paste0("T (total): ", round(sum(more$`T`[1:50], na.rm = TRUE), 2)), color = "darkgoldenrod1") +
  # SB
  geom_line(aes(x = more$dt[1:50], y = prediction_df$`SB`[1:50]), color = "royalblue2") +
  annotate("text", x = as.POSIXct("2026-05-02 4:00:00"), y = 21, 
           label = paste0("SB (total): ", round(sum(more$`SB`[1:50], na.rm = TRUE), 2)), color = "royalblue2") +
  # NB
  geom_line(aes(x = more$dt[1:50], y = prediction_df$`NB`[1:50]), color = "springgreen3") +
  annotate("text", x = as.POSIXct("2026-05-02 4:00:00"), y = 19, 
           label = paste0("NB (total): ", round(sum(more$`NB`[1:50], na.rm = TRUE), 2)), color = "springgreen3") +
  # MB
  geom_line(aes(x = more$dt[1:50], y = prediction_df$`MB`[1:50]), color = "palevioletred1") +
  annotate("text", x = as.POSIXct("2026-05-02 4:00:00"), y = 17, 
           label = paste0("MB (total): ", round(sum(more$`MB`[1:50], na.rm = TRUE), 2)), color = "palevioletred1") +
  # M 
  geom_line(aes(x = more$dt[1:50], y = prediction_df$`M`[1:50]), color = "chocolate1") +
  annotate("text", x = as.POSIXct("2026-05-02 4:00:00"), y = 15, 
           label = paste0("M (total): ", round(sum(more$`M`[1:50], na.rm = TRUE), 2)), color = "chocolate1") +
  # LB
  geom_line(aes(x = more$dt[1:50], y = prediction_df$`LB`[1:50]), color = "plum") +
  annotate("text", x = as.POSIXct("2026-05-02 4:00:00"), y = 13, 
           label = paste0("LB (total): ", round(sum(more$`LB`[1:50], na.rm = TRUE), 2)), color = "plum") +
  # L
  geom_line(aes(x = more$dt[1:50], y = prediction_df$`L`[1:50]), color = "tomato") +
  annotate("text", x = as.POSIXct("2026-05-02 4:00:00"), y = 11, 
           label = paste0("L (total): ", round(sum(more$`L`[1:50], na.rm = TRUE), 2)), color = "tomato") +
  # EB
  geom_line(aes(x = more$dt[1:50], y = prediction_df$`EB`[1:50]), color = "skyblue2") +
  annotate("text", x = as.POSIXct("2026-05-02 4:00:00"), y = 9, 
           label = paste0("EB (total): ", round(sum(more$`EB`[1:50], na.rm = TRUE), 2)), color = "skyblue2") +
  # Sum
  geom_line(aes(x = more$dt[1:50], y = more$sum[1:50])) +
  annotate("text", x = as.POSIXct("2026-05-02 4:00:00"), y = 25, 
           label = paste0("Sum of curves (total): ", round(sum(more$sum[1:50], na.rm = TRUE), 2)), color = "black") +
  labs(x = "Datetime", y = expression(paste(V["s,5"], "(cm/hr), estimated"))) +
  theme(panel.grid = element_blank())


R1171_raw_int <- R1171 |> 
  group_by(Datetime) |> 
  summarize(sum = sum(VhrmHRM5))

R1171_compare <- R1171_raw_int |> 
  rename(dt = Datetime, raw_sum = sum) |> 
  inner_join(more |> dplyr::select(sum, dt), by = "dt") |> 
  filter(dt > as.POSIXct("2026-05-01 00:00:00") & dt < as.POSIXct("2026-05-13 00:00:00"))

R1171_compare |> 
  ggplot() +
  annotate("text", x = as.POSIXct("2026-05-04 00:00:00"), y = 52,
           label = "Raw sum of all sap flow sensors", color = "forestgreen") +
  annotate("text", x = as.POSIXct("2026-05-04 00:00:00"), y = 47,
           label = "Estimated sum of all dendrometers", color = "skyblue3") +
  geom_line(aes(x = dt, y = raw_sum), linewidth = 0.7, color = "forestgreen") +
  geom_line(aes(x = dt, y = sum), linewidth = 0.7, color = "skyblue3") +
  labs(x = "Datetime", y = "Sum (cm)") +
  theme(panel.grid = element_blank())

line <- lm(sum ~ raw_sum, data = R1171_compare)

R1171_compare |> 
  mutate(time = lubridate::hour(dt)) |> 
  ggplot(aes(x = raw_sum, y = sum)) +
  annotate("text", x = -15, y = 55,
           label = expression(paste(r^2, ": 0.7477"))) +
  annotate("text", x = -15, y = 47,
           label = paste0("Slope: ", round(line$coefficients[2], 2))) +
  annotate("text", x = -15, y = 39,
           label = paste0("Intercept: ", round(line$coefficients[1], 2))) +
  geom_abline(aes(slope = 1, intercept = 0), linetype = "dashed", color = "gray20") +
  geom_abline(aes(slope = line$coefficients[2], intercept = line$coefficients[1])) +
  geom_point(aes(color = time)) +
  scale_color_gradient(low = "navy", high = "orange") +
  xlim(-25, 60) +
  ylim(-25, 60) +
  labs(x = "Raw sum of all sap flow sensors",
       y = "Estimated sum of all sap flow sensors",
       color = "Hour of day") +
  theme(panel.grid = element_blank())


# Fitting a Gaussian

gaus_params <- data.frame(location = NA, m = NA, sd = NA, k = NA)
for(i in 1:length(unique(R1171$location))) {
  x <- R1171 |> 
    mutate(time = as.numeric(factor(hms::as_hms(Datetime)))) |> 
    filter(location == unique(R1171$location)[i], !is.na(VhrmHRM5)) |> 
    dplyr::select(time)
  
  x <- as.numeric(unlist(x))
  
  y <- R1171 |> 
    filter(location == unique(R1171$location)[i], !is.na(VhrmHRM5)) |> 
    dplyr::select(VhrmHRM5)
  
  y <- as.numeric(unlist(y))
  
  gaus_fit <- function(par) {
    m <- par[1]
    sd <- par[2]
    k <- par[3]
    yhat <- k * exp(-0.5 * ((x - m)/sd)^2)
    sum((y - yhat)^2)
  }
  
  if (i == 8) {
    gaus_result <- optim(c(125, 34, 12), gaus_fit)
  } else {
    gaus_result <- optim(c(65, 40, 15), gaus_fit)
  }

  gaus_params <- add_row(gaus_params, location = unique(R1171$location)[i], m = gaus_result$par[1], sd = gaus_result$par[2], k = gaus_result$par[3])
}

gaus_params <- gaus_params |> 
  filter(!is.na(location))

gaus_function <- function(x, par) {
  m <- par[1]
  sd <- par[2]
  k <- par[3]
  k * exp(-0.5 * ((x - m)/sd)^2)
}

gaus_R1171 <- R1171 |> 
  mutate(time = as.numeric(factor(hms::as_hms(Datetime)))) |> 
  mutate(time_2 = factor(hms::as_hms(round_date(Datetime, unit = "30 minutes")))) |> 
  merge(gaus_params, by = "location") |> 
  filter(!is.na(VhrmHRM5))

ggplot() +
  geom_jitter(data = gaus_R1171 |> filter(location == "M", VhrmHRM5 < 27), aes(x = time, y = VhrmHRM5), size = 0.3,, width = 1, color = "chocolate1") +
  geom_jitter(data = gaus_R1171 |> filter(location == "NB", VhrmHRM5 < 27), aes(x = time, y = VhrmHRM5), size = 0.3,, width = 1, color = "springgreen3") +
  geom_jitter(data = gaus_R1171 |> filter(location == "L"), aes(x = time, y = VhrmHRM5), size = 0.3, width = 1, color = "tomato") +
  geom_jitter(data = gaus_R1171 |> filter(location == "LB"), aes(x = time, y = VhrmHRM5), size = 0.3, width = 1, color = "plum") +
  geom_jitter(data = gaus_R1171 |> filter(location == "MB"), aes(x = time, y = VhrmHRM5), size = 0.3, width = 1, color = "palevioletred1") +
  geom_jitter(data = gaus_R1171 |> filter(location == "SB"), aes(x = time, y = VhrmHRM5), size = 0.3, width = 1, color = "royalblue1") +
  geom_jitter(data = gaus_R1171 |> filter(location == "T"), aes(x = time, y = VhrmHRM5), size = 0.3,, width = 1, color = "darkgoldenrod1") +
  geom_function(fun = gaus_function, args = list(par = c(gaus_params$m[5], gaus_params$sd[5], gaus_params$k[5])), linewidth = 1, color = "chocolate1") +
  geom_function(fun = gaus_function, args = list(par = c(gaus_params$m[3], gaus_params$sd[3], gaus_params$k[3])), linewidth = 1, color = "springgreen3") +
  geom_function(fun = gaus_function, args = list(par = c(gaus_params$m[1], gaus_params$sd[1], gaus_params$k[1])), linewidth = 1, color = "darkgoldenrod1") +
  geom_function(fun = gaus_function, args = list(par = c(gaus_params$m[2], gaus_params$sd[2], gaus_params$k[2])), linewidth = 1, color = "royalblue1") +
  geom_function(fun = gaus_function, args = list(par = c(gaus_params$m[6], gaus_params$sd[6], gaus_params$k[6])), linewidth = 1, color = "plum") +
  geom_function(fun = gaus_function, args = list(par = c(gaus_params$m[4], gaus_params$sd[4], gaus_params$k[4])), linewidth = 1, color = "palevioletred1") +
  geom_function(fun = gaus_function, args = list(par = c(gaus_params$m[7], gaus_params$sd[7], gaus_params$k[7])), linewidth = 1, color = "tomato")

gaus_params |> 
  ggplot(aes(x = location, y = m)) +
  geom_rect(aes(ymin = m - sd, ymax = m + sd, width = 0.2, fill = location), alpha = 0.5) +
  geom_point(aes(color = location), size = 5) +
  scale_color_manual(values = c("tomato", "plum", "chocolate1", "palevioletred1", "springgreen3", "royalblue1", "darkgoldenrod1")) +
  scale_fill_manual(values = c("tomato", "plum", "chocolate1", "palevioletred1", "springgreen3", "royalblue1", "darkgoldenrod1")) +
  labs(x = "Branch location", y = "Gaussian mean", color = "Location", fill = "Location") +
  theme(panel.grid = element_blank())




