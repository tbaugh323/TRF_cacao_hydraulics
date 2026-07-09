#### why am I getting negative gs? ####
library(tidyverse)

humidity <- sum_small_data |> 
  group_by(dt, Date, period) |> 
  summarize(rh_m = mean(rh_s_m),
            rh_sd = sd(rh_s_m)) |> 
  ungroup()

smaller_measurements |> 
  ggplot(aes(x = dt)) +
  geom_point(aes(y = gsw, color = factor(individual), shape = canopy), size = 2) +
  geom_point(aes(y = rh_s / 100), size = 2, alpha = 0.5, color = "navyblue") +
  # geom_line(aes(y = rh_s / 100)) +
  # geom_errorbar(data = humidity, aes(x = dt, ymin = (rh_m - rh_sd)/100, ymax = (rh_m + rh_sd)/100), width = 1500) +
  # geom_line(data = humidity, aes(x = dt, y = rh_m / 100)) +
  scale_y_continuous(expression(paste(g[s], " (mol ", m^-2, s^-1, ")")),
                     sec.axis = sec_axis(~ . * 100, "RH (%)"))

smaller_measurements |> 
  ggplot(aes(x = dt)) +
  geom_point(aes(y = gsw, color = factor(individual), shape = canopy), size = 2) +
  geom_point(aes(y = Tleaf/ 17), color = "salmon", size = 2, alpha = 0.5) +
  scale_y_continuous(expression(paste(g[s], " (mol ", m^-2, s^-1, ")")),
                     sec.axis = sec_axis(~ . * 17, expression(paste(T[leaf], " (", degree, "C)"))))


third <- smaller_measurements |> 
  filter(individual == "2" | individual == "3" | individual == "4") |> 
  group_by(Date, period) |> 
  summarize(temp = mean(Tmeas),
            rh = mean(rh_s)) |> 
  ungroup()

second <- smaller_measurements |> 
  filter(individual == "1") |> 
  group_by(Date, period) |> 
  summarize(temp = mean(Tmeas),
            rh = mean(rh_s)) |> 
  ungroup()

# It's really humid



#### Why is the 1670 broken? ####

working_test <- read_csv("data/working_test.csv")
working_test <- working_test[!grepl("lciSerialNumber", working_test$configName),]
working_test <- working_test[!grepl("configName", working_test$configName),]
working_test <- working_test |> 
  mutate(`Date` = as.Date(`Date`),
         Time = hms::as_hms(Time)) |>
  mutate(dt = as.POSIXct(paste(Date, Time))) |> 
  mutate(across(c(Observation:leaf_width, Fo:batt, rh_adj:Ble, flash_intensity:z_flr), as.numeric)) |> 
  relocate(dt) |> 
  filter(configAuthor == "LI-COR Default")
working_test <- working_test[-1,]

broken_test <- read_csv("data/broken_test.csv")
broken_test <- broken_test[,-(109:378)]
broken_test <- broken_test[!grepl("lciSerialNumber", broken_test$configName),]
broken_test <- broken_test[!grepl("configName", broken_test$configName),]
broken_test <- broken_test |> 
  mutate(`Date` = as.Date("2026-07-07")) |> 
  mutate(`Date` = as.Date(`Date`),
         Time = hms::as_hms(Time)) |>
  mutate(dt = as.POSIXct(paste(Date, Time))) |> 
  mutate(across(c(Observation:leaf_width, Fo:batt, rh_adj:Ble, flash_intensity:z_flr), as.numeric)) |> 
  relocate(dt) |> 
  filter(configAuthor == "LI-COR Default") |> 
  mutate(`Obs#` = row_number(), `Observation` = row_number())

ggplot() +
  geom_point(data = working_test, aes(x = dt, y = gsw, color = "working"), size = 3) +
  geom_point(data = broken_test, aes(x = dt, y = gsw, color = "broken"), size = 3)

ggplot() +
  geom_point(data = working_test, aes(x = dt, y = `Fm'`, color = "working"), size = 3) +
  geom_point(data = broken_test, aes(x = dt, y = `Fm'`, color = "broken"), size = 3)

ggplot() +
  geom_point(data = working_test, aes(x = dt, y = rh_s, color = "working"), size = 3) +
  geom_point(data = broken_test, aes(x = dt, y = rh_s, color = "broken"), size = 3)

ggplot() +
  geom_point(data = working_test, aes(x = dt, y = flow_s, color = "working"), size = 3) +
  geom_point(data = broken_test, aes(x = dt, y = flow_s, color = "broken"), size = 3)


broken_test2 <- read_csv("data/broken_test2.csv")
broken_test2 <- broken_test2[!grepl("lciSerialNumber", broken_test2$configName),]
broken_test2 <- broken_test2[!grepl("configName", broken_test2$configName),]
broken_test2 <- broken_test2 |> 
  mutate(`Date` = as.Date(`Date`),
         Time = hms::as_hms(Time)) |>
  mutate(dt = as.POSIXct(paste(Date, Time))) |> 
  mutate(across(c(Observation:leaf_width, Fo:batt, rh_adj:Ble, flash_intensity:z_flr), as.numeric)) |> 
  relocate(dt) |> 
  filter(configAuthor == "LI-COR Default")

broken_test2 |> 
  ggplot(aes(x = dt)) +
  # geom_point(aes(y = gsw), color = "forestgreen")
  # geom_point(aes(y = VPleaf), color = "navy")
  # geom_point(aes(y = `Fm'`), color = "red") +
  geom_point(aes(y = flow), color = "chocolate1")

