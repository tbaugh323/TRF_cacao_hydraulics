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

