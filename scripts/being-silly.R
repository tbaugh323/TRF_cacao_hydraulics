library(tidyverse)

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
  mutate(dt = as.POSIXct(paste(date, time)),
         time = hms::as_hms(time)) |> 
  relocate(dt)

wp_silly <- wp |> 
  group_by(date, period) |> 
  summarize(first_time = first(time),
            last_time = last(time)) |> 
  ungroup() |> 
  mutate(time_dif = last_time - first_time,
         min_time_dif = as.numeric(time_dif) / 60)

wp_silly |> 
  ggplot(aes(x = date, y = min_time_dif)) +
  geom_line(aes(group = period, color = period)) +
  geom_point(aes(shape = period, color = period), size = 4) +
  scale_color_manual(values = c("orchid3", "chocolate1")) +
  labs(x = "Date", y = "Time to complete measurements (minutes)", shape = "Period", color = "Period") +
  theme(panel.grid = element_blank(),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15))

wp_silly |> 
  ggplot(aes(x = date, y = first_time)) +
  geom_line(aes(group = period, color = period)) +
  geom_point(aes(shape = period, color = period), size = 4) +
  scale_color_manual(values = c("orchid3", "chocolate1")) +
  labs(x = "Date", y = "Time to start measurements", shape = "Period", color = "Period") +
  theme(panel.grid = element_blank(),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15))
