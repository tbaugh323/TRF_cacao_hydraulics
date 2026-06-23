
hold <- one_clean |>
  select(dt, individual, canopy, period, `Obs#`, Time, Date, configAuthor, gsw, gbw, gtw) |> 
  pivot_longer(!c(dt, individual, canopy, period, `Obs#`, Time, Date, configAuthor),
               names_to = "g_measurement",
               values_to = "g_value")

li <- hold |> 
  filter(configAuthor == "LI-COR Default") |> 
  ggplot(aes(x = dt, y = g_value)) +
  ggtitle("LICOR") +
  geom_point() +
  facet_wrap(~ g_measurement, scales = "free")

ps <- hold |> 
  filter(configAuthor == "PSA-00618") |> 
  ggplot(aes(x = dt, y = g_value)) +
  ggtitle("PSF") +
  geom_point() +
  facet_wrap(~ g_measurement, scales = "free")

li / ps



one <- read_csv("data/licor600_data/2026-06-22_morning.csv")
one <- one[-3,]
one <- one[-2,]
one <- one[-1,]

one_clean <- one |> 
  mutate(individual = c(rep(1, 6), rep(2, 6), rep(2, 6), rep(1, 6), rep(3, 12), rep(4, 12)),
         canopy = c(rep("lower", 12), rep("upper", 12), rep("lower", 6), rep("upper", 6), rep("lower", 6), rep("upper", 6)),
         Date = as.Date(Date),
         Time = hms::as_hms(Time),
         dt = as.POSIXct(paste(Date, Time)),
         period = case_when(Time <= hms::as_hms("9:00:00") ~ "morning",
                            Time > hms::as_hms("9:00:00") & Time < hms::as_hms("14:00:00") ~ "noon",
                            Time >= hms::as_hms("14:00:00") ~ "afternoon")) |> 
  relocate(dt, individual, canopy, period) |> 
  mutate(across(c(Observation:leaf_width, Fo:batt, rh_adj:Ble, flash_intensity:z_flr), as.numeric))

one_clean_licor <- one_clean |> 
  filter(configAuthor == "LI-COR Default") |> 
  mutate(canopy = ifelse(canopy == "lower", "Lower", "Upper"),
         individual = as.character(individual))

gsw <- one_clean |> 
  ggplot(aes(x = dt, y = gsw)) + 
  geom_point(aes(color = factor(individual), shape = canopy)) +
  facet_wrap(~configAuthor, scales = "free") +
  theme(panel.grid = element_blank())

gbw <- one_clean |> 
  ggplot(aes(x = dt, y = gbw)) + 
  geom_point(aes(color = factor(individual), shape = canopy)) +
  facet_wrap(~configAuthor, scales = "free") +
  theme(panel.grid = element_blank())

gtw <- one_clean |> 
  ggplot(aes(x = dt, y = gtw)) + 
  geom_point(aes(color = factor(individual), shape = canopy)) +
  facet_wrap(~configAuthor, scales = "free") +
  theme(panel.grid = element_blank())

gsw / gbw / gtw

one_clean |> 
  ggplot(aes(x = dt, y = `Fm'`)) +
  geom_point(aes(color = factor(individual), shape = canopy)) +
  facet_wrap(~configAuthor, scales = "free") +
  theme(panel.grid = element_blank())
