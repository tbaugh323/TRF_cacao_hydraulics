library(tidyverse)
theme_set(theme_bw())

# Concatenating
tidy_files <- list.files("data/old_data/tidy/")

all_old <- data.frame()

for(i in 1:length(tidy_files)) {
  current_file <- tidy_files[i]
  current_name <- sub("_.*", "", current_file)
  hold_old <- read_csv(paste0("data/old_data/tidy/", current_file)) |> 
    mutate(ID = current_name)
  all_old <- bind_rows(hold_old, all_old)
}

# Cleaning
all_old_clean <- all_old |> 
  mutate(TIMESTAMP = as.POSIXct(TIMESTAMP, format = "%m/%d/%Y %H:%M")) |> 
  rename(dt = TIMESTAMP) |> 
  relocate(dt, ID) |> 
  mutate(period = case_when(ID == "AC1" & dt < as.POSIXct("2021-12-31") ~ "AC1_winter21",
                            ID == "AC1" & dt > as.POSIXct("2021-12-31") ~ "AC1_winter22",
                            ID == "AC2" & dt < as.POSIXct("2022-01-01") ~ "AC2_winter21",
                            ID == "AC2" & dt > as.POSIXct("2022-01-01") ~ "AC2_winter22",
                            ID == "CF2" & dt < as.POSIXct("2021-11-15") ~ "CF2_fall21",
                            ID == "CF2" & dt > as.POSIXct("2021-11-15") & dt < as.POSIXct("2022-02-01") ~ "CF2_winter21",
                            ID == "CF2" & dt > as.POSIXct("2022-02-01") ~ "CF2_winter22",
                            ID == "CP" & dt < as.POSIXct("2021-12-01") ~ "CP_fall21",
                            ID == "CP" & dt > as.POSIXct("2021-12-01") ~ "CP_winter22",
                            ID == "HT" & dt < as.POSIXct("2022-01-01") ~ "HT_winter21",
                            ID == "HT" & dt > as.POSIXct("2022-01-01") ~ "HT_winter22",
                            ID == "PA" & dt < as.POSIXct("2021-12-01") ~ "PA_winter21",
                            ID == "PA" & dt > as.POSIXct("2021-12-01") ~ "PA_winter22",
                            ID == "SC" & dt < as.POSIXct("2022-01-01") ~ "SC_winter21",
                            ID == "SC" & dt > as.POSIXct("2022-01-01") ~ "SC_winter22",
                            TRUE ~ ID))

ID_names <- unique(all_old_clean$ID)

# Looking
all_old_clean |> 
  ggplot(aes(x = dt)) +
  geom_point(aes(y = TotalSapFlow_L_hr), size = 0.3) +
  facet_wrap(~ ID, scales = "free_y")

all_old_clean |> 
  ggplot(aes(x = dt)) +
  geom_point(aes(y = Dendro_um), size = 0.3) +
  facet_wrap(~ ID, scales = "free_y")

all_old_clean |> 
  ggplot(aes(x =  dt)) +
  geom_point(aes(y = radiation_mtn1300_PAR), size = 0.3) +
  facet_wrap(~ ID, scales = "free_y")

all_old_clean |> 
  filter(Dendro_um < 6500) |> 
  filter(TotalSapFlow_L_hr < 1000) |> 
  ggplot(aes(x = dt)) +
  geom_point(aes(y = radiation_mtn1300_PAR), size = 0.3, color = "red") +
  geom_point(aes(y = humidity_mtn_300), size = 0.3, color = "limegreen") +
  geom_point(aes(y = temperature_mtn_300), size = 0.3, color = "tomato") +
  geom_point(aes(y = VPD_3m), size = 0.3, color = "seagreen") +
  geom_point(aes(y = TotalSapFlow_L_hr), size = 0.3, color = "darkgoldenrod") +
  geom_point(aes(y = Dendro_um / 100), size = 0.3, color = "chocolate2") +
  scale_y_continuous(sec.axis = sec_axis(~ . * 100, "dendro")) +
  facet_wrap(~ ID, scales = "free_x")

all_old_clean |> 
  filter(Dendro_um < 6500) |> 
  filter(TotalSapFlow_L_hr < 1000) |> 
  ggplot(aes(x = dt)) +
  geom_point(aes(y = humidity_mtn_100 / 3), size = 0.3, color = "plum1") +
  geom_point(aes(y = humidity_mtn_300 / 3), size = 0.3, color = "orchid1") +
  geom_point(aes(y = humidity_mtn_700 / 3), size = 0.3, color = "orchid3") +
  geom_point(aes(y = humidity_mtn_1300 / 3), size = 0.3, color = "mediumorchid3") +
  geom_point(aes(y = humidity_mtn_2000 / 3), size = 0.3, color = "mediumorchid4") +
  geom_point(aes(y = temperature_mtn_100), size = 0.3, color = "cadetblue2") +
  geom_point(aes(y = temperature_mtn_300), size = 0.3, color = "darkslategray3") +
  geom_point(aes(y = temperature_mtn_700), size = 0.3, color = "lightblue4") +
  geom_point(aes(y = temperature_mtn_1300), size = 0.3, color = "slategray3") +
  geom_point(aes(y = temperature_mtn_2000), size = 0.3, color = "slategray") +
  scale_y_continuous(sec.axis = sec_axis(~ . * 3, "Humidity (%)")) +
  labs(y = expression(paste("Temperature (", degree, "C)"))) +
  facet_wrap(~ period, scales = "free_x") +
  theme(axis.title.y.left = element_text(color = "mediumorchid4"),
        axis.title.y.right = element_text(color = "lightblue4"))

all_old_clean |> 
  filter(Dendro_um < 6500) |> 
  filter(TotalSapFlow_L_hr < 1000) |> 
  ggplot(aes(x = dt)) +
  geom_point(aes(y = humidity_mtn_100 / 3), size = 0.3, color = "plum1") +
  geom_point(aes(y = humidity_mtn_300 / 3), size = 0.3, color = "orchid1") +
  geom_point(aes(y = humidity_mtn_700 / 3), size = 0.3, color = "orchid3") +
  geom_point(aes(y = humidity_mtn_1300 / 3), size = 0.3, color = "mediumorchid3") +
  geom_point(aes(y = humidity_mtn_2000 / 3), size = 0.3, color = "mediumorchid4") +
  geom_point(aes(y = temperature_mtn_100), size = 0.3, color = "cadetblue2") +
  geom_point(aes(y = temperature_mtn_300), size = 0.3, color = "darkslategray3") +
  geom_point(aes(y = temperature_mtn_700), size = 0.3, color = "lightblue4") +
  geom_point(aes(y = temperature_mtn_1300), size = 0.3, color = "slategray3") +
  geom_point(aes(y = temperature_mtn_2000), size = 0.3, color = "slategray") +
  scale_y_continuous(sec.axis = sec_axis(~ . * 3, "Humidity (%)")) +
  labs(y = expression(paste("Temperature (", degree, "C)"))) +
  theme(axis.title.y.left = element_text(color = "mediumorchid4"),
        axis.title.y.right = element_text(color = "lightblue4"))


all_old_clean |> 
  ggplot() +
  geom_point(aes(x = temperature_mtn_300, y = Vh_Outer_cm_hr), color = "forestgreen") +
  geom_point(aes(x = temperature_mtn_300, y = Vh_Inner_cm_hr), color = "yellowgreen") +
  facet_wrap(~ ID, scales = "free_y")

all_old_clean |> 
  filter(ID == "AC1") |>
  # filter(TotalSapFlow_L_hr > 2000) |> 
  ggplot(aes(x = humidity_mtn_100)) +
  geom_point(aes(y = TotalSapFlow_L_hr, color = temperature_mtn_100))

# AM, HT, CF2, 

all_old_clean |> 
  filter(!is.na(radiation_mtn1300_PAR)) |> 
  # filter(ID == ID_names[6]) |> 
  ggplot() +
  geom_line(aes(x = as.POSIXct(dt), y = radiation_mtn1300_PAR), linewidth = 0.3)
  # facet_wrap(~ period, scales = "free_x")

all_old_clean |> 
  filter(ID == ID_names[2]) |>
  # filter(TotalSapFlow_L_hr < 12) |> 
  ggplot() +
  geom_line(aes(x = dt, y = radiation_mtn1300_PAR), linewidth = 0.3) +
  geom_point(aes(x = dt, y = TotalSapFlow_L_hr), size = 0.4, color = "tomato")

all_old_clean |> 
  ggplot() + 
  geom_point(aes(x = radiation_mtn1300_PAR, y = TotalSapFlow_L_hr), size = 0.3, alpha = 0.5) +
  facet_wrap(~ ID, scales = "free_y")

all_old_clean |> 
  ggplot() + 
  geom_point(aes(x = radiation_mtn1300_PAR, y = Dendro_um), size = 0.3, alpha = 0.5) +
  facet_wrap(~ ID, scales = "free_y")

all_old_clean |> 
  ggplot() + 
  geom_point(aes(x = TotalSapFlow_L_hr, y = Dendro_um), size = 0.3, alpha = 0.5) +
  facet_wrap(~ ID, scales = "free")

all_old_time <- all_old_clean |> 
  mutate(time = hms::as_hms(dt)) |> 
  mutate(date = as.Date(dt))

all_old_time |> 
  filter(TotalSapFlow_L_hr > 0) |> 
  ggplot(aes(x = time)) +
  geom_point(aes(y = TotalSapFlow_L_hr, color = date), size = 0.4, alpha = 0.5) +
  scale_color_viridis_c(option = "turbo", trans = "date") +
  facet_wrap(~ID, scales = "free_y") +
  theme(panel.grid = element_blank())

all_old_time |> 
  ggplot(aes(x = time)) +
  geom_point(aes(y = radiation_mtn1300_PAR), color = "gold", size = 0.4, alpha = 0.5) +
  geom_point(aes(y = temperature_mtn_100 * 20), color = "hotpink", size = 0.3, alpha = 0.5) +
  geom_point(aes(y = humidity_mtn_100 * 10), color = "limegreen",  size = 0.3, alpha = 0.5) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 20, "temperature_mtn_100 | humidity_mtn_100 / 2")) +
  # scale_color_viridis_c(option = "turbo", trans = "date") +
  facet_wrap(~ID, scales = "free_y")

all_old_time |> 
  filter(Vh_Outer_cm_hr != 0) |> 
  filter(Vh_Inner_cm_hr != 0) |> 
  ggplot(aes(x = time)) +
  geom_point(aes(y = Vh_Outer_cm_hr), color = "forestgreen",  size = 0.3, alpha = 0.5) +
  geom_point(aes(y = Vh_Inner_cm_hr), color = "limegreen",  size = 0.3, alpha = 0.5) +
  # scale_color_viridis_c(option = "turbo", trans = "date") +
  facet_wrap(~ ID, scales = "free_y")

all_old_time |>  
  ggplot(aes(x = time)) +
  geom_point(aes(y = Dendro_um, color = date),  size = 0.3, alpha = 0.5) +
  scale_color_viridis_c(option = "turbo", trans = "date") +
  facet_wrap(~ ID, scales = "free_y")

write_csv(all_old_time, "data/old_data/all_old_data.csv")

all_old_binned <- all_old_time |> 
  mutate(sap_flow_binned = cut(TotalSapFlow_L_hr, breaks = 2000),
         temp_100_binned = cut(temperature_mtn_100, breaks = 20),
         hum_100_binned = cut(humidity_mtn_100, breaks = 20),
         dendro_binned = cut(Dendro_um, breaks = 40))

all_old_binned |> 
  filter(ID %in% c("AC1", "AC2", "AM", "CF2", "HT", "PI")) |> 
  group_by(dendro_binned) |> 
  summarize(n = n())

all_old_binned |> 
  filter(ID %in% c("AC1", "AC2", "AM", "CF2", "HT", "PI")) |> 
  ggplot(aes(x = sap_flow_binned, y = humidity_mtn_300)) +
  geom_boxplot()

all_old_binned |> 
  filter(ID %in% c("AC1", "AC2", "AM", "CF2", "HT", "PI")) |> 
  ggplot(aes(x = dendro_binned, y = radiation_mtn1300_PAR)) +
  geom_boxplot()

all_old_binned |> 
  filter(ID %in% c("AC1", "AC2", "AM", "CF2", "HT", "PI")) |> 
  ggplot(aes(x = hum_100_binned, y = TotalSapFlow_L_hr)) +
  geom_boxplot()

