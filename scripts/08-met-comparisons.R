#### Seeing how response variables change with env conditions ####
library(tidyverse)
theme_set(them_bw())

#### Reading in data ####

all_dendros <- read_csv("data/dendro_data/all_dendros.csv")

sv_all <- read_csv("data/sap_flow/sv_all.csv")

#### Visualizing TWD ####

all_dendros |> 
  # filter(date == as.Date("2026-07-08") | date == as.Date("2026-07-09")) |> 
  ggplot(aes(x = Temperature, y = twd_norm)) +
  geom_point(aes(color = datetime)) +
  scale_color_viridis_c(option = "viridis", trans = scales::time_trans()) +
  facet_wrap(~ Tree_ID)

# Showing difference in amplitude change VS TWD accumulation
all_dendros |> 
  filter(Tree_ID == "BioR1170" | Tree_ID == "BioR1171") |> 
  filter(date == as.Date("2026-07-07") | 
           date == as.Date("2026-07-08") | 
           date == as.Date("2026-07-09") | 
           date == as.Date("2026-07-10")) |> 
  ggplot(aes(x = Temperature, y = twd_norm)) +
  geom_point(aes(color = datetime), size = 4) +
  scale_color_viridis_c(option = "viridis", trans = scales::time_trans()) +
  facet_wrap(~ Tree_ID) +
  labs(x = expression(paste("Temperature (", degree, "C)")), 
       y = expression(paste(TWD[norm])), color = "Date") +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 23),
        axis.title = element_text(size = 25),
        legend.text = element_text(size = 23),
        legend.title = element_text(size = 25),
        strip.text = element_text(size = 23, color = "white"),
        strip.background = element_rect(fill = "#205A3D"))

all_dendros |> 
  filter(Tree_ID == "BioR1170" | Tree_ID == "BioR1171") |> 
  filter(date == as.Date("2026-07-15") | 
           date == as.Date("2026-07-16") | 
           date == as.Date("2026-07-17") |
           date == as.Date("2026-07-18")) |> 
  ggplot(aes(x = Temperature, y = twd_norm)) +
  geom_point(aes(color = datetime), size = 4) +
  geom_point(data = all_dendros |> 
               filter(datetime == as.POSIXct("2026-07-16 00:00:00"), 
                      Tree_ID == "BioR1170" | Tree_ID == "BioR1171"), 
             aes(x = Temperature, y = twd_norm), 
             color = "cornflowerblue", size = 10, alpha = 0.7) +
  scale_color_viridis_c(option = "viridis", trans = scales::time_trans()) +
  facet_wrap(~ Tree_ID) +
  labs(x = expression(paste("Temperature (", degree, "C)")), 
       y = expression(paste(TWD[norm])), color = "Date") +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 23),
        axis.title = element_text(size = 25),
        legend.text = element_text(size = 23),
        legend.title = element_text(size = 25),
        strip.text = element_text(size = 23, color = "white"),
        strip.background = element_rect(fill = "#205A3D"))

all_dendros |> 
  filter(Tree_ID == "BioR1170" | Tree_ID == "BioR1171") |> 
  filter(date == as.Date("2026-07-18") | 
           date == as.Date("2026-07-19") | 
           date == as.Date("2026-07-20")) |> 
  ggplot(aes(x = Temperature, y = twd_norm)) +
  geom_point(aes(color = datetime)) +
  scale_color_viridis_c(option = "viridis", trans = scales::time_trans()) +
  facet_wrap(~ Tree_ID) +
  labs(x = expression(paste("Temperature (", degree, "C)")), 
       y = expression(paste(TWD[norm])), color = "Date") +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        strip.text = element_text(size = 13))

all_dendros |> 
  filter(Tree_ID == "BioR1170" | 
           Tree_ID == "BioR1171" | 
           Tree_ID == "BioR1192") |> 
  ggplot(aes(x = datetime)) +
  geom_line(aes(y = twd_norm, color = VPD)) +
  geom_point(aes(y = twd_norm, color = VPD)) +
  scale_color_viridis_c(option = "rocket") +
  facet_wrap(~ Tree_ID, ncol = 1) +
  labs(x = "Date", y = expression(paste(TWD[norm])), color = "VPD") +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        strip.text = element_text(size = 13))

all_dendros |> 
  filter(Tree_ID == "BioR1170" | 
           Tree_ID == "BioR1171") |> 
  filter(date >= as.Date("2026-07-05") & date <= as.Date("2026-07-20")) |> 
  ggplot(aes(x = datetime)) +
  geom_vline(aes(xintercept = as.Date("2026-07-16")), color = "navy", 
             linewidth = 2, alpha = 0.4) +
  geom_line(aes(y = twd_norm, color = VPD)) +
  geom_point(aes(y = twd_norm, color = VPD), size = 2) +
  scale_color_viridis_c(option = "rocket") +
  facet_wrap(~ Tree_ID) +
  labs(x = "Date", y = expression(paste(TWD[norm])), color = "VPD") +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 23),
        axis.title = element_text(size = 25),
        legend.text = element_text(size = 23),
        legend.title = element_text(size = 25),
        strip.text = element_text(size = 23, color = "white"),
        strip.background = element_rect(fill = "#205A3D"))

#### Visualizing sap velocity ####

all_met <- read_csv("data/met_data/all_met.csv")

sv_vpd <- left_join(sv_all, all_met |> 
                      rename(dt = DateTime_MST), by = c("Tree_ID", "dt"))

sv_vpd |> 
  filter(Tree_ID_location != "BioR1171_M", Tree_ID != "BioR1171_NB_location") |> 
  filter(date >= as.Date("2026-06-23")) |> 
  filter(VhrmHRM5 <= 40) |> 
  ggplot() +
  geom_line(aes(x = dt, y = VhrmHRM5, color = TA), linewidth = 0.5) +
  geom_point(aes(x = dt, y = VhrmHRM5, color = TA), size = 0.5) +
  scale_color_viridis_c(option = "rocket") +
  facet_wrap(~ Tree_ID)

sv_vpd |> 
  filter(Tree_ID_location != "BioR1171_M", Tree_ID_location != "BioR1171_NB") |> 
  filter(date == as.Date("2026-07-08")) |> 
  filter(VhrmHRM5 <= 40) |> 
  ggplot(aes(x = TA, y = VhrmHRM5)) +
  geom_point(aes(color = dt)) +
  scale_color_viridis_c(option = "viridis", trans = scales::time_trans()) +
  facet_wrap(~ Tree_ID)

# what the heck is going on


