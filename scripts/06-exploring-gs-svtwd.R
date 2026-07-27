library(tidyverse)
theme_set(theme_bw())

#### Reading ####

clean_all_licor600 <- read_csv("data/licor600_data/clean_all_licor600.csv") |> 
  dplyr::select(dt, individual, canopy, period, condition, Time, Date, gsw, VPleaf, `Fm'`, Tleaf, instrument) |> 
  mutate(dt = round_date(dt, unit = "30 minutes"),
         individual = case_when(individual == 1 ~ "BioR1192",
                                individual == 2 ~ "BioR1171",
                                individual == 3 ~ "BioR1170",
                                individual == 4 ~ "BioR1192")) |> 
  mutate(individual = case_when(individual == "BioR1171" & canopy == "Lower" ~ "BioR1171_LB",
                                individual == "BioR1171" & canopy == "Upper" ~ "BioR1171_EB",
                                TRUE ~ individual)) |> 
  rename(Tree_ID = individual)

deltas <- read_csv("data/licor600_data/deltas.csv")

sv_all <- read_csv("data/sap_flow/sv_all.csv")

all_dendros <- read_csv("data/dendro_data/all_dendros.csv")

#### Joining ####

sv_gs <- full_join(clean_all_licor600, sv_all, by = c("dt", "Tree_ID")) |> 
  filter(Date >= as.Date("2026-06-20"))

sv_gs |> 
  ggplot(aes(x = dt)) +
  geom_line(aes(y = VhrmHRM5, group = Date), 
            linewidth = 0.4, color = "cornflowerblue") +
  geom_point(aes(y = VhrmHRM5), size = 0.5, color = "cornflowerblue") +
  geom_point(aes(y = gsw * 20), size = 0.5, color = "chocolate1") +
  scale_y_continuous(sec.axis = sec_axis(~. / 20, "gsw")) +
  facet_wrap(~ Tree_ID) +
  theme(panel.grid = element_blank())

ggplot() +
  geom_point(data = sv_all |> filter(date >= as.Date("2026-06-20"),
                                     VhrmHRM5 < 40),
             aes(x = dt, y = VhrmHRM5), size = 0.4, color = "navy") +
  geom_point(data = deltas |> filter(Date <= as.Date("2026-07-16")), 
             aes(x = Date, y = delta1 * 60), size = 2, color = "pink") +
  geom_point(data = deltas |> filter(Date <= as.Date("2026-07-16")), 
             aes(x = Date, y = delta2 * 60), size = 2, color = "hotpink") +
  scale_y_continuous(sec.axis = sec_axis(~ . / 60, "gsw")) +
  facet_wrap(~ Tree_ID)
  
# maybe the best plot here
ggplot() +
  geom_line(data = all_dendros, aes(x = date, y = amplitude, group = Tree_ID), color = "sienna") +
  geom_point(data = all_dendros, aes(x = date, y = amplitude), color = "sienna") +
  geom_line(data = deltas, aes(x = Date, y = delta1 * 500, group = canopy), color = "pink") +
  geom_point(data = deltas, aes(x = Date, y = delta1 * 500), color = "pink") +
  geom_line(data = deltas, aes(x = Date, y = delta2 * 500, group = canopy), color = "hotpink") +
  geom_point(data = deltas, aes(x = Date, y = delta2 * 500), color = "hotpink") +
  scale_y_continuous(sec.axis = sec_axis(~ . / 500, "gsw")) +
  facet_wrap(~ Tree_ID)

ggplot() +
  geom_point(data = clean_all_licor600, aes(x = Tleaf, y = gsw, color = dt)) +
  facet_wrap(~ factor(condition, levels = c("predrought", "drought", "recovery")))


  
  
  
