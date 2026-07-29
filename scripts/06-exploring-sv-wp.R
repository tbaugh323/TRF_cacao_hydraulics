#### Looking at how WP is related to SV ####
library(tidyverse)
theme_set(theme_bw())

#### Reading ####
wp_PDMD_long <- read_csv("data/water_potential/wp_PDMD_long.csv") |> 
  mutate(Tree_ID = case_when(individual == 1 ~ "BioR1192",
                             individual == 2 ~ "BioR1171",
                             individual == 3 ~ "BioR1170",
                             individual == 4 ~ "BioR1191"),
         Tree_ID = case_when(individual == 2 & canopy == "Upper canopy" ~ paste0(Tree_ID, "_EB"),
                             individual == 2 & canopy == "Lower canopy" ~ paste0(Tree_ID, "_LB"),
                             TRUE ~ Tree_ID))

R1171 <- read_csv("data/sap_flow/raw_sapflow/R1171-postprocess.csv") |> 
  mutate(Datetime = as.POSIXct(Datetime, tz = "America/Phoenix")) |> 
  rename(date = Date) |> 
  mutate(Tree_ID = paste0(Tree_ID, "_", location)) |> 
  dplyr::select(Datetime, date, Tree_ID, VhrmHRM5, VhrmHRM15, VhrmHRM25, 
                VhrmHRM35, Year, Month, Day, Hour, Minute, Second)

R1132 <- read_csv("data/sap_flow/raw_sapflow/R1132-postprocess.csv") |> 
  mutate(Datetime = lubridate::force_tz(Datetime, tzone = "America/Phoenix")) |> 
  mutate(date = as.Date(Datetime),
         TreeID = "BioR1132") |> 
  rename(Tree_ID = TreeID) |> 
  dplyr::select(Datetime, date, Tree_ID, VhrmHRM5, VhrmHRM15, VhrmHRM25, 
                VhrmHRM35, Year, Month, Day, Hour, Minute, Second)

R1192 <- read_csv("data/sap_flow/raw_sapflow/R1192B-postprocess.csv") |> 
  mutate(Datetime = lubridate::force_tz(Datetime, tzone = "America/Phoenix")) |> 
  mutate(date = as.Date(Datetime),
         TreeID = "BioR1192") |> 
  rename(Tree_ID = TreeID) |> 
  dplyr::select(Datetime, date, , Tree_ID, VhrmHRM5, VhrmHRM15, VhrmHRM25, 
                VhrmHRM35, Year, Month, Day, Hour, Minute, Second)

# Writing this out IN LOCAL TIME
sv_all <- rbind(R1171, R1132, R1192) |> 
  rename(dt = Datetime)

write_csv(sv_all, "data/sap_flow/sv_all.csv")

#### Joining ####

wp_list <- unique(wp_PDMD_long$date)
sv_list <- unique(c(R1171$date, R1132$date, R1192$date))
overlap_list <- wp_list[wp_list %in% sv_list]

R1171_small <- R1171 |> filter(date %in% overlap_list) |> 
  rename(dt = Datetime)

R1132_small <- R1132 |> filter(date %in% overlap_list) |> 
  rename(dt = Datetime)

R1192_small <- R1192 |> filter(date %in% overlap_list) |> 
  rename(dt = Datetime)

wp_small <- wp_PDMD_long |> filter(date %in% overlap_list)

sv_full <- rbind(R1171_small, R1132_small, R1192_small)

sv_wp_full <- full_join(sv_full, wp_small, by = c("dt", "date", "Tree_ID"))

sv_wp_fuller <- data.frame()
for (i in 1:length(unique(sv_wp_full$Tree_ID))) {
  current_df <- sv_wp_full |> 
    filter(Tree_ID == unique(sv_wp_full$Tree_ID)[i])
  
  current_df <- current_df |> 
    mutate(VhrmHRM5_norm = VhrmHRM5 / max(VhrmHRM5, na.rm = TRUE))
  
  sv_wp_fuller <- bind_rows(sv_wp_fuller, current_df)
}
  
#### Exploring ####

sv_wp_fuller |> 
  filter(Tree_ID == "BioR1171_EB" |
           Tree_ID == "BioR1171_LB" |
           Tree_ID == "BioR1192") |> 
  ggplot(aes(x = dt, y = wp_m)) +
  geom_line(aes(x = dt, y = VhrmHRM5_norm, group = date), color = "forestgreen") +
  geom_point(aes(x = dt, y = VhrmHRM5_norm), color = "forestgreen") +
  geom_point(aes(x = dt, y = wp_m * 1, color = period), size = 2, alpha = 0.7) +
  scale_color_manual(values = c("orchid3", "chocolate1")) +
  scale_y_continuous("Sap flow velocity at 5 cm (normalized)",
                     sec.axis = sec_axis(~ . / 1, name = expression(paste(Psi)),
                                         breaks = seq(0, -3.5, -0.5))) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~ Tree_ID)

sv_wp_fuller |> 
  filter(Tree_ID == "BioR1171_EB" |
           Tree_ID == "BioR1171_LB" |
           Tree_ID == "BioR1192",
         !is.na(period)) |> 
  ggplot(aes(y = VhrmHRM5_norm, x = wp_m)) +
  geom_point(aes(color = period, shape = period), size = 4) +
  scale_color_manual(values = c("orchid3", "chocolate1")) +
  # facet_wrap(~ Tree_ID) +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        plot.title = element_text(size = 15))

sv_wp_works <- sv_wp_fuller |> 
  filter(Tree_ID == "BioR1171_EB" | 
           Tree_ID == "BioR1171_LB" | 
           Tree_ID == "BioR1192") |> 
  filter(period == "MD")

# doing slopes
sv_wp_lms <- data.frame(Tree_ID = NA, slope = NA, intercept = NA, r2 = NA)
for (i in 1:length(unique(sv_wp_works$Tree_ID))) {
  current_tree <- sv_wp_works |> 
    filter(Tree_ID == unique(sv_wp_works$Tree_ID)[i])
  
  current_lm <- lm(current_tree$VhrmHRM5_norm ~ current_tree$wp_m)
  
  sv_wp_lms <- sv_wp_lms |> 
    add_row(Tree_ID = unique(sv_wp_works$Tree_ID)[i], 
            slope = current_lm$coefficients[2], 
            intercept = current_lm$coefficients[1], 
            r2 = summary(current_lm)$r.squared)
}
sv_wp_lms <- sv_wp_lms |> filter(!is.na(Tree_ID)) |> 
  rename(lambda_sv = slope)

sv_wp_fuller |> 
  filter(Tree_ID == "BioR1171_EB" |
           Tree_ID == "BioR1171_LB" |
           Tree_ID == "BioR1192",
         !is.na(period)) |> 
  filter(Tree_ID != "BioR1192") |> 
  mutate(Tree_ID = case_when(Tree_ID == "BioR1171_EB" ~ "Upper canopy",
                             Tree_ID == "BioR1171_LB" ~ "Lower canopy")) |> 
  ggplot(aes(y = VhrmHRM5_norm, x = wp_m)) +
  geom_abline(data = sv_wp_lms |> 
                filter(Tree_ID != "BioR1192") |> 
                mutate(Tree_ID = case_when(Tree_ID == "BioR1171_EB" ~ "Upper canopy",
                                           Tree_ID == "BioR1171_LB" ~ "Lower canopy")), 
              aes(slope = lambda_sv, intercept = intercept), color = "orchid3", 
              linewidth = 1) +
  geom_point(aes(color = period, shape = period), size = 5) +
  scale_color_manual(values = c("orchid3", "chocolate1")) +
  labs(x = expression(paste(Psi)), y = expression(paste(V["s,5"], " (cm/hr)")),
       color = "Period", shape = "Period") +
  facet_wrap(~ Tree_ID) +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 23),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 23),
        plot.title = element_text(size = 25),
        strip.text = element_text(size = 23, color = "white"),
        strip.background = element_rect(fill = "#205A3D"))

# these slopes will act as the λG[c,norm]!

write_csv(sv_wp_lms, "data/sap_flow/lambda_sv_norm.csv")

