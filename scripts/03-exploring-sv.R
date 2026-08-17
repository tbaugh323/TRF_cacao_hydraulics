#### Trying to figure out what's going on with sap flow ####
library(tidyverse)
theme_set(theme_bw())

#### Reading in ####

sv_all <- read_csv("data/sap_flow/sv_all.csv") |> 
  mutate(dt = as.POSIXct(dt, tz = "America/Phoenix")) |> 
  mutate(condition = case_when(date <= as.Date("2026-06-30") ~ "Predrought",
                               date > as.Date("2026-06-30") & 
                                 date <= as.Date("2026-07-15") ~ "Drought",
                               date > as.Date("2026-07-15") ~ "Recovery"))

#### Plotting ####
# All sap velocity all time
sv_all |> 
  filter(dt >= as.POSIXct("2026-06-23 00:00:00")) |> 
  filter(VhrmHRM5 <= 30) |> 
  ggplot(aes(x = dt, y = VhrmHRM5)) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(color = factor(condition, 
                                levels = c("Predrought", "Drought", "Recovery"))), 
             size = 0.6) +
  scale_color_manual(values = c("springgreen4", "lightsalmon3", "yellowgreen")) +
  labs(x = "Date", y = "Sap velocity at 5 mm (cm/hr)", color = "Condition") +
  facet_wrap(~ Tree_ID) +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.position = "none",
        strip.text = element_text(size = 13))

# Only 1171_EB and 1171_LB
sv_all |> 
  filter(dt >= as.POSIXct("2026-06-23 00:00:00")) |> 
  filter(Tree_ID == "BioR1171_EB" | Tree_ID == "BioR1171_LB") |> 
  filter(VhrmHRM5 <= 30) |> 
  ggplot(aes(x = dt, y = VhrmHRM5)) +
  geom_hline(aes(yintercept = 0)) +
  # geom_hline(aes(yintercept = max((sv_all |> 
  #                                    filter(Tree_ID == "BioR1171_EB", 
  #                                           dt >= as.POSIXct("2026-06-23 00:00:00")))[4])), 
  #            color = "tomato", alpha = 0.5) +
  # geom_hline(aes(yintercept = max((sv_all |> 
  #                                    filter(Tree_ID == "BioR1171_LB", 
  #                                           dt >= as.POSIXct("2026-06-23 00:00:00")))[4])), 
  #            color = "skyblue2", alpha = 0.5) +
  geom_line(aes(color = factor(condition, 
                               levels = c("Predrought", "Drought", "Recovery")))) +
  geom_point(aes(color = factor(condition, 
                                levels = c("Predrought", "Drought", "Recovery"))), 
             size = 0.6) +
  scale_color_manual(values = c("springgreen4", "lightsalmon3", "yellowgreen")) +
  labs(x = "Date", y = "Sap velocity at 5 mm (cm/hr)", color = "Condition") +
  facet_wrap(~ Tree_ID, ncol = 1) +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.position = "none",
        strip.text = element_text(size = 13))

# Just two days
# sv_all |>
#   filter(date == as.Date("2026-07-12") | date == as.Date("2026-07-13")) |>
#   filter(VhrmHRM5 <= 30) |>
#   ggplot(aes(x = dt, y = VhrmHRM5)) +
#   geom_point(aes(color = factor(condition, levels = c("Predrought", "Drought", "Recovery"))), size = 0.6) +
#   scale_color_manual(values = c("springgreen4", "lightsalmon3", "yellowgreen")) +
#   labs(color = "Condition") +
#   facet_wrap(~ Tree_ID)

#### Stats ####
# Summarizing to daily max/min?
sv_sum <- sv_all |> 
  group_by(date, condition, Tree_ID) |> 
  summarize(VhrmHRM5_max = max(VhrmHRM5, na.rm = TRUE),
            VhrmHRM5_min = min(VhrmHRM5, na.rm = TRUE),
            VhrmHRM5_var = var(VhrmHRM5, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(VhrmHRM5_amp = VhrmHRM5_max - VhrmHRM5_min)

sv_sum |> 
  filter(Tree_ID != "BioR1171_NB", Tree_ID != "BioR1171_M") |> 
  filter(date >= as.Date("2026-06-20")) |> 
  ggplot(aes(x = date, y = VhrmHRM5_var)) +
  geom_point(aes(color = factor(condition, levels = c("Predrought", "Drought", "Recovery")))) +
  scale_color_manual(values = c("springgreen4", "lightsalmon3", "yellowgreen")) +
  labs(color = "Condition") +
  facet_wrap(~ Tree_ID)

sv_aov <- aov(VhrmHRM5_var ~ condition, data = sv_sum)
summary(sv_aov)

sv_sum_2 <- sv_sum |> 
  mutate(condition = case_when(condition == "Predrought" ~ "wet",
                               condition == "Drought" ~ "dry",
                               condition == "Recovery" ~ "wet"))

sv_aov_2 <- aov(VhrmHRM5_var ~ condition, data = sv_sum_2)
summary(sv_aov_2)

# The three phases, treated separately, are significantly different, but the 
# wet phases are not significantly different from each other.

# Summarizing more to just drought phases
sv_super_sum <- sv_sum |> 
  # filter(Tree_ID == "BioR1171_EB" | Tree_ID == "BioR1171_LB" | Tree_ID == "BioR1132" | Tree_ID == "BioR1192") |> 
  group_by(Tree_ID, condition) |> 
  summarize(max_vel = max(VhrmHRM5_max),
            min_vel = min(VhrmHRM5_min)) |> 
  ungroup() |> 
  mutate(max_range = max_vel - min_vel)
View(sv_super_sum)

sv_super_aov <- aov(min_vel ~ condition, data = sv_super_sum)
summary(sv_super_aov)

# Not significant when summarizing more!


