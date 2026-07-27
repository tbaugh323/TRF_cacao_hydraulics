#### Trying to figure out what's going on with sap flow ####
library(tidyverse)
theme_set(theme_bw())

#### Reading in ####

sv_all <- read_csv("data/sap_flow/sv_all.csv") |> 
  mutate(dt = as.POSIXct(dt, tz = "America/Phoenix")) |> 
  mutate(condition = case_when(date <= as.Date("2026-06-30") ~ "Predrought",
                               date > as.Date("2026-06-30") & date <= as.Date("2026-07-15") ~ "Drought",
                               date > as.Date("2026-07-15") ~ "Recovery"))

sv_all |> 
  filter(dt >= as.POSIXct("2026-06-23 00:00:00")) |> 
  filter(VhrmHRM5 <= 30) |> 
  ggplot(aes(x = dt, y = VhrmHRM5)) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(color = factor(condition, levels = c("Predrought", "Drought", "Recovery"))), size = 0.6) +
  scale_color_manual(values = c("springgreen4", "lightsalmon3", "yellowgreen")) +
  labs(x = "Date", y = "Sap velocity at 5 mm (cm/hr)", color = "Condition") +
  facet_wrap(~ Tree_ID) +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.position = "none",
        strip.text = element_text(size = 13))

sv_all |> 
  filter(dt >= as.POSIXct("2026-06-23 00:00:00")) |> 
  filter(Tree_ID == "BioR1171_EB" | Tree_ID == "BioR1171_LB") |> 
  filter(VhrmHRM5 <= 30) |> 
  ggplot(aes(x = dt, y = VhrmHRM5)) +
  geom_hline(aes(yintercept = 0)) +
  geom_line(aes(color = factor(condition, levels = c("Predrought", "Drought", "Recovery")))) +
  geom_point(aes(color = factor(condition, levels = c("Predrought", "Drought", "Recovery"))), size = 0.6) +
  scale_color_manual(values = c("springgreen4", "lightsalmon3", "yellowgreen")) +
  labs(x = "Date", y = "Sap velocity at 5 mm (cm/hr)", color = "Condition") +
  facet_wrap(~ Tree_ID) +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        legend.position = "none",
        strip.text = element_text(size = 13))

# sv_all |> 
#   filter(date == as.Date("2026-07-12") | date == as.Date("2026-07-13")) |> 
#   filter(VhrmHRM5 <= 30) |> 
#   ggplot(aes(x = dt, y = VhrmHRM5)) +
#   geom_point(aes(color = factor(condition, levels = c("Predrought", "Drought", "Recovery"))), size = 0.6) +
#   scale_color_manual(values = c("springgreen4", "lightsalmon3", "yellowgreen")) +
#   labs(color = "Condition") +
#   facet_wrap(~ Tree_ID)

sv_sum <- sv_all |> 
  group_by(date, condition, Tree_ID) |> 
  summarize(VhrmHRM5_max = max(VhrmHRM5, na.rm = TRUE),
            VhrmHRM5_min = min(VhrmHRM5, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(VhrmHRM5_amp = VhrmHRM5_max - VhrmHRM5_min)

sv_sum |> 
  ggplot(aes(x = date, y = VhrmHRM5_amp)) +
  geom_point(aes(color = factor(condition, levels = c("Predrought", "Drought", "Recovery")))) +
  scale_color_manual(values = c("springgreen4", "lightsalmon3", "yellowgreen")) +
  labs(color = "Condition") +
  facet_wrap(~ Tree_ID)
# not super informative actually






