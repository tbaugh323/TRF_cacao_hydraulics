#### Looking at how WP is related to SV ####
library(tidyverse)
theme_set(theme_bw())

#### Reading ####
sv_all <- read_csv("data/sap_flow/sv_all.csv") |> 
  mutate(dt = as.POSIXct(dt, tz = "America/Phoenix")) |> 
  mutate(Tree_ID_location = Tree_ID) |> 
  mutate(Tree_ID = case_when(Tree_ID == "BioR1171_T" ~ "BioR1171",
                             Tree_ID == "BioR1171_NB" ~ "BioR1171",
                             Tree_ID == "BioR1171_MB" ~ "BioR1171",
                             Tree_ID == "BioR1171_M" ~ "BioR1171",
                             Tree_ID == "BioR1171_LB" ~ "BioR1171",
                             Tree_ID == "BioR1171_L" ~ "BioR1171",
                             Tree_ID == "BioR1171_EB" ~ "BioR1171",
                             TRUE ~ Tree_ID))
  
all_dendros <- read_csv("data/dendro_data/all_dendros.csv") |> 
  mutate(datetime = as.POSIXct(datetime, tz = "America/Phoenix")) |> 
  rename(dt = datetime)

lambda_sv_norm <- read_csv("data/sap_flow/lambda_sv_norm.csv") |> 
  rename(sv_intercept = intercept, sv_r2 = r2) |> 
  mutate(Tree_ID_location = Tree_ID,
         Tree_ID = case_when(Tree_ID == "BioR1171_LB" ~ "BioR1171",
                             Tree_ID == "BioR1171_EB" ~ "BioR1171",
                             TRUE ~ Tree_ID))

lambda_twd_min <- read_csv("data/dendro_data/lambda_twd_min.csv") |> 
  rename(twd_intercept = intercept, twd_r2 = r2)

#### Joining ####
sv_dendro <- full_join(sv_all, all_dendros, by = c("dt", "date", "Tree_ID")) |> 
  filter(date %in% unique(sv_all$date) & date %in% unique(all_dendros$date))

lambdas <- full_join(lambda_sv_norm, lambda_twd_min, by = "Tree_ID")

#### Visualizing ####
sv_dendro |> 
  ggplot(aes(x = dt)) +
  geom_point(aes(y = VhrmHRM5), color = "red", size = 0.2) +
  geom_point(aes(y = twd_norm * 30), color = "blue", size = 0.2) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 30, "twd_min")) +
  facet_wrap(~ Tree_ID)

puny_lm <- lm(lambda_sv ~ lambda_twd, data = lambdas)
summary(puny_lm)

lambdas |> 
  filter(!is.na(Tree_ID_location)) |> 
  ggplot(aes(x = lambda_twd, y = lambda_sv)) +
  geom_abline(aes(slope = puny_lm$coefficients[2], intercept = puny_lm$coefficients[1]), color = "gray50") +
  geom_point(aes(color = Tree_ID_location), size = 4) +
  scale_x_continuous(limits = c(0.415, 0.412)) +
  labs(x = expression(paste("|", lambda, TWD[pd], "|")), y = expression(paste(lambda, SV["5,norm"]))) +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))





