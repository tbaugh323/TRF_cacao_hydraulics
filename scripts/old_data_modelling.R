library(tidyverse)
library(rjags)
load.module('dic')
library(mcmcplots)
# devtools::install_github("fellmk/PostJAGS/postjags")
library(postjags)
library(broom.mixed)

all_old_time <- read_csv("data/old_data/all_old_data.csv") |> 
  mutate(ID = case_when(ID == "SC" ~ 1,
                        ID == "PI" ~ 2,
                        ID == "PA" ~ 3,
                        ID == "HT" ~ 4,
                        ID == "HC" ~ 5,
                        ID == "CP" ~ 6,
                        ID == "CF2" ~ 7,
                        ID == "AM" ~ 8,
                        ID == "AC2" ~ 9,
                        ID == "AC1" ~ 10)) |> 
  filter(!is.na(TotalSapFlow_L_hr) & !is.na(temperature_mtn_100) & !is.na(humidity_mtn_100) & !is.na(radiation_mtn1300_PAR))

# Create list of data
dat_list <- list(y = all_old_time$TotalSapFlow_L_hr,
                 # x = all_old_time$dt,
                 ta = all_old_time$temperature_mtn_100,
                 hu = all_old_time$humidity_mtn_100,
                 rad = all_old_time$radiation_mtn1300_PAR,
                 N = nrow(all_old_time),
                 id = factor(all_old_time$ID),
                 Nid = length(unique(all_old_time$ID)))

# Create list of initials
inits <- function() {
  list(a.mu = rnorm(1, 0, 10),
       b.mu = rnorm(1, 0, 10),
       c.mu = rnorm(1, 0, 10),
       tau = runif(1, 0, 1),
       tau.a = runif(1, 0, 1),
       tau.b = runif(1, 0, 1),
       tau.c = runif(1, 0, 1))
}
inits_list <- list(inits(), inits(), inits())

jm <- jags.model("models/old_test.JAGS",
                 data = dat_list,
                 inits = inits_list,
                 n.chains = 3)
update(jm, 10000)

