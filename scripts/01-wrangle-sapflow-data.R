#### Wrangling and working with sap flow data ####
library(tidyverse)
library(dygraphs)
theme_set(theme_bw())

#### Exploring ####
test <- read_csv("data/sap_flow/R1171-processed.csv")
summary(test)

test |> 
  ggplot() +
  geom_point(aes(x = Datetime, y = VhrmHRM5), color = "red") +
  geom_point(aes(x = Datetime, y = VhrmHRM15), color = "goldenrod") +
  geom_point(aes(x = Datetime, y = VhrmHRM25), color = "green") +
  geom_point(aes(x = Datetime, y = VhrmHRM35), color = "blue")

test |> 
  filter(Month == 5) |> 
  ggplot(aes(x = Datetime, y = VhrmHRM5)) +
  geom_line()

# Looking at time series
# Making xts objects
test_dy5 <- test |> 
  select(Datetime, VhrmHRM5)
test_dy5 <- xts::xts(test_dy5[,-1], order.by = test_dy5$Datetime)
test_dy15 <- test |> 
  select(Datetime, VhrmHRM5)
test_dy15 <- xts::xts(test_dy15[,-1], order.by = test_dy15$Datetime)
test_dy25 <- test |> 
  select(Datetime, VhrmHRM5)
test_dy25 <- xts::xts(test_dy25[,-1], order.by = test_dy25$Datetime)
test_dy35 <- test |> 
  select(Datetime, VhrmHRM5)
test_dy35 <- xts::xts(test_dy35[,-1], order.by = test_dy35$Datetime)

# dygraphs
dygraph(test_dy5, group = "r1171") |> 
  dySeries("VhrmHRM5") |> 
  dyAxis("y", label = "VhrmHRM5") |> 
  dyRangeSelector()

dygraph(test_dy15, group = "r1171") |> 
  dySeries("VhrmHRM5") |> 
  dyAxis("y", label = "VhrmHRM15") |> 
  dyRangeSelector()

dygraph(test_dy25, group = "r1171") |> 
  dySeries("VhrmHRM5") |> 
  dyAxis("y", label = "VhrmHRM25") |> 
  dyRangeSelector()

dygraph(test_dy35, group = "r1171") |> 
  dySeries("VhrmHRM5") |> 
  dyAxis("y", label = "VhrmHRM35") |> 
  dyRangeSelector()

#### Reading ####

lower <- read_csv("data/sap_flow/tree_2/R1171_L-processed.csv")
lower <- lower[,-2] |> 
  relocate(Datetime)

tree_2_files <- list.files("data/sap_flow/tree_2/")
tree_2_raw <- grep("R1171", tree_2_files)
tree_2_raw <- c(tree_2_files[grep("R1171", tree_2_files)])
tree_2_all <- data.frame()

for (i in 1:length(tree_2_raw)) {
  
  # Extracting the label for each sensor
  id <- tree_2_raw[i]
  tag <- id |> str_extract("(?<=_).*(?=\\-)")
  
  hold <- read_csv(paste0("data/sap_flow/tree_2/", tree_2_raw[i])) |> 
    mutate(location = tag) |> 
    relocate(Datetime, location)
  
  tree_2_all <- bind_rows(hold, tree_2_all)
}

tree_2_clean <- tree_2_all |> 
  mutate(Date = as.Date(Datetime))

write_csv(tree_2_clean, "data/sap_flow/tree_2/tree_2_all.csv")

