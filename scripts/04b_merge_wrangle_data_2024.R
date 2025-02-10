# Merge and wrangle data

# Initial settings --------------------------------------------------------

library(tidyverse)
source("functions/clean_edata.R")

# Load data ---------------------------------------------------------------

load("output/election_data.RData")
mun2024 <- read_rds("output/munlist2024.rds")

# Clean election data -----------------------------------------------------

ele2024 <- clean_edata(smd2024) %>% 
  mutate(munname =　case_when(munname == "三浦郡葉山町" ~ "葉山町", 
                             munname == "越智郡上島町" ~ "上島町", 
                             munname == "千種区" ~ "名古屋市千種区", 
                             munname == "名東区" ~ "名古屋市名東区", 
                             munname == "守山区" ~ "名古屋市守山区", 
                             munname == "千種区" ~ "名古屋市千種区", 
                             TRUE ~ munname))

# Find special cases
ele2024 %>% filter(str_detect(munname, "・|、"))

# Add municipality codes and weights  -------------------------------------

e2024muncode <- ele2024 %>% 
  full_join(mun2024, by = c("prefecture", "munname")) %>% 
  group_by(prefecture, munname) %>% 
  mutate(weight = vote_total / sum(vote_total)) %>% 
  ungroup()

# Save data ---------------------------------------------------------------

saveRDS(e2024muncode, "output/e2024muncode.RDS")

