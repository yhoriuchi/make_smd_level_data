# Aggreate eStats data at the level of SMDs

# Initial settings --------------------------------------------------------

library(tidyverse)

# Load data ---------------------------------------------------------------

estats <- readRDS("output/eStat_data.RDS")
smd2017_weights <- readRDS("output/smd2017_weights.RDS")
smd2021_weights <- readRDS("output/smd2021_weights.RDS")
e2017muncode <- readRDS("output/e2017muncode")
e2021muncode <- readRDS("output/e2021muncode")


d2017 <- smd2017_weights %>% 
  left_join(e2017muncode %>% select(prefecture, munname, muncode, "district" = smd) %>% distinct())

D320113 <- d2017 %>% 
  left_join(estats %>% 
              filter(year == 2016) %>% 
              select(muncode, D320113, A2101) 
  )
