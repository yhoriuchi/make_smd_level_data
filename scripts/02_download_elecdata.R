# Download the 2017 and 2021 election data 

# Initial settings --------------------------------------------------------

library(tidyverse)

# Download data -----------------------------------------------------------

smd2017 <- read.csv("https://raw.githubusercontent.com/yhoriuchi/2017-Japanese-Lower-House-Election-Municipality-Level-Data/master/output/smd_data.csv")
smd2021 <- read.csv("https://raw.githubusercontent.com/yhoriuchi/2021-Japanese-Lower-House-Election-Municipality-Level-Data/main/output/smd_data.csv")

pr2017 <- read.csv("https://raw.githubusercontent.com/yhoriuchi/2017-Japanese-Lower-House-Election-Municipality-Level-Data/master/output/pr_data.csv")
pr2021 <- read.csv("https://raw.githubusercontent.com/yhoriuchi/2021-Japanese-Lower-House-Election-Municipality-Level-Data/main/output/pr_data.csv")

# Save data ---------------------------------------------------------------

save.image("output/election_data.RData")

