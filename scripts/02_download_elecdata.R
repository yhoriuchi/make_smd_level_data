# Download the 2017, 2021, 2024 election data 

# Initial settings --------------------------------------------------------

library(tidyverse)

# Download data -----------------------------------------------------------

smd2017 <- read.csv("https://raw.githubusercontent.com/yhoriuchi/jelec2017lh/refs/heads/master/output/smd_data.csv")
smd2021 <- read.csv("https://raw.githubusercontent.com/yhoriuchi/jelec2021lh/refs/heads/main/output/smd_data.csv")
smd2024 <- read.csv("https://raw.githubusercontent.com/yhoriuchi/jelec2024lh/refs/heads/main/output/smd_data.csv")

pr2017 <- read.csv("https://raw.githubusercontent.com/yhoriuchi/jelec2017lh/refs/heads/master/output/pr_data.csv")
pr2021 <- read.csv("https://raw.githubusercontent.com/yhoriuchi/jelec2021lh/refs/heads/main/output/pr_data.csv")
pr2024 <- read.csv("https://raw.githubusercontent.com/yhoriuchi/jelec2024lh/refs/heads/main/output/pr_data.csv")

# Save data ---------------------------------------------------------------

save.image("output/election_data.RData")

