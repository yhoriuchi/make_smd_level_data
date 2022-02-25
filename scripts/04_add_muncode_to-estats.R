# Add district codes to eStats data

# Initial settings --------------------------------------------------------

library(tidyverse)

# Load data ---------------------------------------------------------------

estats <- readRDS("output/eStat_data.RDS")
e2017muncode <- readRDS("output/e2017muncode")
e2021muncode <- readRDS("output/e2021muncode")
