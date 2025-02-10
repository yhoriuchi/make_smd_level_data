# Merge and wrangle data

# Initial settings --------------------------------------------------------

library(tidyverse)
source("functions/clean_edata.R")

# Load data ---------------------------------------------------------------

load("output/election_data.RData")

mun2017 <- read_rds("output/munlist2017.rds")
mun2021 <- read_rds("output/munlist2021.rds")

# Clean election data -----------------------------------------------------

ele2017 <- clean_edata(smd2017)
ele2021 <- clean_edata(smd2021)

remove(list= ls()[!(ls() %in%c("ele2017", "ele2021", "mun2017", "mun2021"))])

# Special cases -----------------------------------------------------------

ele2017 %>% filter(str_detect(munname, "・|、"))
ele2021 %>% filter(str_detect(munname, "・|、"))

ele2017 %>% filter(str_detect(munname, "中原区|高津区"))
ele2021 %>% filter(str_detect(munname, "中原区|高津区"))

# https://www.city.kawasaki.jp/nakahara/cmsfiles/contents/0000116/116076/ootoF.pdf
# 住民基本台帳２０２０年９月末
# 中原区人口＝259,414
# うち大戸地区（神奈川１８区）＝82,273 (31%)

special_cases_2017 <- tribble(
  ~prefecture, ~munname,  ~district, ~vote_total, 
  "神奈川県", "川崎市中原区", "神奈川県第10区", 69881, # Vote total is actual number
  "神奈川県", "川崎市中原区", "神奈川県第18区", 69881/2, # Vote total is an estimate
  "神奈川県", "川崎市高津区", "神奈川県第18区", 126979 - 69881/2 # Vote total is an estimate
)

special_cases_2021 <- tribble(
  ~prefecture, ~munname,  ~district, ~vote_total, 
  "神奈川県", "川崎市中原区", "神奈川県第10区", 82135, # Vote total is actual number
  "神奈川県", "川崎市中原区", "神奈川県第18区", 82135/2, # Vote total is an estimate
  "神奈川県", "川崎市高津区", "神奈川県第18区", 147267 - 82135/2 # Vote total is an estimate
)

ele2017_added <- bind_rows(ele2017, special_cases_2017)
ele2021_added <- bind_rows(ele2021, special_cases_2021)

# Add municipality codes and weights  -------------------------------------

e2017muncode <- ele2017_added %>% 
  filter(munname != "川崎市中原区・高津区") %>% 
  full_join(mun2017, by = c("prefecture", "munname")) %>% 
  group_by(prefecture, munname) %>% 
  mutate(weight = vote_total / sum(vote_total)) %>% 
  ungroup()

e2021muncode <- ele2021_added %>% 
  filter(munname != "川崎市中原区・高津区") %>% 
  full_join(mun2021, by = c("prefecture", "munname")) %>% 
  group_by(prefecture, munname) %>% 
  mutate(weight = vote_total / sum(vote_total)) %>% 
  ungroup()

# Check that there is no missing values
colSums(is.na(e2017muncode))
colSums(is.na(e2021muncode))

# Save data ---------------------------------------------------------------

saveRDS(e2017muncode, "output/e2017muncode.RDS")
saveRDS(e2021muncode, "output/e2021muncode.RDS")

