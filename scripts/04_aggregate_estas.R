# Aggreate eStats data at the level of SMDs

# Initial settings --------------------------------------------------------

library(tidyverse)
library(readxl)

# Load data ---------------------------------------------------------------

estats <- readRDS("output/eStat_data.RDS")
e2017muncode <- readRDS("output/e2017muncode")
e2021muncode <- readRDS("output/e2021muncode")

# Municipality codes and names --------------------------------------------

muncode <- bind_rows(
  read_xlsx("data/000730858.xlsx", sheet = 1),
  read_xlsx("data/000730858.xlsx", sheet = 2)
) %>%
  select(1:3) %>%
  set_names(c("muncode", "prefecture", "munname")) %>%
  mutate(muncode = str_sub(muncode, 1L, 5L)) %>%
  distinct() %>%
  filter(!is.na(munname))

# Municipalities that we need to pay close attention to:

# 2018.10.01: 40305 (那珂川町) --> 40231 (那珂川市)
# 2016.10.10: 04423（富谷町）--> 04216（富谷市）

# Add municipality names to eStats data -----------------------------------

estats_rev <- estats %>% 
  filter(year %in% c(2015, 2018)) %>% 
  full_join(muncode) %>% 
  filter(!muncode == 13100) %>% # 特別区合計
  mutate(prefecture = case_when(muncode == "04423" ~ "宮城県",
                                muncode == "40305" ~ "福岡県",
                                TRUE ~ prefecture),
         munname = case_when(muncode == "04423" ~ "富谷町",
                             muncode == "40305" ~ "那珂川町",
                             TRUE ~ munname)) %>% 
  select(muncode, prefecture, munname, everything()) %>% 
  # ロシア連邦による占領・統治下
  # 北方領土の泊村（国後郡）は、古宇郡の泊村と異なるので注意
  filter(!(munname %in% c("色丹村", "留夜別村", "留別村", "紗那村", "蘂取村") | muncode == "01696"))

colSums(is.na(estats_rev[1:4]))

# Calculate weights -------------------------------------------------------

weights <- estats_rev %>% 
  filter(year %in% c(2015, 2018)) %>% 
  select(year, muncode, prefecture, munname, A2101) %>% 
  mutate(muncode_num = as.integer(muncode)) %>% 
  mutate(id = case_when(muncode_num >=  1100 & muncode_num <  1200 ~ 1,
                        muncode_num >=  4100 & muncode_num <  4200 ~ 2,
                        muncode_num >= 11100 & muncode_num < 11200 ~ 3,
                        muncode_num >= 12100 & muncode_num < 12200 ~ 4,
                        muncode_num >= 13100 & muncode_num < 13200 ~ 5,
                        muncode_num >= 14100 & muncode_num < 14200 ~ 6,
                        muncode_num >= 14130 & muncode_num < 14140 ~ 7,
                        muncode_num >= 14150 & muncode_num < 14200 ~ 8,
                        muncode_num >= 15100 & muncode_num < 15200 ~ 9,
                        muncode_num >= 22100 & muncode_num < 22130 ~ 10,
                        muncode_num >= 22130 & muncode_num < 22200 ~ 11,
                        muncode_num >= 23100 & muncode_num < 23200 ~ 12,
                        muncode_num >= 26100 & muncode_num < 26200 ~ 13,
                        muncode_num >= 27100 & muncode_num < 27140 ~ 14,
                        muncode_num >= 27140 & muncode_num < 27200 ~ 15,
                        muncode_num >= 28100 & muncode_num < 28200 ~ 16,
                        muncode_num >= 33100 & muncode_num < 33200 ~ 17,
                        muncode_num >= 34100 & muncode_num < 34200 ~ 18,
                        muncode_num >= 40100 & muncode_num < 40130 ~ 19,
                        muncode_num >= 40130 & muncode_num < 40200 ~ 20,
                        muncode_num >= 43100 & muncode_num < 43200 ~ 21),
         id = ifelse(is.na(id), muncode_num, id)) %>% 
  group_by(year, id) %>% 
  mutate(max_pop = max(A2101), 
         weight = A2101 / max_pop) %>% 
  ungroup() %>% 
  select(year, muncode, id, weight, prefecture, munname) %>% 
  arrange(muncode)

estats_rev2 <- estats_rev %>% 
  left_join(weights) %>% 
  relocate(weight, .after = munname) %>% 
  relocate(id, .after = weight)

# Fill in missing values --------------------------------------------------

nodata <- estats_rev2 %>%
  map_df(function(x) sum(is.na(x))) %>%
  gather(variable, num_NAs) %>%
  filter(num_NAs != 0) %>% 
  as.data.frame()

nodata

estats_rev3 <- estats_rev2 %>% 
  select(1:6,  A2101, A210102, A2201, B1101, B1103, C120110, contains("D")) %>% 
  
  # Special case
  mutate(temp = case_when(prefecture == "福岡県" & munname %in% c("那珂川市", "那珂川町") & year == 2018 ~ 1,
                          TRUE ~ NA_real_)) %>% 
  mutate(across(A2101:D320406, ~ifelse(!is.na(temp), max(.x, na.rm = TRUE), .x))) %>% 
  filter(!munname == "那珂川町") %>% 
  select(-temp) %>% 

  group_by(id, year) %>% 
  fill(D2201) %>% 
  mutate(across(c(C120110, D320101, D320108, D320113, D320115, D320122, D320406), ~max(.x, na.rm = TRUE) * weight)) %>% 
  ungroup() %>% 
  
  # Assume 可住地面積 = 総面積 if is.na(可住地面積); 相模市, 熊本市
  mutate(B1103 = ifelse(is.na(B1103), B1101, B1103)) %>% 
  
  # Assign 3 to 財政力指数 if 特別区
  mutate(D2201 = ifelse(is.na(D2201), 3, D2201))

# Check missing valeus
colSums(is.na(estats_rev3))

# Finally aggregate eStats at SMD level -----------------------------------

remove(list= ls()[!(ls() %in%c("estats_rev3", "e2017muncode", "e2021muncode"))])



