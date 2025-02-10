# Aggreate eStats data at the level of SMDs

# Initial settings --------------------------------------------------------

library(tidyverse)
library(readxl)

# Load data ---------------------------------------------------------------

estats <- readRDS("output/eStat_data.RDS")

e2017muncode <- readRDS("output/e2017muncode.rds") %>% mutate(munname = ifelse(munname == "篠山市", "丹波篠山市", munname))
e2021muncode <- readRDS("output/e2021muncode.rds")
e2024muncode <- readRDS("output/e2024muncode.rds")

# Municipality codes and names --------------------------------------------

read_muncode <- function(.file){
  
  bind_rows(
    read_xlsx(.file, sheet = 1),
    read_xlsx(.file, sheet = 2)
  ) %>%
    select(1:3) %>%
    set_names(c("muncode", "prefecture", "munname")) %>%
    mutate(muncode = str_sub(muncode, 1L, 5L)) %>%
    distinct() %>%
    filter(!is.na(munname)) %>% 
    mutate(munname = case_when(
      # 公式名の漢字を使用
      munname == "梼原町" ~ "檮原町", 
      # 名称変更
      # 2019.05.01: 282219（篠山市） --> 282219（丹波篠山市）
      munname == "篠山市" ~ "丹波篠山市", 
      TRUE ~ munname))
  
}

# For 2017 (using a list of municipalities as of Jan 1, 2019)
muncode2017 <- read_muncode("data/000730858.xlsx")

# For 2021 (using a list of municipalities as of Jan 1, 2019)
muncode2021 <- read_muncode("data/000730858.xlsx")

# For 2024 (using a list of municipalities as of Jan 1, 2024)
muncode2024 <- read_muncode("data/000925835.xlsx")

# Check the name correspondence between files -----------------------------

anti_join(e2017muncode, muncode2017, by = "munname")
anti_join(e2021muncode, muncode2021, by = "munname")
anti_join(e2024muncode, muncode2024, by = "munname")

# Special cases -----------------------------------------------------------
# 平成28年（2016年）以降の市政以降
# 2016.10.10: 04423（富谷町）--> 04216（富谷市）
# 2018.10.01: 40305 (那珂川町) --> 40231 (那珂川市)

# check
estats %>% 
  filter(muncode %in% c("04423", "04216")) %>% 
  filter(year %in% c(2016))

estats %>% 
  filter(muncode %in% c("40305", "40231")) %>% 
  filter(year %in% c(2018))

special_cases <- estats %>% 
  filter(muncode %in% c("04423", "04216", "40305", "40231")) %>% 
  mutate(muncode2 = case_when(muncode == "04423" ~ "04216",
                              muncode == "40305" ~ "40231",
                              TRUE ~ muncode), .after = muncode) %>% 
  group_by(muncode2, year) %>% 
  fill(everything(), .direction = "updown") %>% 
  ungroup() 

# Check again
special_cases %>% 
  filter(muncode %in% c("04423", "04216")) %>% 
  filter(year %in% c(2016))

special_cases %>% 
  filter(muncode %in% c("40305", "40231")) %>% 
  filter(year %in% c(2018))

special_cases_v2 <- special_cases %>% 
  filter(!(muncode == "04423" & year == 2016)) %>% 
  filter(!(muncode == "40305" & year == 2018)) %>% 
  select(-muncode) %>% 
  rename("muncode" = muncode2)

estats_rev <- 
  bind_rows(
    # All municipalities excluding two special cases
    estats %>% filter(!(muncode %in% c("04423", "04216", "40305", "40231"))),
    # Keep one observation for each special case (for each year)
    special_cases_v2
  )

# Initial cleaning of eStats ----------------------------------------------

estats_rev2 <- estats_rev %>% 
  
  # Remove the total for Tokyo 23 wards
  filter(!muncode == 13100) %>% 
  
  # use the most recent data (if missing, e.g., the Census data)
  group_by(muncode) %>% 
  arrange(muncode, year) %>% 
  fill(everything(), .direction = "down") %>% 
  ungroup() %>% 
  
  # Add weights for cities designated by ordinance
  mutate(muncode_num = as.integer(muncode)) %>% 
  mutate(id = case_when(muncode_num >=  1100 & muncode_num <  1200 ~ 1, # Sapporo
                        muncode_num >=  4100 & muncode_num <  4200 ~ 2, # Sendai
                        muncode_num >= 11100 & muncode_num < 11200 ~ 3, # Saitama
                        muncode_num >= 12100 & muncode_num < 12200 ~ 4, # Chiba
                        muncode_num >= 13100 & muncode_num < 13200 ~ 5, # Tokyo
                        muncode_num >= 14100 & muncode_num < 14130 ~ 6, # Yokohama
                        muncode_num >= 14130 & muncode_num < 14150 ~ 7, # Kawasaki
                        muncode_num >= 14150 & muncode_num < 14200 ~ 8, # Sagamihara
                        muncode_num >= 15100 & muncode_num < 15200 ~ 9, # Niigata
                        muncode_num >= 22100 & muncode_num < 22130 ~ 10, # Shizuoka
                        muncode_num >= 22130 & muncode_num < 22200 ~ 11, # Hamamatsu
                        muncode_num >= 23100 & muncode_num < 23200 ~ 12, # Nagoya
                        muncode_num >= 26100 & muncode_num < 26200 ~ 13, # Kyoto
                        muncode_num >= 27100 & muncode_num < 27140 ~ 14, # Osaka
                        muncode_num >= 27140 & muncode_num < 27200 ~ 15, # Hyogo
                        muncode_num >= 28100 & muncode_num < 28200 ~ 16, # Kobe
                        muncode_num >= 33100 & muncode_num < 33200 ~ 17, # Okayama
                        muncode_num >= 34100 & muncode_num < 34200 ~ 18, # Hiroshima
                        muncode_num >= 40100 & muncode_num < 40130 ~ 19, # Kita-Kyushu
                        muncode_num >= 40130 & muncode_num < 40200 ~ 20, # Fukuoka
                        muncode_num >= 43100 & muncode_num < 43200 ~ 21), # Kumamoto
         id = ifelse(is.na(id), muncode_num, id)) %>% 
  group_by(year, id) %>% 
  mutate(max_pop = max(A2101), 
         weight = A2101 / max_pop) %>% 
  ungroup() 

# Fill-in the missing values for "ku" -------------------------------------

# Focus only on 20 cities designated by ordinance
special_cities <- estats_rev2 %>% 
  filter(id %in% c(1:4, 6:21)) %>% 
  
  # arrange by year
  arrange(year, muncode) %>% 
  
  # fill
  group_by(id) %>% 
  fill(C120110:D320406) %>% 
  ungroup() %>% 
  
  # Calculate the weighted values
  mutate(across(c(C120110, D320101:D320406), ~ .x * weight))

# Focus only on 24 wards in Tokyo
tokyo_23wards <- estats_rev2 %>% 
  filter(id == 5)

# Re-wrangle estats -------------------------------------------------------

temp1 <- estats_rev2 %>% filter(!(id %in% 1:21))
temp2 <- special_cities
temp3 <- tokyo_23wards

estats_rev3 <- bind_rows(temp1, temp2, temp3) %>% 
  
  # Remove unnecessary variables
  select(-id, -max_pop, -weight, -muncode_num) %>% 
  
  # Assume 可住地面積 = 総面積 if is.na(可住地面積); 相模市, 熊本市
  mutate(B1103 = ifelse(is.na(B1103), B1101, B1103)) %>% 
  
  # Assign 3 to 財政力指数 if 特別区
  mutate(D2201 = ifelse(is.na(D2201), 3, D2201))

# Check NA
colSums(is.na(estats_rev3))

# Check duplicates
estats_rev2 %>% 
  count(muncode, year) %>% 
  filter(n != 1)

# Further adjustment only for the 2024 election ---------------------------
# 注意：2024（令和6）年1月1日、浜松市の行政区が7区から3区に変更
# https://www.city.hamamatsu.shizuoka.jp/kikaku/kuseido/index.html
# 厳密には、北区の一部は中央区に所属

estats_rev3_2024 <- estats_rev3 %>% 
  filter(year == 2022) %>% 
  mutate(muncode2 = case_when(muncode == "22131" ~ "22138", # 浜松市中区 -> 浜松市中央区
                              muncode == "22132" ~ "22138", # 浜松市東区 -> 浜松市中央区
                              muncode == "22133" ~ "22138", # 浜松市西区 -> 浜松市中央区
                              muncode == "22134" ~ "22138", # 浜松市南区 -> 浜松市中央区
                              muncode == "22135" ~ "22139", # 浜松市北区 　-> 浜松市浜名区
                              muncode == "22136" ~ "22139", # 浜松市浜北区 -> 浜松市浜名区
                              muncode == "22137" ~ "22140", # 浜松市天竜区 -> 浜松市天竜区
                              TRUE ~ muncode), .before = 0L) %>% 
  pivot_longer(cols = 4:ncol(.)) %>% 
  group_by(muncode2, year, name) %>% 
  summarise(total = sum(value),
            mean = mean(value), .groups = "drop") %>% 
  # Use mean for 財政力指数 and total for others
  mutate(value = ifelse(name == "D2201", mean, total)) %>% 
  rename("muncode" = muncode2) %>% 
  select(-total, -mean) %>% 
  pivot_wider(names_from = name, values_from = value)

# Check duplicates
estats_rev3_2024 %>% 
  count(muncode, year) %>% 
  filter(n != 1)

# Finally aggregate eStats at SMD level -----------------------------------

aggregate_data <- function(.dataframe, .year){
  .dataframe %>% 
    select(prefecture, district, weight, vote_total, vote_total, A1101:D320406) %>% 
    pivot_longer(cols = 5:ncol(.)) %>% 
    group_by(district, name) %>% 
    summarise(total = sum(value),
              mean = mean(value), .groups = "drop") %>% 
    # Use mean for 財政力指数 and total for others
    mutate(value = ifelse(name == "D2201", mean, total)) %>% 
    select(-total, -mean) %>% 
    pivot_wider(names_from = name, values_from = value) %>% 
    mutate(year = .year, .before = 0L)
}

estats_smd2017 <- e2017muncode %>% 
  mutate(muncode = ifelse(muncode == "40305", "40231", muncode)) %>% 
  left_join(estats_rev3 %>% filter(year == 2016), by = "muncode") %>% 
  aggregate_data(2017) 

estats_smd2021 <- e2021muncode %>% 
  left_join(estats_rev3 %>% filter(year == 2020), by = "muncode") %>% 
  aggregate_data(2021)

estats_smd2024 <- e2024muncode %>% 
  left_join(estats_rev3_2024) %>% 
  aggregate_data(2024)

colSums(is.na(estats_smd2017))
colSums(is.na(estats_smd2021))
colSums(is.na(estats_smd2024))

estats_aggregated <- bind_rows(
  estats_smd2017, 
  estats_smd2021, 
  estats_smd2024,
)

colSums(is.na(estats_aggregated))

# Save data ---------------------------------------------------------------

# delete the "output" folder
unlink("output", recursive = TRUE)
dir.create("output")

write_csv(estats_aggregated, "output/estats_aggregated.CSV")
saveRDS(estats_aggregated, "output/estats_aggregated.RDS")

