# Make a list of municipalities as of the election date

# Initial settings --------------------------------------------------------

library(tidyverse)

# Load data ---------------------------------------------------------------

# List of municipalities as of the election dates
# Source: https://www.e-stat.go.jp/municipalities/cities/areacode
muncode2017 <- read_csv("data/FEA_hyoujun-20220225015750.csv", locale = locale(encoding = "shift-jis"))
muncode2021 <- read_csv("data/FEA_hyoujun-20220225015648.csv", locale = locale(encoding = "shift-jis"))
muncode2024 <- read_csv("data/FEA_hyoujun-20250210024619.csv", locale = locale(encoding = "shift-jis"))

# Clean municipality code data --------------------------------------------

clean_muncode <- function(data){
  
  data %>% 
    select("prefecture" = `都道府県`,
           "muncode" = `標準地域コード`,
           "name1" = `政令市･郡･支庁･振興局等`,
           "name2" = `市区町村`) %>% 
    mutate(name1 = ifelse(str_detect(name1, "郡|支庁|振興局"), NA, name1),
           munname = ifelse(!is.na(name1), paste0(name1, name2), name2)) %>% 
    filter(!(!is.na(name1) & is.na(name2))) %>% 
    select(-name1, -name2) %>% 
    filter(!(munname %in% c("特別区部", "色丹村", "留夜別村", "留別村", "紗那村", "蘂取村"))) %>% 
    # 北方領土の泊村（国後郡）は、古宇郡の泊村と異なるので注意
    filter(!muncode == "01696")
  
}

mun2017 <- clean_muncode(muncode2017)
mun2021 <- clean_muncode(muncode2021)
mun2024 <- clean_muncode(muncode2024)

# Save data ---------------------------------------------------------------

write_rds(mun2017, "output/munlist2017.rds")
write_rds(mun2021, "output/munlist2021.rds")
write_rds(mun2024, "output/munlist2024.rds")
