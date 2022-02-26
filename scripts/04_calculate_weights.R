# Add district codes to eStats data

# Initial settings --------------------------------------------------------

library(tidyverse)

# Load data ---------------------------------------------------------------

load("output/election_data.RData")
e2017muncode <- readRDS("output/e2017muncode")
e2021muncode <- readRDS("output/e2021muncode")

# Calculate weights -------------------------------------------------------

clean_edata <- function(data){
  
  out <- data %>% 
    rename(munname = municipality) %>% 
    mutate(prefecture = str_remove_all(district, "第.+") %>% str_trim()) %>% 
    filter(!str_detect(munname, "合計")) %>% 
    mutate(munname = str_remove_all(munname, " +|　+"),
           munname = str_remove_all(munname, "\\(.+\\)"),
           munname = str_remove_all(munname, "\\（.+\\）"),
           munname = str_remove_all(munname, "\\n"),
           munname = str_remove_all(munname, "第"),
           munname = str_remove_all(munname, "\\d区$"),
           munname = str_remove_all(munname, "１|２|1|2"),
           munname = str_replace_all(munname, "鯵ヶ沢町", "鰺ヶ沢町"),
           munname = str_replace_all(munname, "袖ヶ浦市", "袖ケ浦市"),
           munname = str_replace_all(munname, "横浜市保土ヶ谷区", "横浜市保土ケ谷区"),
           munname = str_replace_all(munname, "栗国村", "粟国村"),
           munname = str_replace_all(munname, "桧枝岐村", "檜枝岐村"),
           munname = str_replace_all(munname, "始良市", "姶良市"),
           munname = str_replace_all(munname, "南薩摩市", "南さつま市"),
           munname = str_replace_all(munname, "横浜市都築区", "横浜市都筑区"),
           munname = ifelse(munname == "太白区", "仙台市太白区", munname),
           munname = ifelse(prefecture == "鹿児島県" & munname == "大崎市", "大崎町", munname),
           munname = ifelse(prefecture == "鹿児島県" & munname == "清水町", "湧水町", munname),
           munname = ifelse(prefecture == "東京都" & munname == "稲敷市", "稲城市", munname),
           munname = str_trim(munname)) %>% 
    
    # Error in the original data: NHKのサイトで間違いを確認
    mutate(munname = ifelse(district == "大阪府第１区" & munname == "大阪市生野区", "大阪市東成区", munname)) %>% 
    
    # https://ja.wikipedia.org/wiki/%E6%96%B0%E6%BD%9F%E7%9C%8C%E7%AC%AC4%E5%8C%BA
    # 新潟４区の北区（旧横越町域）は有権者数が僅少とのこと
    mutate(munname = ifelse(district == "新潟県第４区" & munname == "新潟市江南区、新潟市北区", "新潟市江南区", munname)) %>% 
    
    group_by(prefecture, munname, district) %>% 
    summarize(vote_total = sum(votes), .groups = "drop") 
    
}

# Special case ------------------------------------------------------------

# https://www.city.kawasaki.jp/nakahara/cmsfiles/contents/0000116/116076/ootoF.pdf
# 住民基本台帳２０２０年９月末
# 中原区人口＝259,414
# うち大戸地区（神奈川１８区）＝82,273 (31%)

special_cases <- tribble(
  ~prefecture, ~munname,  ~district, ~vote_total, 
  "神奈川県", "川崎市中原区", "神奈川県第10区", 69881, # Vote total is actual number
  "神奈川県", "川崎市中原区", "神奈川県第18区", 69881/2, # Vote total is an estimate
  "神奈川県", "川崎市高津区", "神奈川県第18区", 126979 - 69881/2 # Vote total is an estimate
)

# Weight for the 2017 election  -------------------------------------------

smd2017_weights <- bind_rows(
  clean_edata(smd2017) %>% filter(!(munname == "川崎市中原区・高津区" & district == "神奈川県第18区")),
  special_cases
) %>% 
  group_by(prefecture, munname) %>% 
  mutate(weight = vote_total / sum(vote_total)) %>% 
  ungroup()

# check data
check2017 <- smd2017_weights %>% 
  filter(weight != 1) %>% 
  arrange(prefecture, munname)

# Weight for the 2021 election  -------------------------------------------

smd2021_weights <- bind_rows(
  clean_edata(smd2021) %>% filter(!(munname == "川崎市中原区・高津区" & district == "神奈川県第18区")),
  special_cases
) %>% 
  group_by(prefecture, munname) %>% 
  mutate(weight = vote_total / sum(vote_total)) %>% 
  ungroup()

# check data
check2021 <- smd2021_weights %>% 
  filter(weight != 1) %>% 
  arrange(prefecture, munname)
  
# Save data ---------------------------------------------------------------

saveRDS(smd2017_weights, "output/smd2017_weights.RDS")
saveRDS(smd2021_weights, "output/smd2021_weights.RDS")














