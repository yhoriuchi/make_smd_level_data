# Merge and wrangle data

# Initial settings --------------------------------------------------------

library(tidyverse)

# Load data ---------------------------------------------------------------

load("output/election_data.RData")

# List of municipalities as of the election dates
# Source: https://www.e-stat.go.jp/municipalities/cities/areacode
muncode2017 <- read_csv("data/FEA_hyoujun-20220225015750.csv", locale = locale(encoding = "shift-jis"))
muncode2021 <- read_csv("data/FEA_hyoujun-20220225015648.csv", locale = locale(encoding = "shift-jis"))

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

# Clean election data -----------------------------------------------------

clean_edata <- function(data){
  
  out <- data %>% 
    mutate(prefecture = str_remove_all(district, "第.+") %>% str_trim(),
           
           # Error in the original data: NHKのサイトで間違いを確認
           municipality = ifelse(district == "大阪府第１区" & municipality == "大阪市生野区", "大阪市東成区", municipality),
           
           # https://ja.wikipedia.org/wiki/%E6%96%B0%E6%BD%9F%E7%9C%8C%E7%AC%AC4%E5%8C%BA
           # 新潟４区の北区（旧横越町域）は有権者数が僅少とのこと
           municipality = ifelse(str_detect(municipality, "新潟市江南区") & 
                                   str_detect(municipality, "新潟市北区"), "新潟市江南区", municipality)
           
    ) %>% 
    select(prefecture, 
           "munname" = municipality, 
           district, 
           votes) %>% 
    filter(!str_detect(munname, "合計")) %>% 
    
    # Cleaning of municipality names
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
           munname = str_trim(munname)
    ) %>% 
    
    # Aggregate votes 
    group_by(prefecture, munname, district) %>% 
    summarise(vote_total = sum(votes), .groups = "drop")

}

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
saveRDS(e2017muncode, "output/e2017muncode.RDS")

