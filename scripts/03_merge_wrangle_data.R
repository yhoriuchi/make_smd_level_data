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
    filter(!(munname %in% c("特別区部", "色丹村", "留夜別村", "留別村", "紗那村", "蘂取村")))
  
}

mun2017 <- clean_muncode(muncode2017)
mun2021 <- clean_muncode(muncode2021)

# Clean election data -----------------------------------------------------

clean_edata <- function(data, type){
  
  if (type == "smd"){
    
    out <- data %>% 
      mutate(prefecture = str_remove_all(district, "第.+") %>% str_trim()) 
    
  } else if (type == "pr"){
    
    out <- data %>% 
      mutate(prefecture = district) 
    
  } else {
    
    stop("type should be either smd or pr")
  }
  
  out <- out %>% 
    select(prefecture, 
           "munname" = municipality, 
           district) %>% 
    distinct() %>% 
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
           munname = str_trim(munname)
    ) 
  
    names(out)[3] <- type
    
    return(out)

}

ele2017 <- full_join(
  clean_edata(smd2017, "smd"), 
  clean_edata(pr2017, "pr")
) %>% 
  mutate(pr = ifelse(is.na(pr) & !is.na(prefecture), prefecture, pr))

pr20201_rev <- pr2021 %>% 
  mutate(municipality = ifelse(district == "北海道" & str_detect(municipality, "区"), 
                               paste0("札幌市", municipality), 
                               municipality))

ele2021 <- full_join(
  clean_edata(smd2021, "smd"), 
  clean_edata(pr20201_rev, "pr")
) %>% 
  mutate(pr = ifelse(is.na(pr) & !is.na(prefecture), prefecture, pr))

# Special cases -----------------------------------------------------------

special_cases <- tribble(
  ~prefecture, ~munname,  ~smd, ~pr,
  "神奈川県", "川崎市中原区", "神奈川県第10区", "神奈川県",
  "神奈川県", "川崎市中原区", "神奈川県第18区", "神奈川県",
  "神奈川県", "川崎市高津区", "神奈川県第18区", "神奈川県",
  "神奈川県", "川崎市宮前区", "神奈川県第18区", "神奈川県",
  "神奈川県", "川崎市多摩区", "神奈川県第18区", "神奈川県",
  "神奈川県", "川崎市多摩区", "神奈川県第９区", "神奈川県",
  "新潟県", "新潟市江南区", "新潟県第１区", "新潟県",
  "新潟県", "新潟市江南区", "新潟県第４区", "新潟県",
  "新潟県", "新潟市北区", "新潟県第４区", "新潟県"
)

ele2017_added <- bind_rows(
  ele2017,
  special_cases
) %>% 
  drop_na() %>% 
  filter(!str_detect(munname, "・|、")) 

ele2021_added <- bind_rows(
  ele2021,
  special_cases
) %>% 
  drop_na() %>% 
  filter(!str_detect(munname, "・|、"))

# Merge data --------------------------------------------------------------

e2017muncode <- ele2017_added %>% 
  full_join(mun2017, by = c("prefecture", "munname")) %>% 
  # fix a strange case of missing
  mutate(smd = ifelse(munname == "大阪市東成区", "大阪府第１区", smd), 
         pr  = ifelse(munname == "大阪市東成区", "大阪府", pr))

e2017muncode %>% 
  filter(is.na(prefecture) | is.na(munname) | is.na(smd) | is.na(pr) | is.na(muncode))

e2021muncode <- ele2021_added %>% 
  full_join(mun2021, by = c("prefecture", "munname"))

e2021muncode %>% 
  filter(is.na(prefecture) | is.na(munname) | is.na(smd) | is.na(pr) | is.na(muncode))

# Save data ---------------------------------------------------------------

saveRDS(e2017muncode, "output/e2017muncode")
saveRDS(e2021muncode, "output/e2021muncode")

