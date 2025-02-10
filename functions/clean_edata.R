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