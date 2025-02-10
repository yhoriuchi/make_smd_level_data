# Download municipality-level demographic and local finance data 

# Initial settings --------------------------------------------------------

library(tidyverse)
library(estatapi)

# Add appID ---------------------------------------------------------------
# You need to get an "application ID" by following guide: https://www.e-stat.go.jp/api/api-info/api-guide
# appId <- "xxxxxxx" # Enter manually in Console 

# Make a list of data sets to use -----------------------------------------

stats_IDs <- estat_getStatsList(appId = appId, 
                                searchWord = "社会・人口統計体系") %>% 
  filter(str_detect(STATISTICS_NAME, "市区町村データ 基礎データ（オリジナル）")) %>% 
  select(`@id`) %>% 
  pull()

# Make a list of variables included in the data sets ----------------------

vars_IDs <- NULL

for (i in 1:length(stats_IDs)){
  vars_IDs <- bind_rows(
    vars_IDs,
    estat_getMetaInfo(appId = appId,
                      statsDataId = stats_IDs[i])[["cat01"]] %>% 
      mutate(ID = stats_IDs[i])
  )
}

# Make a set of specific variables to use ---------------------------------

# Check the variable @names using the saved CSV file.
write_csv(vars_IDs, "output/eStats_varlist.csv")

selected <- vars_IDs %>% 
  filter(`@name` %in% c("A1101_総人口",
                        "A1102_日本人人口",
                        "A1414_15歳以上人口",
                        "A2101_住民基本台帳人口（日本人）",
                        "A210102_住民基本台帳人口（日本人）（女）",
                        "B1101_総面積（北方地域及び竹島を除く）",
                        "B1103_可住地面積",
                        "C120110_課税対象所得",
                        "D2201_財政力指数（市町村財政）",
                        "D320101_地方税（市町村財政）",
                        "D320108_地方交付税（市町村財政）",
                        "D320113_国庫支出金（市町村財政）",
                        "D320115_県支出金（市町村財政）",
                        "D320122_地方特例交付金（市町村財政）",
                        "D320406_普通建設事業費（市町村財政）"))

# Get a set of specific variables to use ----------------------------------

get_data <- function(id, year_from = 2015){
  
  out <- estat_getStatsData(appId = appId,
                            statsDataId = selected[id, "ID"],
                            cdCat01     = selected[id, "@code"])
  
  varcode <- out[1, "cat01_code"] # Variable code
  
  out %>% 
    mutate(year = str_extract(`調査年`, "\\d+") %>% as.numeric()) %>% 
    select(area_code, 
           year, 
           value) %>% 
    filter(year >= year_from) %>% 
    set_names(c("muncode", "year", varcode))
  
}

out <- get_data(1)

for (i in 2:nrow(selected)){
  temp <- get_data(i)
  out <- full_join(out, temp, by = c("muncode", "year")) 
}

# Save data ---------------------------------------------------------------

saveRDS(out, "output/eStat_data.RDS")
