# Add appID ---------------------------------------------------------------
# You need to get an "application ID" by following guide: https://www.e-stat.go.jp/api/api-info/api-guide
# appId <- "xxxxxxx" # Enter manually in Console 

files <- list.files("scripts")
for (i in 1:length(files)){
  source(paste0("scripts/", files[i]))
}
