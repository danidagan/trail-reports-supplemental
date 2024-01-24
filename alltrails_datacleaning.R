## CREATED BY: Dani Dagan ##
## Cite as: TBD ##

library(tidyverse)

# README:
# This script takes a folder of CSVs that have been copy-pasted from
# AllTrails.com and produces a dataframe where each row is a review and
# columns for metadata like trail name, trail star rating, review activity.
# Procedures for collecting raw data are in this github repo: QAQC_protocol.docx
# This code works on AllTrails pages on desktop as of 2023 _only if_ the window
# is sized such that the reviews occupy the entire window width. If a sidebar
# is open on the right, shrink the window horizontally until it disappears.
# This script may break if the website changes.

## INFORMATION PARSING / FEATURE ENGINEERING ##

# paths and setup

files_location <- "<string>" # folder path string for folder where raw CSVs from AllTrails live
file_names <- sort(list.files(files_location))
clean_folder <-  "<string>" # folder path string for folder where clean CSVs for each trail will output
qc_folder <- "<string>" # folder path string for folder to dump a progress log

# the bulk of the operations occur in this loop which takes a folder of raw CSVs and outputs clean CSVs into another folder
for(i in file_names){
  
  # import raw CSVs
  
  trail_url <- i
  df <- read.csv(paste0(files_location,trail_url),header=FALSE,col.names=c("raw"), colClasses = "character")
  link <- df[1,1]
  
  # clear junk
  
  delete_index <- which(grepl("Showing results \\d+ - \\d+ of \\d+", df$raw))
  df <- df[seq_len(min(delete_index, nrow(df))), , drop = FALSE]
  
  delete_regex <- "(^\\d$|^ExploreCommunity$|^Saved$|^Shop$|^Help$|^Dani$|^United States of America$|^Print/PDF map$|^Share$|^More$|^Show more$|^Preview trail$|^Jobs$|^About$|^Company$|^Press$|^Community$|^Monday$|^Tuesday$|^Wednesday$|^Thursday$|^Friday$|^Saturday$|^Sunday$|First to review|^.*\\d+°\\s*/\\s*\\d+°\\s*F$)" # replace Dani term with the first name from the AllTrails login information
  df <- df %>% filter(!str_detect(df$raw,delete_regex))
  
  # create a column with user names copied into it
  
  df$user <- 0
  
  for (i in 2:nrow(df)) {
    if (gsub(" ", "", df$raw[i]) == gsub(" ", "", df$raw[i - 1])) {
      df$user[i] <- 1
      df$user[i-1] <- 1
    }}
  
  # create columns with review month and activity copied into them
  
  months_regex <- "(January|February|March|April|May|June|July|August|September|October|November|December).*•.*"
  df$month <- ""
  df$activity <- ""
  for(i in 1:nrow(df)){
    if(df$user[i-1] == 1 && grepl(pattern=months_regex,x=df$raw[i])){
      df$month[i] <- as.character(gsub("^([A-Za-z]+).*","\\1",df$raw[i]),perl=TRUE)
      df$activity[i] <- as.character(gsub(".*•(.*)","\\1",df$raw[i]))
    }}
  
  # create column that classifies the type of information in each row of the raw data column
  
  df$type <- ""
  geo_regex <- "^Length.*Elevation gain.*Route type"
  rating_regex <- "^([A-Za-z]+)•(\\d+\\.\\d+).*"
  for (i in 2:nrow(df)) {
    if(grepl(pattern=geo_regex,x=df$raw[i])){df$type[i] <- "geo"
    elev_change <- as.character(gsub(".*Elevation gain(.*?) ft(.*)","\\1",df$raw[i]))}
    else if(grepl(pattern=geo_regex,x=df$raw[i-1])){df$type[i] <- ""}
    else if(grepl(pattern=months_regex,x=df$raw[i])){df$type[i] <- "review_meta"}
    else if(grepl(pattern=months_regex,x=df$activity[i])){df$type[i] <- "activity"}
    else if(df$user[i] == 1){df$type[i] <- "user"}
    else if(grepl(pattern=".*›.*",x=df$raw[i])){df$type[i] <- "location"}
    else if(grepl(pattern=rating_regex,x=df$raw[i])){df$type[i] <- "rating"
    difficulty <- as.character(gsub("(.*)•.*","\\1",df$raw[i]))
    total_rates <- as.character(gsub(".*\\((\\d+)\\)","\\1",df$raw[i]))}
  }
  
  for (i in 1:nrow(df)) {
    if (!is.na(df$type[i]) && df$type[i] == "review_meta" && !is.na(df$type[i+2]) && df$type[i+2] == "user" && df$type[i+1] != "user") {
      df$type[i+1] <- "review"}
  }
  
  for (i in 1:nrow(df)) {
    if (!is.na(df$type[i]) && df$type[i] == "review_meta" && !is.na(df$type[i+3]) && df$type[i+3] == "user" && df$type[i+1] != "user") {
      df$type[i+1] <- "review"}
  }
  
  for (i in 1:nrow(df)) {
    if (!is.na(df$type[i]) && df$type[i] == "review_meta" && !is.na(df$type[i+4]) && df$type[i+4] == "user" && df$type[i+1] != "user") {
      df$type[i+1] <- "review"}
  }
  
  for (i in 1:nrow(df)) {
    if (!is.na(df$type[i]) && df$type[i] == "review_meta" && !is.na(df$type[i+5]) && df$type[i+5] == "user" && df$type[i+1] != "user") {
      df$type[i+1] <- "review"}
  }
  
  # grabs relevant text from raw data column and copies it into the correct column
  
  df$data <- ""
  
  for (i in 1:nrow(df)){
    if(df$type[i] == "review" && !is.na(df$type[i+5]) && df$type[i+5] == "user"){df$data[i] <- paste(df$raw[i],df$raw[i+1],df$raw[i+2],df$raw[i+3])}
    else if(df$type[i] == "review" && !is.na(df$type[i+4]) && df$type[i+4] == "user"){df$data[i] <- paste(df$raw[i],df$raw[i+1],df$raw[i+2])}
    else if(df$type[i] == "review" && !is.na(df$type[i+3]) && df$type[i+3] == "user"){df$data[i] <- paste(df$raw[i],df$raw[i+1],df$raw[i+2])}
    else if(df$type[i] == "review" && !is.na(df$type[i+2]) && df$type[i+2] == "user"){df$data[i] <- paste(df$raw[i],df$raw[i+1])}
    else if(df$type[i] == "review"){df$data[i] <- df$raw[i]}}
  
  for (i in 1:nrow(df)){
    if(df$type[i] == "geo"){df$data[i] <- as.character(gsub("Length(.*?)mi(.*)","\\1",df$raw[i]))}
    else if(df$type[i] == "review"){df$data[i] <- df$raw[i]}
    else if(df$type[i] == "review_meta"){df$data[i] <- df$month[i]}
    else if(df$type[i] == "rating"){df$data[i] <- as.character(gsub(rating_regex, "\\2", df$raw[i], perl = TRUE))
    }}
  
  location_regex <- ".*(National Forest|National Scenic Area|National Grassland|National Wilderness Area|National Recreation Area|Basin Management Unit|National Volcanic Monument|Savannah River Site|National Monument).*"
  for (i in 1:nrow(df)) {
    if (df$type[i] == "location" && grepl(location_regex, df$raw[i])) {
      df$data[i] <- df$raw[i]
      df$type[i] <- "forest_name"
  }}
  
  for (i in 1:nrow(df)){
    if(df$type[i] == "forest_name"){df$type[i-1] <- "state"
    df$data[i-1] <- df$raw[i-1]
    df$type[i+1] <- "trail_name"
    df$data[i+1] <- df$raw[i+1]}
  }
  
  # tag rows for cleanup
  
  for (i in 1:nrow(df)){
    if(df$type[i] == "review" && grepl(pattern="^Conditions:.*",x=df$raw[i])){df$type[i] <- "condition"}
    else if(df$type[i] == "review" && grepl(pattern="^Comment from AllTrails.*",x=df$raw[i])){df$type[i] <- "alltrailsreply"}
  }
  
  # reshape data so that each row is a review with metadata in columns
  
  df$review_string <- ""
  df$trail_link <- df[1,1]
  df$trail_id <- trail_url
  df$trail_name <- ""
  df$trail_stars <- ""
  df$trail_rating <- ""
  df$trail_length <- ""
  df$trail_state <- ""
  df$forest_name <- ""
  df$review_month <- ""
  df$review_year <- ""
  df$review_day <- ""
  df$review_activity <- ""
  df$trail_totalrates <- total_rates
  df$trail_difficulty <- difficulty
  df$trail_elevchange <- elev_change
  
  for (i in 1:nrow(df)){
    if (df$type[i] == "rating"){
      rating <- sub(rating_regex,"\\1",df$raw[i])
    }}
  
  for (i in 2:nrow(df)){
    if(df$type[i] == "review"){
      df$review_string[i] <- df$data[i]
      df$trail_name[i] <- df$data[df$type == "trail_name"]
      df$trail_stars[i] <- df$data[df$type == "rating"][1]
      df$trail_rating[i] <- rating
      df$trail_length[i] <- df$data[df$type == "geo"]
      df$trail_state[i] <- df$data[df$type == "state"]
      df$forest_name[i] <- df$data[df$type == "forest_name"]
      df$review_month[i] <- df$data[i-1]
      df$review_year[i] <- as.character(gsub(".*\\b(\\d{4})\\b.*","\\1",df$raw[i-1]),perl=TRUE)
      df$review_day[i] <- as.numeric(gsub(".*\\b(\\d{1,2})\\b.*","\\1",df$raw[i-1]))
      df$review_activity[i] <- df$activity[i-1]
    }}
  
  drops <- c("raw","id","month","data","type","user")
  reviews <- df[ , !(names(df) %in% drops)] %>%
    filter(df$type == "review")
  
  write.csv(reviews,file=paste0(clean_folder,trail_url,".csv")) # an empty folder for clean CSVs for each trail
  
  # drop files into QC folder
  
  temp <- data.frame(trail_url,Sys.Date())
  write.table(temp,file=paste0(qc_folder,trail_url,".txt")) # create a new folder to dump a progress log
  
  # remove variables from working memory
  
  excess <- c("delete_index","difficulty","elev_change","i","link","months_regex","rating","total_rates","trail_url","df","reviews")
  try(rm(list=excess),silent=TRUE)
  gc()
  
}

## STITCH TRAIL PAGES ##

csv_files <- list.files(clean_folder, pattern = "\\.csv$", full.names = TRUE)
df_combined <- data.frame() # This will become the full dataset

for (file in csv_files) {
  temp_data <- read.csv(file)
  df_combined <- rbind(df_combined, temp_data)
}