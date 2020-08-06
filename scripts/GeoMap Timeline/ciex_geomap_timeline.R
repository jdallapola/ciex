library(tidyverse)
library(gsheet)
library(googlesheets4)
library(dplyr)
library(plyr)

# Calling Database #
url <- "https://docs.google.com/spreadsheets/d/1OTPqPYQ0NsXXT_bU33ygCrICfTq1WVExzf-0uv7CNfU/edit#gid=1046921816" 
intact_main_df <- data.frame(gsheet2tbl(url))

# Calling ISO 3 / coordinates reference list # 

iso_lat_lon = read.csv("https://raw.githubusercontent.com/jdallapola/ciex/master/scripts/GeoMap%20Timeline/iso_lat_lon.csv")

main_df = select(intact_main_df,!contains("obsolete"))%>%
  filter(stndr_name != "[Unidentified by CIEX]")

# Checking for errors

  count_cn =  data.frame(cn = str_count(main_df$cntry_action, ";"))
  count_cn[is.na(count_cn)] <- -1 
  count_dt = data.frame(dt = str_count(main_df$date_action, ";"))
  count_dt[is.na(count_dt)] <- -1 
  errors_geo = cbind(main_df,count_cn,count_dt)%>%
    mutate(difference = cn - dt)
  errors_geo = filter(errors_geo, difference != 0)
  nrow(errors_geo) #should be zero

# Consolidating country of birth
  
  main_df$birth<-ifelse(is.na(main_df$iden_cntry_birth),main_df$birth<-main_df$assum_cntry_birth,main_df$birth<-main_df$iden_cntry_birth)
  main_df = filter(main_df, birth=="Brazil") # For geomap timeline with only Brazilians
  
# Selecting name column
  names_df = select(main_df,stndr_name)

# Splitting Country Codes in Columns by Delimiter
  countries_df = data.frame(str_split_fixed(main_df$cntry_action, ";", Inf))
  
# Binding with names in Main DF, Replacing Blanks with NA and Transforming Columns into Rows
  countries_df = cbind(names_df,countries_df)
  countries_df[countries_df==""]<-NA
  countries_df = pivot_longer(countries_df,-stndr_name, values_to = "iso3", values_drop_na = TRUE)

# Splitting Dates in Columns by Delimiter, Replacing Blanks with NA and Transforming Columns into Rows
  dates_df = data.frame(str_split_fixed(main_df$date_action, ";", Inf))
  dates_df[dates_df==""]<-NA
  dates_df = pivot_longer(dates_df,cols = X1:X23,values_to = "date",values_drop_na = TRUE)

# Binding it all together and removing unecessary columns
  countries_df = cbind(countries_df,dates_df)
  countries_df = select(countries_df, -name)
  
# Removing unknown countries
  countries_df = filter(countries_df,!grepl('XX',iso3))
  
 # Run date fixer script below
  dates_df_fix <- data.frame(str_split_fixed(countries_df$date, "/", Inf))
  source("https://raw.githubusercontent.com/jdallapola/ciex/master/scripts/GeoMap%20Timeline/date_cleaner.R")  

# Matching Country codes to Longitude and Latitude
  countries_df <- join(countries_df, iso_lat_lon, by = "iso3", type = "left")

  # Exporting to CSV File
  write.csv(countries_df, "./export_data_geomap_timelineBR.csv",fileEncoding = "UTF-8", row.names = FALSE)

  
# ---------------------------------------------------------------------------------------------------------  
  
# For Appendix:   
  
  # Frequency table of countries

  most_freq_cn_action <- count(countries_df$iso3)

  most_freq_cn_action <- most_freq_cn_action[order(-most_freq_cn_action$freq),]
  names(most_freq_cn_action)[] = c("Country","Frequency")

  write.csv(head(most_freq_cn_action,10), "./most_freq_country_action.csv",fileEncoding = "UTF-8", row.names = FALSE)
  

  