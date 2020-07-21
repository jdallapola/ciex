# Loading Libraries #
library(tidyverse)
library(gsheet)
library(googlesheets4)
library(dplyr)
library(plyr)

# Remember: After fixing mismatched country codes and dates, 
# must convert ISO country codes to ISO 3 standard before beginning -> in excel

# Calling Database #
url <- "https://docs.google.com/spreadsheets/d/1OTPqPYQ0NsXXT_bU33ygCrICfTq1WVExzf-0uv7CNfU/edit#gid=1046921816" 
main_df <- gsheet2tbl(url)

# Calling and customizing ISO 3 Country Code List # 

iso_lat_lon = read.csv("https://gist.githubusercontent.com/tadast/8827699/raw/3cd639fa34eec5067080a61c69e3ae25e3076abb/countries_codes_and_coordinates.csv")
iso_lat_lon = iso_lat_lon%>%
  select("Alpha.3.code","Latitude..average.","Longitude..average.")

iso_lat_lon$Alpha.3.code <- trimws(iso_lat_lon$Alpha.3.code, which = c("left"))

# Converting to Data.Frame #
main_df <- main_df %>%
  data.frame()
# Fixing encoding #
Encoding(rownames(main_df)) = "UTF-8"
#Removing old columns
main_df <- main_df%>%
            select(-País.da.Ação,
                   -Cidade.da.Ação,
                   -País.Destino,
                   -Cidade.Destino, 
                   -Nomes.Entidades, 
                   -Data.Ação, 
                   -Status)
# Checking if alright #
head(main_df)
colnames(main_df)
dim(main_df)


# Consolidating country of birth

main_df$birth<-ifelse(is.na(main_df$iden_cntry_birth),main_df$birth<-main_df$assum_cntry_birth,main_df$birth<-main_df$iden_cntry_birth)
bra_nomes<-main_df[main_df$birth=="Brazil","stndr_name"]
bra_nomes<-unique(bra_nomes)

main_df = main_df%>%
  filter(birth=="Brazil")

# Ignoring errors

count_cn =  data.frame(cn = str_count(main_df$cntry_action, ";"))
  count_cn[is.na(count_cn)] <- -1 

count_dt = data.frame(dt = str_count(main_df$date_action, ";"))
  count_dt[is.na(count_dt)] <- -1 

main_df = cbind(main_df,count_cn,count_dt)%>%
  mutate(total_geo = cn - dt)

main_df = main_df%>%
  filter(total_geo == 0)

errors_geo = main_df%>%
  filter(total_geo != 0)

###############
# Geomap Analysis Timeline
  
  names_df = main_df%>% #separating names
        select(stndr_name)

  # Splitting Country Codes in Columns by Delimiter
  countries_df = str_split_fixed(main_df$cntry_action, ";", Inf)%>%
  data.frame()
  
  
  # Binding with names in Main DF and pivoting (Transforming Columns in to Rows)
  countries_df = cbind(countries_df,names_df)
  countries_df[countries_df==""]<-NA
  countries_df = countries_df %>%
      pivot_longer(-stndr_name, values_to = "Alpha.3.code", values_drop_na = TRUE)
  
  # Splitting Dates in Columns by Delimiter
  dates_df = str_split_fixed(main_df$date_action, ";", Inf)%>%
  data.frame()
  
  #Replacing Blanks with NA
  dates_df[dates_df==""]<-NA
  colnames(dates_df)
  
  # Pivot - Transforming Columns in to Rows
  dates_df = dates_df%>%
    pivot_longer(cols = X1:X23,values_to = "date",values_drop_na = TRUE)
  

# Binding it all together and removing unecessary columns
  countries_df = cbind(countries_df,dates_df)
  countries_df = select(countries_df, -name)
  
  # Removing unknown countries
  countries_df = countries_df%>%
                filter(!grepl('XX',Alpha.3.code))
  
  # Run date fixer script below
  
  source("https://raw.githubusercontent.com/jdallapola/ciex/master/scripts/GeoMap%20Timeline/set_random_D_M.R")  

  # Run ISO 2 to ISO 3 converter
  
  # Calling lookup table
      lookup = read.csv("./iso_conversion_table.csv",sep=";")
  
  # Testing find and replace based on lookup table
      countries_df$Alpha.3.code[] <- lookup$ISO3[match(unlist(countries_df$Alpha.3.code), lookup$ISO2)]
      countries_df = countries_df%>%
        data.frame()
  
  
  
  # Matching Country codes to Longitude and Latitude
  countries_df <- join(countries_df, iso_lat_lon, by = "Alpha.3.code", type = "left")

  # Exporting to CSV File
  write.csv(countries_df, "./data_geomap_timeline.csv",fileEncoding = "UTF-8", row.names = FALSE)



