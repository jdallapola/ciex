# Loading Libraries #
library(tidyverse)
library(gsheet)
library(gsheet)
library(googlesheets4)
library(dplyr)
library(plyr)

# Calling Main Database #
url <- "https://docs.google.com/spreadsheets/d/1OTPqPYQ0NsXXT_bU33ygCrICfTq1WVExzf-0uv7CNfU/edit#gid=1046921816"
main_df <- gsheet2tbl(url)

# Calling Directory of Names #
url_dir = "https://docs.google.com/spreadsheets/d/10b8AVDZJTfaC4fCUc0L7v_oiAELWTAIZ4BTHYM8QUDw/edit#gid=0"
names_dir = data.frame(gsheet2tbl(url_dir))%>%
  select(name)

# Creating list of names from DF
names_df = select(main_df, stndr_name)
freq_names = count(tolower(names_df$stndr_name))     

colnames(freq_names)

# Disclaimer: before running, make sure:
# 1) Google Spreadsheet housing the directory of names is in alphabetical order;
     # If not in alphabetical order, go back to sheet and sort columns by abc order     
          head(names_dir)
          tail(names_dir)
          
# 2) The number of rows in the freq_names is the same as the number of rows names_dir
          nrow(freq_names)
          nrow(names_dir)
        
# 3) Check to see if all names in freq_names are in names_dir
        check = purrr::map_df(names_dir, ~ .x %in% freq_names$x)
        count(check) # All should be True
        
# Writing frequency to LM
range_write(url_dir,
            data.frame(freq_names$freq),
            sheet = "GS - Lista Master",
            range = "GS - Lista Master!D2:D",
            col_names = FALSE,
            reformat = FALSE)

