# Loading Libraries #
library(tidyverse)
library(gsheet)
library(googlesheets4)
library(dplyr)


# Calling Database #
url = "https://docs.google.com/spreadsheets/d/14BhydyH1cySwZLYdVMCTAih9UaseYCVMKwlTnCnnhqQ/edit?usp=sharing"
SearchableDat = gsheet2tbl(url)


# Converting to Data.Frame #
SearchableDat = SearchableDat %>%
  data.frame()


# Fixing encoding #
Encoding(rownames(SearchableDat)) = "UTF-8"


# Checking if alright #
head(SearchableDat)


# Calling results Sheet #
ss = "docs.google.com/spreadsheets/d/1IdBYo0T4-Okoow2PwImNSu28aCQgv-8QmK4xrr_r5-U/edit#gid=0 #"


# Organizing Database & Searching #
search_results_obs = function() {
  cat("\014")     
  
  searchDat_obs = select(SearchableDat_obs, 
                      stndr_.name,
                      Y,
                      entr,
                      n_pdf,
                      n_doc,
                      pdf_url,
                      obs.,
                      check)%>%
                      filter(check==TRUE | is.na(check))
  
  print(searchDat_obs)
  
}

go_search_obs = function(){
  search_results_obs()
  
  
  range_write(ss,
              search_results_obs(),
              sheet = "Res_Search",
              range = "Res_Search!B7:I",
              col_names = FALSE,
              reformat = FALSE)
}


# Activating Search Function  #   
# (Looking for new search terms and syncing with Database every 30 sec) # 

while(1==1)  {
  
  queries_obs = range_read(ss,"Search Interface","Res_Search!B1:F4",col_types="c")
  search_term_obs_esp = queries_obs[[3,2]]
  check_search_obs = grepl(search_term_obs_esp, SearchableDat$obs) 
  SearchableDat_obs = mutate(SearchableDat_obs,check=check_search_obs)
  
  go_search_obs()
  
  Sys.time()
  print(queries_obs)
  print(Sys.time())
  
  countdown = function(from)
  {
    print(from)
    while(from!=0)
    {
      Sys.sleep(1)
      from = from - 1
      print(from)
    }
  }
  
  countdown(25)
  
}

1
# ------------------------------------------------------------------------------------ #

