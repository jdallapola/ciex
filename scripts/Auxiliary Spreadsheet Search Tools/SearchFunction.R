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
search_results = function() {
  cat("\014")     
  
  searchDat1 = select(SearchableDat, 
                      stndr_.name,
                      Y,
                      entr,
                      n_pdf,
                      n_doc,
                      pdf_url,
                      obs.,
                      check) %>%
    
    filter(stndr_.name==search_term1)
  
  searchDat2 = select(SearchableDat, 
                      stndr_.name,
                      Y,
                      entr,
                      n_pdf,
                      n_doc,
                      pdf_url,
                      obs.,
                      check) %>%
    
    filter(stndr_.name==search_term2)
  
  
  merged_results = merge(searchDat1,searchDat2, all = T)%>%
    filter(check==TRUE | is.na(check))
  print(merged_results)
  
}

go_search = function(){
  search_results()
  
  
  range_write(ss,
              search_results(),
              sheet = "Res_Search",
              range = "Res_Search!B7:I",
              col_names = FALSE,
              reformat = FALSE)
}


# Activating Search Function  #   
# (Looking for new search terms and syncing with Database every 30 sec) # 

while(1==1)  {
  
  queries = range_read(ss,"Search Interface","Res_Search!B1:F4",col_types="c")
  search_term1 = queries[[1,2]]
  search_term2 = queries[[2,2]]
  search_term_obs_esp = queries[[3,2]]
  check_search_obs = grepl(search_term_obs_esp, SearchableDat$obs) 
  SearchableDat = mutate(SearchableDat,check=check_search_obs)
  
  go_search()
  
  Sys.time()
  print(queries)
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

