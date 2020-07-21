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
search_results_doc = function() {
  
  searchDat_doc = select(SearchableDat, 
                      stndr_.name,
                      Y,
                      entr,
                      n_pdf,
                      n_doc,
                      pdf_url,
                      obs.,
                      check) %>%
                      filter(Y==year & entr == entrada & n_pdf == pdf & n_doc == doc)
  
}

go_search_doc = function(){
  search_results_doc()
  
  range_write(ss,
              search_results_doc(),
              sheet = "Res_Search_Doc",
              range = "Res_Search_Doc!B5:I",
              col_names = FALSE,
              reformat = FALSE)
}


# Activating Search Function  #   
# (Looking for new search terms and syncing with Database every 30 sec) # 

while(1==1){
  
  queries_doc = range_read(ss,"Search Interface","Res_Search_Doc!C1:F2",col_types="c")
  year = queries_doc[[1,1]]
  entrada = queries_doc[[1,2]]
  pdf = queries_doc[[1,3]]
  doc = queries_doc[[1,4]]

  go_search_doc()
  
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

