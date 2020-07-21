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


,# Calling results Sheet #
ss = "docs.google.com/spreadsheets/d/1IdBYo0T4-Okoow2PwImNSu28aCQgv-8QmK4xrr_r5-U/edit#gid=0 #"


# Organizing Database and Filtering#
filter_results = function() {
  cat("\014")     
  
    merged_results = select(SearchableDat, 
                          stndr_.name,
                          acao_title,
                          Y,
                          n_pdf,
                          n_doc,
                          pdf_url,
                          assum_cntry_birth,
                          iden_cntry_birth,
                          obs.,
                          n) %>%
    filter(is.na(assum_cntry_birth) & is.na(iden_cntry_birth) & stndr_.name != "Desconhecido [ver observações especiais]")%>%
    arrange(n)
    

  }

go_filter = function(){
  
  filter_results()
  
  
  range_write(ss,
              filter_results(),
              sheet = "Res_Filter",
              range = "Res_Filter!B2:",
              col_names = FALSE,
              reformat = FALSE)
  
  print("DONE!")
  
}


go_filter()

view(merged_results)