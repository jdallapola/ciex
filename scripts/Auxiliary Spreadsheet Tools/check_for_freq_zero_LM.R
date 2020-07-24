# Loading Libraries #
library(tidyverse)
library(gsheet)
library(gsheet)
library(dplyr)
library(plyr)


# Calling Database #
url <- "https://docs.google.com/spreadsheets/d/1OTPqPYQ0NsXXT_bU33ygCrICfTq1WVExzf-0uv7CNfU/edit#gid=1046921816"
main_df <- gsheet2tbl(url)

# Calling Lista Master 
url_lm = "https://docs.google.com/spreadsheets/d/10b8AVDZJTfaC4fCUc0L7v_oiAELWTAIZ4BTHYM8QUDw/edit#gid=0"
lista_master = gsheet2tbl(url_lm)%>%
  data.frame

names_df = main_df%>% #separating names
  select(stndr_name)%>%
  filter(stndr_name != "Desconhecido [ver observações especiais]")

freq_nomes = count(names_df)

top10_names = head(freq_nomes[order(-freq_nomes$freq),],10)
top20_names = head(freq_nomes[order(-freq_nomes$freq),],20)

write.csv(top10_names, "~/R/CIEX/ciex_online/top_10_names.csv", row.names = FALSE)
write.csv(top20_names, "~/R/CIEX/ciex_online/top_20_names.csv", row.names = FALSE)

names_lm = lista_master%>%
  select(name)



check = purrr::map_df(names_lm, ~ .x %in% freq_nomes$stndr_name)

range_write(url_lm,
            check,
            sheet = "GS - Lista Master",
            range = "GS - Lista Master!E2:E",
            col_names = FALSE,
            reformat = FALSE)
