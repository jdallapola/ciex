library(tidyverse)
library(gsheet)
library(dplyr)
library(plyr)
library(dvmisc)


#Graphs from Collected Data

# Calling Database #
url <- "https://docs.google.com/spreadsheets/d/1OTPqPYQ0NsXXT_bU33ygCrICfTq1WVExzf-0uv7CNfU/edit#gid=1046921816" 
intact_main_df <- data.frame(gsheet2tbl(url))

main_df = select(intact_main_df,!contains("obsolete"))

names_df = main_df%>% #separating names
  select(stndr_name)%>%
  filter(stndr_name != "[Unidentified by CIEX]"&
        !grepl('u.f.n.',stndr_name)&
        !grepl('u.l.n.',stndr_name)&
        !grepl('illegible',stndr_name))


# Names in database and respective frequencies
freq_nomes = count(names_df)

# By Frequency Group

count_freq = count(freq_nomes$freq)
names(count_freq)[1] <- "Mentions"
names(count_freq)[2] <- "Number_of_people"

  # [1]
  f.1 = data.frame("Interval" = "01 = n", 
                   "Names" = sum(select(filter(count_freq, Mentions == 1),"Number_of_people")),
                   "Entries" = sum(select(aggregate(filter(freq_nomes, freq == 1)$freq, by = list(freq = filter(freq_nomes, freq == 1)$freq), FUN = sum),x)))
  # [2]
  f.2 = data.frame("Interval" = "02 = n", 
                   "Names" = sum(select(filter(count_freq, Mentions == 2),"Number_of_people")),
                   "Entries" = sum(select(aggregate(filter(freq_nomes, freq == 2)$freq, by = list(freq = filter(freq_nomes, freq == 2)$freq), FUN = sum),x)))
  # [3]
  f.3 = data.frame("Interval" = "03 = n", 
                   "Names" = sum(select(filter(count_freq, Mentions == 3),"Number_of_people")),
                   "Entries" = sum(select(aggregate(filter(freq_nomes, freq == 3)$freq, by = list(freq = filter(freq_nomes, freq == 3)$freq), FUN = sum),x)))
  
  # [4, 5]
  f.4.5 = data.frame("Interval" = "04 \u2264 n \u2265 5", 
                     "Names" = sum(select(filter(count_freq, Mentions == 4 | Mentions == 5),"Number_of_people")),
                     "Entries" = sum(select(aggregate(filter(freq_nomes, freq == 4 | freq == 5)$freq, by = list(freq = filter(freq_nomes, freq == 4 | freq == 5)$freq), FUN = sum),x)))
  
  # [6, 10)
  f.6.10 = data.frame("Interval" = "06 \u2264 n > 10", 
                      "Names" = sum(select(filter(count_freq, Mentions >= 6 & Mentions < 10),"Number_of_people")),
                      "Entries" = sum(select(aggregate(filter(freq_nomes, freq >= 6 & freq < 10)$freq, by = list(freq = filter(freq_nomes, freq >= 6 & freq < 10)$freq), FUN = sum),x)))
   
  # [10, 20)
  f.10.20 = data.frame("Interval" = "10 \u2264 n > 20", 
                       "Names" = sum(select(filter(count_freq, Mentions >= 10 & Mentions < 20),"Number_of_people")),
                       "Entries" = sum(select(aggregate(filter(freq_nomes, freq >= 10 & freq < 20)$freq, by = list(freq = filter(freq_nomes, freq >= 10 & freq < 20)$freq), FUN = sum),x)))
  
  # [20, 30)
  f.20.30 = data.frame("Interval" = "20 \u2264 n > 30", 
                       "Names" = sum(select(filter(count_freq, Mentions >= 20 & Mentions < 30),"Number_of_people")),
                       "Entries" = sum(select(aggregate(filter(freq_nomes, freq >= 20 & freq < 30)$freq, by = list(freq = filter(freq_nomes, freq >= 20 & freq < 30)$freq), FUN = sum),x)))
 
  # [30, 40)
  f.30.40 = data.frame("Interval" = "30 \u2264 n > 40", 
                       "Names" = sum(select(filter(count_freq, Mentions >= 30 & Mentions < 40),"Number_of_people")),
                       "Entries" = sum(select(aggregate(filter(freq_nomes, freq >= 30 & freq < 40)$freq, by = list(freq = filter(freq_nomes, freq >= 30 & freq < 40)$freq), FUN = sum),x)))
  
  # [40, 50)
  f.40.50 = data.frame("Interval" = "40 \u2264 n > 50", 
                       "Names" = sum(select(filter(count_freq, Mentions >= 40 & Mentions < 50),"Number_of_people")),
                       "Entries" = sum(select(aggregate(filter(freq_nomes, freq >= 40 & freq < 50)$freq, by = list(freq = filter(freq_nomes, freq >= 40 & freq < 50)$freq), FUN = sum),x)))
  
# [50, Inf)
 
  f.over.50 = data.frame("Interval" = "n \u2265 50", 
                       "Names" = sum(select(filter(count_freq, Mentions >= 50),"Number_of_people")),
                       "Entries" = sum(select(aggregate(filter(freq_nomes, freq >= 50)$freq, by = list(freq = filter(freq_nomes, freq >= 50)$freq), FUN = sum),x)))
  