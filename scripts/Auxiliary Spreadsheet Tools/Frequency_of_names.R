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

# Top names
top10_names = head(freq_nomes[order(-freq_nomes$freq),],10)
top20_names = head(freq_nomes[order(-freq_nomes$freq),],20)

# Distribution of frequencies


# By Frequency Group

count_freq = count(freq_nomes$freq)
names(count_freq)[1] <- "Frequency"
names(count_freq)[2] <- "Number_of_people"

  # [1]
  f.1 = data.frame("Interval" = "01 = n", 
                   "Names" = sum(select(filter(count_freq, Frequency == 1),"Number_of_people")),
                   "totalMentions" = sum(select(aggregate(filter(freq_nomes, freq == 1)$freq, by = list(freq = filter(freq_nomes, freq == 1)$freq), FUN = sum),x)))
  # [2]
  f.2 = data.frame("Interval" = "02 = n", 
                   "Names" = sum(select(filter(count_freq, Frequency == 2),"Number_of_people")),
                   "totalMentions" = sum(select(aggregate(filter(freq_nomes, freq == 2)$freq, by = list(freq = filter(freq_nomes, freq == 2)$freq), FUN = sum),x)))
  # [3]
  f.3 = data.frame("Interval" = "03 = n", 
                   "Names" = sum(select(filter(count_freq, Frequency == 3),"Number_of_people")),
                   "totalMentions" = sum(select(aggregate(filter(freq_nomes, freq == 3)$freq, by = list(freq = filter(freq_nomes, freq == 3)$freq), FUN = sum),x)))
  
  # [4, 5]
  f.4.5 = data.frame("Interval" = "04 \u2264 n \u2265 5", 
                     "Names" = sum(select(filter(count_freq, Frequency == 4 | Frequency == 5),"Number_of_people")),
                     "totalMentions" = sum(select(aggregate(filter(freq_nomes, freq == 4 | freq == 5)$freq, by = list(freq = filter(freq_nomes, freq == 4 | freq == 5)$freq), FUN = sum),x)))
  
  # [6, 10)
  f.6.10 = data.frame("Interval" = "06 \u2264 n > 10", 
                      "Names" = sum(select(filter(count_freq, Frequency >= 6 & Frequency < 10),"Number_of_people")),
                      "totalMentions" = sum(select(aggregate(filter(freq_nomes, freq >= 6 & freq < 10)$freq, by = list(freq = filter(freq_nomes, freq >= 6 & freq < 10)$freq), FUN = sum),x)))
   
  # [10, 20)
  f.10.20 = data.frame("Interval" = "10 \u2264 n > 20", 
                       "Names" = sum(select(filter(count_freq, Frequency >= 10 & Frequency < 20),"Number_of_people")),
                       "totalMentions" = sum(select(aggregate(filter(freq_nomes, freq >= 10 & freq < 20)$freq, by = list(freq = filter(freq_nomes, freq >= 10 & freq < 20)$freq), FUN = sum),x)))
  
  # [20, 30)
  f.20.30 = data.frame("Interval" = "20 \u2264 n > 30", 
                       "Names" = sum(select(filter(count_freq, Frequency >= 20 & Frequency < 30),"Number_of_people")),
                       "totalMentions" = sum(select(aggregate(filter(freq_nomes, freq >= 20 & freq < 30)$freq, by = list(freq = filter(freq_nomes, freq >= 20 & freq < 30)$freq), FUN = sum),x)))
 
  # [30, 40)
  f.30.50 = data.frame("Interval" = "30 \u2264 n > 40", 
                       "Names" = sum(select(filter(count_freq, Frequency >= 30 & Frequency < 40),"Number_of_people")),
                       "totalMentions" = sum(select(aggregate(filter(freq_nomes, freq >= 30 & freq < 40)$freq, by = list(freq = filter(freq_nomes, freq >= 30 & freq < 40)$freq), FUN = sum),x)))
  
  # [40, 50)
  f.40.50 = data.frame("Interval" = "40 \u2264 n > 50", 
                       "Names" = sum(select(filter(count_freq, Frequency >= 40 & Frequency < 50),"Number_of_people")),
                       "totalMentions" = sum(select(aggregate(filter(freq_nomes, freq >= 40 & freq < 50)$freq, by = list(freq = filter(freq_nomes, freq >= 40 & freq < 50)$freq), FUN = sum),x)))
  
# [50, Inf)
 
  f.over.50 = data.frame("Interval" = "n \u2265 50", 
                       "Names" = sum(select(filter(count_freq, Frequency >= 50),"Number_of_people")),
                       "totalMentions" = sum(select(aggregate(filter(freq_nomes, freq >= 50)$freq, by = list(freq = filter(freq_nomes, freq >= 50)$freq), FUN = sum),x)))
  
  
  freq_groups = rbind(rbind(f.1, f.2,f.3, f.4.5, f.6.10, f.10.20, f.20.30, f.30.40,f.40.50, f.over.50),
  data.frame("Interval" = "Total", "Names" = sum(count_freq$Number_of_people), "totalMentions" = sum(freq_nomes$freq)))
  
  freq_groups = mutate(freq_groups, "per.Names" = (Names/freq_groups[11,2])*100)
  freq_groups = mutate(freq_groups, "per.Mentions" = (totalMentions/freq_groups[11,3])*100)


  write.csv(freq_groups,"./people_by_mention_freq.csv", row.names = FALSE)
  
  
  over50_list = filter(freq_nomes[order(-freq_nomes$freq),],freq>=50)

  write.csv(over50_list, "./names_by_frequency_over50.csv", row.names = FALSE)









# By Quantiles

# Quantile graph
x <- count_freq$Frequency
groups <- quant_groups(x, 10)
qAll = count(groups)

qAll$x = gsub('[]([]', '', qAll$x)

qAll = qAll%>%
  separate(x, into = c("Low","High"),sep = ",")%>%
  select(-freq)

qAll$Low = as.numeric(qAll$Low)
qAll$High = as.numeric(qAll$High)

i = 1
distribution_df = data.frame(Quantile = numeric(), N_ppl= numeric())

while(i<=nrow(qAll)){
  
  low = qAll$Low[i]
  high = qAll$High[i]
  
  assign(paste("q", i, sep=""),filter(count_freq, Frequency >= low & Frequency <= high))
  q = assign(paste("q", i, sep=""),filter(count_freq, Frequency >= low & Frequency <= high))
  
  assign(paste("sum", i, sep=""), sum(q$Number_of_people))
  
  s =  assign(paste("sum", i, sep=""), sum(q$Number_of_people))
  
  new = data.frame("Quantile" = i, "N_ppl" = s)
  
  distribution_df = rbind(distribution_df,new)
  
  i = i+1
  
}

distribution_df = cbind(distribution_df,qAll)
distribution_df = mutate(distribution_df, "per" = N_ppl/sum(count_freq$Number_of_people))

write.csv(distribution_df,"~/distribution.csv",row.names = FALSE)


ggplot(distribution_df, aes(x = Quantile, y = per))+
  geom_bar(stat="identity")