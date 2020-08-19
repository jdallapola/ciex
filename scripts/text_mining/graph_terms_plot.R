# Sourcing and Running Text Mining Script (WARNING: Will take a few seconds to run)

source("https://raw.githubusercontent.com/jdallapola/ciex/master/scripts/Text%20Mining/ciex_pdf_mining.R")


#Dropbox
setwd("C:/Users/jdall/Dropbox/FGV/Work/Authoritarian Diffusion Project/Authoritarian Diffusion/CIEX/Data Analysis/Visual Representations of Data/R - Gráficos Exportados")
#GitHub
setwd("~/R/CIEX/ciex_online/tables_and_figures/")



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#    
# Appendix Tables

#Table 1

# Table 2

country_list <- read.csv("https://raw.githubusercontent.com/jdallapola/ciex/master/scripts/Text%20Mining/Exported_csvs/country_list_pt.csv", header = FALSE)
country_list <-as.character(country_list$V1)
countries.dtm <-DocumentTermMatrix(corp_cleaned, control=list(dictionary = country_list))

cn_top_10_tm<- head(stack(sort(apply(countries.dtm, 2, sum), decreasing = TRUE)),10)
  cn_top_10_tm <- cn_top_10_tm[,c(2,1)]
  names(cn_top_10_tm) <- c("Country","Frequency")
  capFirst <- function(s) {paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")}
  cn_top_10_tm$Country <- capFirst(cn_top_10_tm$Country)

write.table(cn_top_10_tm, "~/R/CIEX/ciex_online/tables_and_figures/table_2.txt", sep = ",", row.names = FALSE)

# Table 3

countries_df = read.csv("https://raw.githubusercontent.com/jdallapola/ciex/master/scripts/GeoMap%20Timeline/export_data_geomap_timeline.csv")
  cn_top_10_df <- count(countries_df$iso3)
  cn_top_10_df <- head(cn_top_10_df[order(-cn_top_10_df$freq),],10)
  names(cn_top_10_df) = c("Country","Frequency")

write.table(cn_top_10_df, "~/R/CIEX/ciex_online/tables_and_figures/table_3.txt", sep = ",", row.names = FALSE)


# Table 4,5 and 6

    source("https://raw.githubusercontent.com/jdallapola/ciex/master/scripts/Auxiliary%20Spreadsheet%20Tools/Frequency_of_names.R")
  
    freq_groups = rbind(rbind(f.1, f.2,f.3, f.4.5, f.6.10, f.10.20, f.20.30, f.30.40,f.40.50, f.over.50),
                    data.frame("Interval" = "Total", "Names" = sum(count_freq$Number_of_people), "Entries" = sum(freq_nomes$freq)))

  # Table 4
    names_df_by_interval = select(freq_groups, Interval, Names)%>%
                       mutate(names_df_by_interval, "Percentage" = (Names/freq_groups[11,2])*100)
          write.table(names_df_by_interval, "~/R/CIEX/ciex_online/tables_and_figures/table_4.txt", sep = ",", row.names = FALSE)
    
  # Table 5
    entries_df_by_interval = select(freq_groups, Interval,Entries)%>%
                         mutate(entries_df_by_interval, "Percentage" = (Entries/freq_groups[11,3])*100)
          write.table(entries_df_by_interval, "~/R/CIEX/ciex_online/tables_and_figures/table_5.txt", sep = ",", row.names = FALSE)
  
  # Table 6 
    over50_list = filter(freq_nomes[order(-freq_nomes$freq),],freq>=50)
          write.table(over50_list, "~/R/CIEX/ciex_online/tables_and_figures/table_6.txt", sep = ",", row.names = FALSE)
    

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#    
# Appendix Figures
     
      # Figure 1

term_1 = as.matrix(ciex_pdfs.dtm)%>%  data.frame()%>%  select(tolower("Asilado"))%>%  cbind(years)%>%  setNames(c("Freq.","Years"))
term_1 = setNames(aggregate(term_1$Freq.,by = list(Years = term_1$Years),FUN = sum), c("Years","Freq."))
term_2 = as.matrix(ciex_pdfs.dtm)%>%  data.frame()%>%  select(tolower("Marginado"))%>%  cbind(years)%>%  setNames(c("Freq.","Years"))
term_2 = setNames(aggregate(term_2$Freq.,by = list(Years = term_2$Years),FUN = sum), c("Years","Freq."))
term_3 = as.matrix(ciex_pdfs.dtm)%>%  data.frame()%>%  select(tolower("Refugiado"))%>%  cbind(years)%>%  setNames(c("Freq.","Years"))
term_3 = setNames(aggregate(term_3$Freq.,by = list(Years = term_3$Years),FUN = sum), c("Years","Freq."))
term_4 = as.matrix(ciex_pdfs.dtm)%>%  data.frame()%>%  select(tolower("Subversivo"))%>%  cbind(years)%>%  setNames(c("Freq.","Years"))
term_4 = setNames(aggregate(term_4$Freq.,by = list(Years = term_4$Years),FUN = sum), c("Years","Freq."))
    
    selected_terms = data.frame("Years" = 1966:1986,"Asilado" = term_1$Freq,"Marginado" = term_2$Freq.,"Refugiado" = term_3$Freq.,"Subversivo" = term_4$Freq.)
    selected_terms <- selected_terms %>% gather(key = "variable", value = "value", -Years)
    selected_terms$variable <- factor(selected_terms$variable,levels=unique(selected_terms$variable))
    f_labels <- data.frame(variable = c("Asilado", "Marginado", "Refugiado", "Subversivo"), label = c("Brazilian 1979 Amnesty Law", "", "",""))
    f_labels$variable <- factor(f_labels$variable,levels=unique(f_labels$variable))    

png("Figure1.png", units="in", width=8, height=6, res=300)

ggplot(selected_terms, aes(x = Years, y = value)) + 
  geom_ribbon(aes(ymin = min(value), ymax = value,fill = variable),size = .1,color = "#514e4f") + 
  theme_bw()+
  scale_fill_manual(values = alpha(c("#87bbd2","#87bbd2","#87bbd2","#87bbd2"),.9), breaks = c("Asilado", "Marginado","Refugiado", "Subversivo"))+
  xlab("Year") +
  ylab("Frequency") + 
  theme(text=element_text(size = 15,family = "A"),
        axis.title.x = element_text(margin = margin(t=10,r=0,b=00,l=0)),
        axis.title.y = element_text(margin = margin(t=00,r=20,b=0,l=00)),
        legend.position = "none")+
  facet_grid(variable ~ .,scales="free")+
  geom_vline(xintercept = 1979,linetype="dotted", color = "#4F4F4F", size=.8)+
  geom_text(x = 1982.5, y = 400, aes(label = label), data = f_labels,family= "A")+
  scale_x_continuous(breaks = round(seq(min(selected_terms$Years), max(selected_terms$Years), by = 2),1))

dev.off()

# Figure 2 
UY = as.matrix(ciex_pdfs.dtm)%>%  data.frame()%>%  select(tolower("Uruguai"))%>%  cbind(years)%>%  setNames(c("Freq.","Years"))
UY = setNames(aggregate(UY$Freq.,by = list(Years = UY$Years),FUN = sum), c("Years","Freq."))
CL = as.matrix(ciex_pdfs.dtm)%>%  data.frame()%>%  select(tolower("Chile"))%>%  cbind(years)%>%  setNames(c("Freq.","Years"))
CL = setNames(aggregate(CL$Freq.,by = list(Years = CL$Years),FUN = sum), c("Years","Freq."))
AR = as.matrix(ciex_pdfs.dtm)%>%  data.frame()%>%  select(tolower("Argentina"))%>%  cbind(years)%>%  setNames(c("Freq.","Years"))
AR = setNames(aggregate(AR$Freq.,by = list(Years = AR$Years),FUN = sum), c("Years","Freq."))
PT = as.matrix(ciex_pdfs.dtm)%>%  data.frame()%>%  select(tolower("Portugal"))%>%  cbind(years)%>%  setNames(c("Freq.","Years"))
PT = setNames(aggregate(PT$Freq.,by = list(Years = PT$Years),FUN = sum), c("Years","Freq."))

cn_terms = data.frame("Years" = 1966:1986,"Uruguay" = UY$Freq,"Chile" = CL$Freq.,"Argentina" = AR$Freq.,"Portugal" = PT$Freq.)
cn_terms <- cn_terms %>% gather(key = "variable", value = "value", -Years)
cn_terms$variable <- factor(cn_terms$variable,levels=unique(cn_terms$variable))


#Plotting Graph

png("Figure2.png", units="in", width=8, height=6, res=300)

  ggplot(cn_terms, aes(x = Years, y = value)) + 
      geom_ribbon(aes(ymin = min(value), ymax = value, fill = variable),size = .1, color = "#514e4f") + 
      theme_bw()+
      scale_fill_manual(values = alpha(c("#ffcd3c","#ff9234","#75AADB","#35d0ba"),.7),name = "",breaks = c("Uruguay", "Chile", "Argentina", "Portugal"))+
      xlab("Year") +
      ylab("Frequency") + 
      theme(text=element_text(size = 15,family = "A"),
            axis.title.x = element_text(margin = margin(t=10,r=0,b=00,l=0)),
            axis.title.y = element_text(margin = margin(t=00,r=20,b=0,l=00)),
            legend.position = "none")+
      facet_grid(variable ~ .)+
      scale_x_continuous(breaks = round(seq(min(selected_terms$Years), max(selected_terms$Years), by = 2),1))

dev.off()


















#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


#Custom Term graph plotting tools
#Remember to set title in graphs

setwd("C:/Users/jdall/Dropbox/FGV/Work/Authoritarian Diffusion Project/Authoritarian Diffusion/CIEX/Data Analysis/Visual Representations of Data/R - Gráficos Exportados")
png(paste0(search_key,".png"), units="in", width=8, height=3, res=300)
search_key <- "subversivo" #insert search key here
go_search()
dev.off()