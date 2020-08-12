# Sourcing and Running Text Mining Script (WARNING: Will take a few seconds to run)

source("https://raw.githubusercontent.com/jdallapola/ciex/master/scripts/Text%20Mining/ciex_pdf_mining.R")


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#    
# Appendix Tables

#Table 1



write.table()


# Table 2

countries <- read.csv("https://raw.githubusercontent.com/jdallapola/ciex/master/scripts/Text%20Mining/Exported_csvs/country_list_pt.csv", header = FALSE)
countries <-as.character(countries$V1)
countries.dtm <-DocumentTermMatrix(corp_cleaned, control=list(dictionary = countries))

cn_top_10 <- head(stack(sort(apply(countries.dtm, 2, sum), decreasing = TRUE)),10)
  cn_top_10 <- cn_top_10[,c(2,1)]
  names(cn_top_10) <- c("Country","Frequency")
  capFirst <- function(s) {paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")}
  cn_top_10$Country <- capFirst(cn_top_10$Country)

write.table(cn_top_10, "~/R/CIEX/ciex_online/cn_top10_tm.txt", sep = ",", row.names = FALSE)

# Table 3



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#    
# Appendix Figures
setwd("C:/Users/jdall/Dropbox/FGV/Work/Authoritarian Diffusion Project/Authoritarian Diffusion/CIEX/Data Analysis/Visual Representations of Data/R - Gráficos Exportados")


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
  geom_text(x = 1982.5, y = 400, aes(label = label), data = f_labels,family= "A")


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
#CU = as.matrix(ciex_pdfs.dtm)%>%  data.frame()%>%  select(tolower("Cuba"))%>%  cbind(years)%>%  setNames(c("Freq.","Years"))
#CU = setNames(aggregate(CU$Freq.,by = list(Years = CU$Years),FUN = sum), c("Years","Freq."))

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
      facet_grid(variable ~ .)

dev.off()


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


#Custom Term graph plotting tools
#Remember to set title in graphs

setwd("C:/Users/jdall/Dropbox/FGV/Work/Authoritarian Diffusion Project/Authoritarian Diffusion/CIEX/Data Analysis/Visual Representations of Data/R - Gráficos Exportados")
png(paste0(search_key,".png"), units="in", width=8, height=3, res=300)
search_key <- "subversivo" #insert search key here
go_search()
dev.off()