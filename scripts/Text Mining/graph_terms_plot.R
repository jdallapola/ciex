# Sourcing and Running Text Mining Script (WARNING: Will take a few seconds to run)

source("https://raw.githubusercontent.com/jdallapola/ciex/master/scripts/Text%20Mining/ciex_pdf_mining.R",
       encoding = "UTF-8")        
                                                                                                  
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#    
#Custom Term graph plotting tools
  #Remember to set title in graphs

search_key <- "" #insert search key here
go_search() # initiate search mechanism and graph plotter
  

# Appendix Figures
setwd("C:/Users/jdall/Dropbox/FGV/Work/Authoritarian Diffusion Project/Authoritarian Diffusion/CIEX/Data Analysis/Visual Representations of Data/R - Gráficos Exportados")


# Figure 1
png("Figure1.png", units="in", width=8, height=3, res=300)
  search_key <- "Asilado" #insert search key here
  go_search()
dev.off()
  
# Figure 2
png("Figure2.png", units="in", width=8, height=3, res=300)
  search_key <- "Refugiado" #insert search key here
  go_search()
dev.off()    

# Figure 3
png("Figure3.png", units="in", width=8, height=3, res=300)
  search_key <- "Subversivo" #insert search key here
  go_search()
dev.off()

# Figure 4 
UY = as.matrix(ciex_pdfs.dtm)%>%  data.frame()%>%  select(tolower("Uruguai"))%>%  cbind(years)%>%  setNames(c("Freq.","Years"))
UY = setNames(aggregate(UY$Freq.,by = list(Years = UY$Years),FUN = sum), c("Years","Freq."))
CL = as.matrix(ciex_pdfs.dtm)%>%  data.frame()%>%  select(tolower("Chile"))%>%  cbind(years)%>%  setNames(c("Freq.","Years"))
CL = setNames(aggregate(CL$Freq.,by = list(Years = CL$Years),FUN = sum), c("Years","Freq."))
AR = as.matrix(ciex_pdfs.dtm)%>%  data.frame()%>%  select(tolower("Argentina"))%>%  cbind(years)%>%  setNames(c("Freq.","Years"))
AR = setNames(aggregate(AR$Freq.,by = list(Years = AR$Years),FUN = sum), c("Years","Freq."))
PT = as.matrix(ciex_pdfs.dtm)%>%  data.frame()%>%  select(tolower("Portugal"))%>%  cbind(years)%>%  setNames(c("Freq.","Years"))
PT = setNames(aggregate(PT$Freq.,by = list(Years = PT$Years),FUN = sum), c("Years","Freq."))

cn_terms = data.frame("Years" = 1966:1986,"UY" = UY$Freq,"CL" = CL$Freq.,"AR" = AR$Freq.,"PT" = PT$Freq.)
cn_terms <- cn_terms %>% gather(key = "variable", value = "value", -Years)

  #Plotting Graph
    
  png("Figure6.png", units="in", width=8, height=5, res=300)
    
    ggplot(cn_terms, aes(x = Years, y = value)) + 
    geom_ribbon(aes(ymin = min(value), ymax = value, 
                    fill = variable),size = 1) + 
    theme_bw()+
    scale_fill_manual(values = alpha(c("#ffcd3c","#ff9234","#75AADB","#35d0ba"),.7),
                      name = "",
                      labels = c("Uruguay", "Chile", "Argentina", "Portugal"),
                      breaks = c("UY", "CL", "AR", "PT"))+
    xlab("Year") +
    ylab("Frequency") + 
    theme(text=element_text(size = 15,family = "A"),
          axis.title.x = element_text(margin = margin(t=10,r=0,b=00,l=0)),
          axis.title.y = element_text(margin = margin(t=00,r=20,b=0,l=00)))

    dev.off()

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#