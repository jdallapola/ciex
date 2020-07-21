# Cleaning Work Environment #

rm(list = ls())

# Calling Required Packages #

library(pdfsearch)
library(pdftools)
library(tm)
library(stopwords)
library(dplyr)
library(stringr)
library(plyr)
library(data.table)
library(ggplot2)

# Calling Required Elements #
  
  # Setting Directory to PDF Files Location#
    setwd("C:/Users/jdall/Dropbox/Fundação Getulio Vargas/Produção Intelectual/Authoritarian Diffusion Project/Authoritarian Diffusion/CIEX/Data Analysis/R Scripts/Text-mining/pdfs")
  
  # Years List
    years = read.csv("C:/Users/jdall/Dropbox/Fundação Getulio Vargas/Produção Intelectual/Authoritarian Diffusion Project/Authoritarian Diffusion/CIEX/Data Analysis/R Scripts/Text-mining/years_CIEX.csv")
  
  # Sourcing and Running Text Mining Script
  source("C:/Users/jdall/Dropbox/Fundação Getulio Vargas/Produção Intelectual/Authoritarian Diffusion Project/Authoritarian Diffusion/CIEX/Data Analysis/R Scripts/Text-mining/Text Mining Scripts.R",local = TRUE)
   
# Front End
######################################################################################################
search_key <- "propaganda" #insert search key here
go_search() # initiate search mechanism and graph plotter

# Under the hood
######################################################################################################
go_search <- function()
  
  {

    search_directory = as.matrix(ciex_pdfs.tdm[ft,])
    search_directory = search_directory%>%
      data.frame()
    search_directory = setDT(search_directory, keep.rownames = TRUE)[]
    filtered = search_directory%>%
      filter(rn==search_key)%>%
      t()%>%
      data.frame()
    filtered$years = years
    filtered = filtered[-c(1), ]
    filtered = as.data.frame(sapply(filtered, as.numeric))
    filtered = aggregate(filtered$., by = list(Category = filtered$years), FUN = sum)
    
    # Graph elements #
      n = sum(filtered$x)  
      title = paste("Frequency of the word"," '",search_key,"' ","in CIEX Documents", sep = " ", collapse = NULL)
      subtitle = paste("n = ",n,"
                     ", sep = "", collapse = NULL)
      windowsFonts(A = windowsFont("Louis George Café"))
      
    # Structuring Graph #  
      
    p <- ggplot(filtered, aes(x=Category, y=x, group=1))+
      geom_line(color="#6891C3", size = 1.2) +
      ggtitle(title,subtitle=subtitle) +
      xlab("
      Year
      ") +
      ylab("Frequency
           ") + 
      theme(text=element_text(size = 25,
                              family = "A"))
    
      p # Plotting Graph
  
    
    }