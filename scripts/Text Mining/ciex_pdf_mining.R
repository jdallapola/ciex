# Calling Required Libraries #

library(pdfsearch)
library(pdftools)
library(tm)
library(stopwords)
library(dplyr)
library(stringr)
library(plyr)
library(data.table)
library(ggplot2)


# Following instructions at https://uvastatlab.github.io/2019/05/14/reading-pdf-files-into-r-for-text-mining/ #

# Reading pdf files into list
            #library(pdftools)

  setwd("~/R/CIEX/pdfs") #Set Working Directory to folder with all pdfs
  
  files <- list.files(pattern = "pdf$")
  ciex_pdfs <- lapply(files, pdf_text)
  length(ciex_pdfs)
  lapply(ciex_pdfs, length) 

# Transforming list into VCorpus
            #library(tm)

corp <- VCorpus(VectorSource(ciex_pdfs))

# Clean-up Process 
    # library(stopwords)

    #stopwords_CIEX = read.csv("https://raw.githubusercontent.com/jdallapola/ciex/master/stopwords_CIEX.csv")
    
    #corp <- tm_map(corp, removeWords, stopwords_pt)
    #corp <- tm_map(corp, removeWords, stopwords_CIEX)

# Creating Document Term Matrix
    
    ciex_pdfs.dtm <- DocumentTermMatrix(corp, 
                                        control = 
                                          list(removePunctuation = TRUE,
                                               stopwords = TRUE,
                                               tolower = TRUE,
                                               removeNumbers = TRUE,
                                               bounds = list(global = c(3, Inf)))) 
    # Search and graph plotter tool
    
    # Importing PDF Years List
    
    years <- read.csv("https://raw.githubusercontent.com/jdallapola/ciex/master/data/years_CIEX.csv")
    
    go_search <- function(){
      
      search_directory = as.matrix(ciex_pdfs.dtm)%>%
        data.frame()%>%
        select(tolower(search_key))%>%
        cbind(years)%>%
        setNames(c("Freq.","Years"))
      
      search_directory = setNames(aggregate(search_directory$Freq.,
                                            by = list(Years = search_directory$Years), 
                                            FUN = sum), 
                                  c("Years","Freq."))
      
      # Graph elements #
      n <- sum(search_directory$Freq.)
      title <- paste0("Frequency of the word ","''",search_key,"''"," in CIEX Documents", collapse = NULL)
      subtitle <- paste("n = ",n, collapse = NULL)
      windowsFonts(A = windowsFont("Louis George Caf?"))
      
      # Structuring and Plotting Graph #  
      
      ggplot(search_directory, aes(x=Years, y=Freq., group=1))+
        geom_line(color="#6891C3", size = 1.2) +
        ggtitle(title,subtitle=subtitle) +
        xlab("Year") +
        ylab("Frequency") + 
        theme(text = element_text(size = 15,family = "A"),
              axis.title.x = element_text(margin = margin(t=20,r=0,b=20,l=0)),
              axis.title.y = element_text(margin = margin(t=00,r=20,b=0,l=20)),
              plot.subtitle = element_text(size = 10, margin = margin(t=0,r=0,b=20,l=0)))
    }   
    
    
    