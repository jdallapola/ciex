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


# Reading pdf files into list
            
  setwd("~/R/CIEX/pdfs") #Set Working Directory to folder with all pdfs
  
  files <- list.files(pattern = "pdf$")
  ciex_pdfs <- lapply(files, pdf_text)
  
# Transforming list into VCorpus
            #library(tm)

corp <- VCorpus(VectorSource(ciex_pdfs))

# Clean-up Process 
    
    ciex_stopwords = scan("~/R/CIEX/ciex_online/data/stopwords_CIEX.csv",what = "character")
    
    corp_cleaned <- tm_map(corp,content_transformer(tolower))
    corp_cleaned <- tm_map(corp_cleaned, removeWords, c("data",ciex_stopwords, 
                                                stopwords("pt"),
                                                stopwords("en"),
                                                stopwords("es")))
# Creating Document Term Matrix

    ciex_pdfs.dtm <- DocumentTermMatrix(corp_cleaned, control = list(removePunctuation = TRUE,
                                                                     stemming = FALSE,
                                                                     removeNumbers = TRUE,
                                                                     bounds = list(global = c(3, Inf)))) 
# Search and graph plotter tool
    
    # Importing PDF Years List
    
    years <- read.csv("https://raw.githubusercontent.com/jdallapola/ciex/master/data/years_CIEX.csv")
    
    # Creating search function
    
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
      windowsFonts(A = windowsFont("Louis George Café"))
      
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
    
    
    