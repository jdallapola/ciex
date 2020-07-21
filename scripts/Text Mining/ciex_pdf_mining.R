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


  print("Reading pdf files into list")
            
  setwd("~/R/CIEX/pdfs") #Set Working Directory to folder with all pdfs
  
  files <- list.files(pattern = "pdf$")
  ciex_pdfs <- lapply(files, pdf_text)
  
  print("Finished reading pdf files")
  
  print("Transforming list into VCorpus")

  corp <- VCorpus(VectorSource(ciex_pdfs))

  print("Clean-up Process")
    
    ciex_stopwords = scan("https://raw.githubusercontent.com/jdallapola/ciex/master/scripts/Text%20Mining/stopwords_CIEX.csv",what = "character")
    
    corp_cleaned <- tm_map(corp,content_transformer(tolower))
    corp_cleaned <- tm_map(corp_cleaned, removeWords, c("data",ciex_stopwords, 
                                                stopwords("pt"),
                                                stopwords("en"),
                                                stopwords("es")))

  corp_cleaned <- tm_map(corp_cleaned, stemDocument, language = "pt")  
  corp_cleaned <- tm_map(corp_cleaned,content_transformer(removePunctuation))
  corp_cleaned <- tm_map(corp_cleaned,content_transformer(removeNumbers))
  
print("Creating Document Term Matrix")

    ciex_pdfs.dtm <- DocumentTermMatrix(corp_cleaned, control = list(bounds = list(global = c(3, Inf)))) 
    
print("Search and graph plotter tool")
    
    #Importing PDF Years List
    
    years <- read.csv("https://raw.githubusercontent.com/jdallapola/ciex/master/scripts/Text%20Mining/years_CIEX.csv")
    
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
      title <- paste0("Figure ",graph_n,search_key,":"," term frequency in CIEX Documents", collapse = NULL)
      subtitle <- paste("n = ",n, collapse = NULL)
      windowsFonts(A = windowsFont("Louis George Café"))
      
      # Structuring and Plotting Graph #  
      
      ggplot(search_directory, aes(x=Years, y=Freq., group=1))+
        geom_line(color="#6891C3", size = 1.2) +
        ggtitle(title,subtitle=subtitle) +
        xlab("Year") +
        ylab("Frequency") + 
        theme(text = element_text(size = 15,family = "A"),
              axis.title.x = element_text(margin = margin(t=20,r=0,b=00,l=0)),
              axis.title.y = element_text(margin = margin(t=00,r=20,b=0,l=00)),
              plot.subtitle = element_text(size = 10, margin = margin(t=0,r=0,b=20,l=0)))
    }   
    
      print("Import finished. Ready for term search and graph plotting.")    
    