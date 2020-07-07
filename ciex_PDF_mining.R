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
                                               stemming = TRUE,
                                               removeNumbers = TRUE,
                                               bounds = list(global = c(3, Inf)))) 
    