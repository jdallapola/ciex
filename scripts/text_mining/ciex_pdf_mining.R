    
  print("Calling Required Libraries")
      
  library(pdfsearch)
  library(pdftools)
  library(tm)
  library(stopwords)
  library(dplyr)
  library(stringr)
  library(plyr)
  library(data.table)
  library(ggplot2)
  library(tidyverse)
    
    
    
    print("1/5 Reading pdf files into list")
                
      setwd("~/R/CIEX/pdfs") #Set Working Directory to folder with all pdfs
      
      files <- list.files(pattern = "pdf$")
      ciex_pdfs <- lapply(files, pdf_text)
      
     
      
    print("2/5 Transforming list into VCorpus")
    
      corp <- VCorpus(VectorSource(ciex_pdfs))
    
      
      
    print("3/5 Clean-up Process")
        
        ciex_stopwords = scan("https://raw.githubusercontent.com/jdallapola/ciex/master/scripts/Text%20Mining/Exported_csvs/stopwords_CIEX.csv",what = "character")
        
        corp_cleaned <- tm_map(corp,content_transformer(tolower))
        corp_cleaned <- tm_map(corp_cleaned, removeWords, c("data",ciex_stopwords, 
                                                    stopwords("pt"),
                                                    stopwords("en"),
                                                    stopwords("es")))
    
        corp_cleaned <- tm_map(corp_cleaned, stemDocument, language = "pt")  
        corp_cleaned <- tm_map(corp_cleaned,content_transformer(removePunctuation))
        corp_cleaned <- tm_map(corp_cleaned,content_transformer(removeNumbers))
      
    
      
    print("4/5 Creating Document Term Matrix")
    
        ciex_pdfs.dtm <- DocumentTermMatrix(corp_cleaned, control = list(bounds = list(global = c(3, Inf)))) 
        ft_ciex <- findFreqTerms(ciex_pdfs.dtm, lowfreq = 2000, highfreq = Inf)
        
        #Importing PDF Years List
        
        years <- read.csv("https://raw.githubusercontent.com/jdallapola/ciex/master/scripts/Text%20Mining/Exported_csvs/years_CIEX.csv")
        
        terms_per_year = data.frame(terms = rowSums(as.matrix(ciex_pdfs.dtm)))%>%
          cbind(years)
        terms_per_year = aggregate(terms_per_year$terms,by = list(Years = terms_per_year$x), FUN = sum)  
        
       g_terms_per_year = ggplot(terms_per_year, aes(x=Years, y=x, group=1))+
          geom_line()+ 
          xlab("Year") +
          ylab("Frequency") +
          theme_bw()+
          theme(text = element_text(size = 15, family = "A"),
                axis.title.x = element_text(margin = margin(t=5,r=0,b=00,l=0)),
                axis.title.y = element_text(margin = margin(t=00,r=20,b=0,l=00)),
                plot.subtitle = element_text(size = 10,margin = margin(t=00,r=00,b=10,l=00)))
    
    print("5/6 Creating search and graph plotter tool")
            
        
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
          title <- paste0(search_key,":"," term frequency in CIEX Documents", collapse = NULL)
          cap <- paste("n = ",n, collapse = NULL)
          windowsFonts(A = windowsFont("Louis George Café"))
          
          # Structuring and Plotting Graph #  
          ggplot(search_directory, aes(x=Years, y=Freq., group=1))+
            geom_area(color = "black", fill =alpha("#6891C3",.7), size = .1) +
            labs(caption=cap) + # Remember to set this title
            xlab("Year") +
            ylab("Frequency") +
            theme_bw()+
            theme(text = element_text(size = 15, family = "A"),
                  axis.title.x = element_text(margin = margin(t=5,r=0,b=00,l=0)),
                  axis.title.y = element_text(margin = margin(t=00,r=20,b=0,l=00)),
                  plot.subtitle = element_text(size = 10,margin = margin(t=00,r=00,b=10,l=00)))
        }   
        
     
        print("6/6 Creating country DTM based off of dictionary of selected country names")
        
        # Pre-defined list of terms
        countries <- read.csv("https://raw.githubusercontent.com/jdallapola/ciex/master/scripts/Text%20Mining/Exported_csvs/country_list_pt.csv", header = FALSE)
        countries <-countries$V1%>%
          as.character
        
        # DTM creation 
        countries.dtm <-DocumentTermMatrix(corp_cleaned, control=list(dictionary = countries))
        
        # Removing low frequency terms 
        ft <- findFreqTerms(countries.dtm, lowfreq = 50, highfreq = Inf)
        
        # Writing to new DTM
        ft.dtm <- as.matrix(countries.dtm[,ft])
        
        #Consolidating mentions
        most_frequent_countries = sort(apply(ft.dtm, 2, sum), decreasing = TRUE)
        
        # Separating Top 10 most frequent countries
        cn_top_10 = as.table(most_frequent_countries)%>%
          data.frame()
        
        cn_top_10 = head(cn_top_10,10)
        
        # Capitalizing first letter
        capFirst <- function(s) {paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")}
        cn_top_10$Var1 <- capFirst(cn_top_10$Var1)
        
        write.csv(cn_top_10, "~/R/CIEX/ciex_online/most_frequent_countries.csv")
        
        
        # Creating country name labels
        #lbls <- paste(cn_top_10$Var1)
        
        # Bar graph of 10 most mentioned countries
        #country_bar = ggplot(cn_top_10, aes(x = reorder(lbls,-Freq),y = Freq))+
         # ggtitle("Frequency of Selected Search Terms", subtitle="Top 10 country names in CIEX documents")+
          #xlab("Term") +
          #ylab("Frequency") + 
          #theme_bw()+
          #geom_bar(stat="identity",fill ="grey" ) +
          #geom_text(aes(x = Var1, y = 300 + (Freq), label = round(Freq, 2)), family = "A")+
          #theme(plot.subtitle = element_text(size = 10, margin = margin(t = 0, r = 00, b = 20, l = 0)),
                #axis.text.x=element_text(angle = 45, vjust = 1, hjust=1),
                #axis.title.x = element_text(margin = margin(t = 10, r = 20, b = 10, l = 0)),
                #text = element_text(size = 15, family = "A", color = "black"),
                #axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))
        
        
          print("Import finished. Ready for term search and graph plotting.")    
        
          