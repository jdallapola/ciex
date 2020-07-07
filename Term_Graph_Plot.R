# Cleaning Work Environment #

rm(list = ls())

# Calling Required Elements #

    # Years List
    years = read.csv("https://raw.githubusercontent.com/jdallapola/ciex/master/years_CIEX.csv")
    
    # Sourcing and Running Text Mining Script
    source("https://raw.githubusercontent.com/jdallapola/ciex/master/ciex_PDF_mining.R")        
                                                                                                  
# --------------------------------------------------------------------------------------------------    
    
# User Interface

search_key <- "" #insert search key here
go_search() # initiate search mechanism and graph plotter










# ---------------------------------------------
## Under the hood

go_search <- function()
  
{
  
    search_directory = as.matrix(ciex_pdfs.dtm)%>%
    data.frame()%>%
    select(search_key)%>%
    data.frame()
  
  search_directory$years = years
  search_directory = setNames(search_directory,c("Freq.","Years"))
  search_directory = setNames(aggregate(search_directory$Freq., by = search_directory$Years, FUN = sum), c("Years","Freq."))
  
  # Graph elements #
  n = sum(search_directory$Freq.)  
  title = paste("Frequency of the word"," '",search_key,"' ","in CIEX Documents", sep = " ", collapse = NULL)
  subtitle = paste("n = ",n,"
                     ", sep = "", collapse = NULL)
  windowsFonts(A = windowsFont("Louis George Café"))
  
  # Structuring Graph #  
  
  p <- ggplot(search_directory, aes(x=Years, y=Freq., group=1))+
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
