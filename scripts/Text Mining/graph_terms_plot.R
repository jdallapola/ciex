
# Calling Required Elements #

    # Importing PDF Years List
    years <- read.csv("https://raw.githubusercontent.com/jdallapola/ciex/master/years_CIEX.csv")
    
    # Sourcing and Running Text Mining Script (WARNING: Will take a few seconds to run)
    source("https://raw.githubusercontent.com/jdallapola/ciex/master/ciex_PDF_mining.R")        
                                                                                                  
# --------------------------------------------------------------------------------------------------    
    
# User Interface

search_key <- "Portugal" #insert search key here
go_search() # initiate search mechanism and graph plotter

head(search_directory)
# ---------------------------------------------
## Under the hood

go_search <- function()
  
{
  
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
    
    
