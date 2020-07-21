
#Reading pdf files into list

setwd("~/R/CIEX/pdfs") #Set Working Directory to folder with all pdfs

files <- list.files(pattern = "pdf$")
ciex_pdfs <- lapply(files, pdf_text)

#Transforming list into VCorpus

corp <- VCorpus(VectorSource(ciex_pdfs))

#Clean-up Process

ciex_stopwords = scan("https://raw.githubusercontent.com/jdallapola/ciex/master/scripts/Text%20Mining/stopwords_CIEX.csv",what = "character")

corp_cleaned <- tm_map(corp,content_transformer(tolower))
corp_cleaned <- tm_map(corp_cleaned, removeWords, c("data",ciex_stopwords, 
                                                    stopwords("pt"),
                                                    stopwords("en"),
                                                    stopwords("es")))

corp_cleaned <- tm_map(corp_cleaned, stemDocument, language = "pt")  
corp_cleaned <- tm_map(corp_cleaned,content_transformer(removePunctuation))
corp_cleaned <- tm_map(corp_cleaned,content_transformer(removeNumbers))

# Pre-defined list of terms

countries <- read.csv("https://raw.githubusercontent.com/jdallapola/ciex/master/scripts/Text%20Mining/country_list_pt.csv", header = FALSE)
countries <-countries$V1%>%
  as.character

countries.dtm <-DocumentTermMatrix(corp_cleaned, control=list(dictionary = countries))

ft <- findFreqTerms(countries.dtm, 
                    lowfreq = 50, 
                    highfreq = Inf)

ft.dtm <- as.matrix(countries.dtm[,ft])

most_frequent_countries = sort(apply(ft.dtm, 2, sum), decreasing = TRUE)

cn_top_10 = as.table(most_frequent_countries)%>%
  data.frame()

cn_top_10 = head(cn_top_10,10)

capFirst <- function(s) {
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

cn_top_10$Var1 <- capFirst(cn_top_10$Var1)

head(cn_top_10)
write.csv(most_frequent_countries, "~/R/CIEX/ciex_online/most_frequent_countries.csv")

lbls <- paste(cn_top_10$Var1)

country_bar = ggplot(cn_top_10, aes(x = reorder(lbls,-Freq),y = Freq))+
  ggtitle("Frequency of Selected Search Terms", subtitle="Top 10 country names in CIEX documents")+
  xlab("Term") +
  ylab("Frequency") + 
  geom_bar(stat="identity",fill ="#6891C3" ) +
  geom_text(aes(x = Var1, y = 300 + (Freq), label = round(Freq, 2)), family = "A")+
  theme(plot.subtitle = element_text(size = 10, margin = margin(t = 0, r = 00, b = 20, l = 0)),
        axis.text.x=element_text(angle = 45, vjust = 1, hjust=1),
        axis.title.x = element_text(margin = margin(t = 10, r = 20, b = 10, l = 0)),
        text = element_text(size = 15, family = "A"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))
        
country_bar

