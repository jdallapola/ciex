# Pre-defined list of terms

countries <- read.csv("C:/Users/jdall/Documents/R/CIEX/ciex_online/scripts/Text Mining/country_list_pt.csv")
countries <-countries$cn%>%
    as.character

countries.dtm <-DocumentTermMatrix(corp, control=list(dictionary = countries,
                                                        removePunctuation = TRUE,
                                                        tolower = TRUE,
                                                        removeNumbers = TRUE))

ft <- findFreqTerms(countries.dtm, 
                    lowfreq = 50, 
                    highfreq = Inf)

ft.dtm <- as.matrix(countries.dtm[,ft])

most_frequent_countries = sort(apply(ft.dtm, 2, sum), decreasing = TRUE)


write.csv(most_frequent_countries, "~/R/CIEX/ciex_online/most_frequent_countries.csv")

#countries_df <- data.frame(docs = countries.dtm$dimnames$Docs, as.matrix(countries.dtm), row.names = NULL)
#View(countries_df)

