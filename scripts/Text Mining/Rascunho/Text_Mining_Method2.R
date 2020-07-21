# Following instructions at https://www.tidytextmining.com/tidytext.html #

library(dplyr)
library(tidytext)

corp_td <- tidy(corp)
corp_td
corp_tokens <- corp_td %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")

# most common words

most_common <- corp_tokens %>%
  count("word")

View(most_common)

library(corpus)


data = corpus_frame(corp)

install.packages("corpus")

test = text_locate(corp, "Chile")

test
View(corp)
help(text_locate)

