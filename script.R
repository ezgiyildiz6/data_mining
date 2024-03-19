source("require_packages.R")
require_packages(c(
  "xml2",
  "httr",
  "tidytext",
  "magrittr",
  "dplyr",
  "readr",
  "tidyr",
  "ggplot2",
  "wordcloud"
))


# HTTP GET Request
cnn_rss <- GET("http://rss.cnn.com/rss/cnn_latest.rss")
writeLines(content(cnn_rss, as = "text"), "myxml.xml")


my_doc <- read_xml("myxml.xml")

xml_structure(my_doc)

xml_find_all(my_doc, ".//item")
number_of_items <- length(xml_find_all(my_doc, ".//item"))
news <- number_of_items
lastBuildDate <- xml_find_all(my_doc, ".//lastBuildDate")
last_build_date <- xml_text(lastBuildDate)
cat("There are",news,"news on CNN World RSS feed on",last_build_date,".")

description <- xml_find_all(my_doc, ".//description")
content_of_the_news <- xml_text(description[-1])
content_of_the_news

# Creating a data frame with the descriptions
description_numbers <- seq(1,number_of_items)
description_dataframe <- data.frame(description_number = description_numbers, 
                                    text = content_of_the_news)

#Tokenisation of the file
(tokenised <- description_dataframe %>% unnest_tokens(token, text))
(word_counts <- tokenised %>% 
    count(token))

################################################################################
#Sentiment analysis - NRC
nrc_sentiment_counts <- nrc %>%
  inner_join(word_counts, by = c("word" = "token")) %>% 
  group_by(sentiment) %>%
  summarise(n = sum(n))

nrc_graph <- nrc_sentiment_counts %>% 
  ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) + 
  labs(title = "Frequency of NRC sentiments in CNN World RSS feed URL")
print(nrc_graph)

################################################################################
#Word Cloud
word_count <- tokenised %>% unnest(token) %>% count(token)
top_100 <- top_n(word_count, 100 , wt = n)
wordcloud(top_100$token, top_100$n)
