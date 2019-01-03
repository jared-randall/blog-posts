#install and loading packages
install.packages("twitteR")
library(ggplot2)
library(dplyr)

#Twitter Authentication
TWconsumerAPI_key <- "JCKkQX9mCDaCQQ8amV26TXMUZ"
TWconsumerAPI_secretKey <- "EUAXlWHZDcxwxtREKavEy4CK0pYO9r4iZr67b40EQ4gBXED95i"
  
TWaccessAPI_token <- "411946161-AVZnQ0EPjiN71reWMhiCQSETAuo77p160r7Bm9Zn"
TWaccessAPI_secretToken <- "KLBf1IrBaUIK7rlJXgzmqIql1HRjMD59bngp7SThcg08U"

setup_twitter_oauth(TWconsumerAPI_key, TWconsumerAPI_secretKey, TWaccessAPI_token, TWaccessAPI_secretToken)

#import 1000 tweets featuring the hashtag '#RoseBowl'

ht_rosebowl_2018 <- searchTwitter('#RoseBowl', n = 5000, since = '2018-01-02')

ht_rosebowl_2018_df <- twListToDF(ht_rosebowl_2018)

#Structure
str(ht_rosebowl_2018_df)

#Unique Tweets Only
ht_rosebowl_unique <- ht_rosebowl_2018_df %>% 
    filter(!isRetweet) %>% 
    mutate(text = sub("https?://[\\w\\./]+", "", text))

#top ten most active users
TenMostActiveUsers <- ht_rosebowl_unique %>%
  count(screenName, sort = TRUE) %>%
  slice(1:10)

#top ten most active users graphed
TenMostActiveUsers %>%
  ggplot(aes(x = reorder(screenName, n, function(n) n), y = n)) +
  ylab("# of Tweets") +
  xlab("") +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 10, 1)) +
  ggtitle("Top 10 Most active twitter users during RoseBowl 2018") +
  theme(plot.title = element_text(hjust = 0.5))

