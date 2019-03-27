#load packages

library(dplyr)
library(tidytext)
library(tm)
library(wordcloud)

#load dataset
load("climate_text.rda")

#explore the dataset
glimpse(climate_text)
head(climate_text)

# Pipe the climate_text dataset and seperate text into individual words
climate_tidy <- climate_text %>%
  unnest_tokens(word, text)

climate_tidy <- climate_tidy %>% 
  anti_join(stop_words) %>%
  filter(!word %in% c("climate", "change")) %>%
  count(word, sort = TRUE)

#wordcloud
wordcloud(climate_tidy$word, max.words = 75)

#count words by station
climate_tidy %>%
  count(station) %>%
  rename(station_total = n)

#inner_join with nrc
climate_sentiment <- climate_tidy %>% 
  group_by(station) %>% 
  mutate(station_total = n()) %>%
  ungroup() %>%
  inner_join(get_sentiments("nrc"))

#percentage of negative sentiment
climate_sentiment %>% 
  count(station, sentiment, station_total) %>%
  mutate(percent = n / station_total) %>%
  filter(sentiment == "negative") %>%
  arrange(percent)

#percentage of positive sentiment
climate_sentiment %>%
  count(station, sentiment, station_total) %>%
  mutate(percent = n / station_total) %>%
  filter(sentiment == "positive") %>%
  arrange(percent)

#load in ggplot2
library(ggplot2)

#graph positive vs. negative  
climate_sentiment %>%
  count(station, sentiment, station_total) %>%
  mutate(percent = 100*n / station_total) %>%
  ggplot(aes(x = sentiment, y = percent, fill = sentiment)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~station)

# Top 10 words for each sentiment
climate_sentiment %>%
  count(word, sentiment) %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip()

# Top 10 words for each station
climate_sentiment %>%
  filter(sentiment == "negative") %>%
  count (word, station) %>%
  group_by(station) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(paste(word, station, sep = "__"), n)) %>%
  ggplot(aes(x = word, y = n, fill = station)) +
  geom_col(show.legend = FALSE) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  facet_wrap(~ station, nrow = 2, scales = "free") +
  coord_flip()

library(lubridate)

sentiment_by_time <- climate_tidy %>%
  mutate(date = floor_date(show_date, unit = "6 months")) %>%
  group_by(date) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  inner_join(get_sentiments("nrc"))

#sentiment over time
sentiment_by_time %>%
  filter(sentiment == "positive" | sentiment == "negative") %>%
  count(date, sentiment, total_words) %>%
  ungroup() %>%
  mutate(percent = n / total_words) %>%
  ggplot(aes(x= date, y = percent, color = sentiment)) +
  geom_line(size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, lty = 2) +
  expand_limits(y = 0)

#sentiment over time - just Fox News
sentiment_by_time %>%
  filter(station == "MSNBC", sentiment == "positive" | sentiment == "negative") %>%
  count(date, sentiment, total_words) %>%
  ungroup() %>%
  mutate(percent = n / total_words) %>%
  ggplot(aes(x= date, y = percent, color = sentiment)) +
  geom_line(size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, lty = 2) +
  expand_limits(y = 0)

#over time for select words
sentiment_by_time %>%
  mutate(date = floor_date(show_date, unit = " 1 month")) %>%
  filter(word %in% c("threat", "hoax", "denier",
                     "real", "warming", "hurricane")) %>%
  count(date, word) %>%
  ungroup() %>%
  ggplot(aes(x = date, y = n, color = word)) +
  facet_wrap(~word) +
  geom_line(size = 1.5, show.legend = FALSE) +
  expand_limits(y = 0)

