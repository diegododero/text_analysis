library(RCurl)
library(XML)
library(jsonlite)
library(tidytext)
library(dplyr)
library(ggplot2)
library(tidyr)

genres <- "dubstep"
tracks = rbind(NULL, c())
for (i in 0:249){
  print(paste0("Page ", i))
  url <- "https://api.soundcloud.com/tracks?client_id=6QvdRwJ2FQEAWlMafWqRjnI9hsdVKNeE&limit=200"
  url <- paste0(url, "&offset=", i, "&genres=", genres)
  response          <- getURL(url)
  response          <- fromJSON(response)
  #Tracks must have at most 8 minutes, ignore sets and mixes
  tracks <- rbind(tracks, select(response, title, created_at, duration))
}

length <- length(tracks)
tracks_df <- data_frame(linenumber = 1:length, track = tracks$title)

colnames(tracks) <- c("title", "created_at", "duration")
write.csv(tracks, file = paste0(genres, ".csv"), quote = FALSE)
?write.csv

tidy_tracks <- tracks_df %>%
  group_by(track) 

tidy_tracks_words <- tidy_tracks %>%
  unnest_tokens(word, track)

tracks_stop_words <- data_frame(word = c("free", "remix", "dj", "download", "mix", "bass", "feat", "trance"))

tidy_tracks_words <- tidy_tracks_words %>%
  anti_join(stop_words)

tidy_tracks_words <- tidy_tracks_words %>%
  anti_join(tracks_stop_words)

tracks_df %>%
  unnest_tokens(word, track) %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

tracks_sentiment <- tidy_tracks_words %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, index = linenumber, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = ifelse(positive > 0, "positive", "negative"))

tracks_sentiment_2 <- tidy_tracks_words %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, index = linenumber, sentiment)

tracks_sentiment_afinn <- tidy_tracks_words %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, index = linenumber, score)

tracks_sentiments_plot <- tracks_sentiment_2 %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  ggplot(aes(sentiment, word_count, fill = -word_count)) +
  geom_col(show.legend = FALSE)
tracks_sentiments_plot

tracks_sentiments_afinn_plot <- tracks_sentiment_afinn %>%
  group_by(score) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  ggplot(aes(score, word_count, fill = -word_count)) +
  geom_col(show.legend = FALSE)
tracks_sentiments_afinn_plot

tracks_sentiment_afinn$score %>%
  sd()
