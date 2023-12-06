# Hickey Final Project
# Sentiment Toward Palestinians and Israelis During Gaza War on Reddit


# Load necessary libraries
#install.packages("RedditExtractoR")
library("RedditExtractoR")
library(tidyverse)
library(tidytext)
library(wordcloud2)
library(devtools)
library(tidyr)
library(syuzhet)
library(ggplot2)
library(dplyr)

set.seed(1232)
gaza_urls <- find_thread_urls(keywords = "gaza")
head(gaza_urls, 5)

gaza_threads <- get_thread_content(gaza_urls$url)
gaza_thread_data <- gaza_threads$threads
gaza_comments_data <- gaza_threads$comments
gaza_subreddit <-find_subreddits("gaza")

tidy_data <- gaza_thread_data %>%
  unnest_tokens(word, text)

tidy_data <- tidy_data %>%
  anti_join(stop_words)

tidy_data %>%
  count(word, sort = TRUE)

tidy_data <-tidy_data %>%
  filter(!word %in% c("https","amp.theguardian.com","png","amp","comments","www.reddit.com","imgur.com"))

tidy_data <-tidy_data %>%
  filter(!grepl("[0-9]+", word))

t_para <- tidy_data %>%
  count(word, sort = TRUE)

wordcloud2(t_para)

###########################################################
# Filter sentences containing reference to the Palestinians
target_words_p <- c("palestine", "palestinians", "palestinian")
reddit_data_filtered_p <- gaza_comments_data %>%
  filter(str_detect(tolower(comment), paste(target_words_p, collapse = "|")))

# Extract sentences and get sentiment scores
sentiments_p <- reddit_data_filtered_p %>%
  rowwise() %>%
  mutate(sentences = list(get_sentences(comment))) %>%
  unnest(sentences) %>%
  filter(str_detect(tolower(sentences), paste(target_words_p, collapse = "|"))) %>%
  mutate(sentiment = get_sentiment(sentences, method = "syuzhet"))

# View sentiment scores
print(sentiments_p)

# Selecting the last two columns
sentences_sentiment_df_p <- sentiments_p %>%
  select(sentences, sentiment)

#Displaying Sentiment Graphically
# Histogram of sentiment scores
ggplot(sentences_sentiment_df_p, aes(x = sentiment)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  labs(title = "Distribution of Sentiment Scores", x = "Sentiment Score", y = "Frequency") +
  theme_minimal()

# Now for a density plot
ggplot(sentences_sentiment_df_p, aes(x = sentiment)) +
  geom_density(aes(y = after_stat(density)), alpha = 0.5) +
  labs(title = "Density of Sentiment Scores", x = "Sentiment Score", y = "Density") +
  theme_minimal()

sentiments_p$date <- as.Date(sentiments_p$date)

# Calculating the average sentiment score for each date
average_sentiment_by_date_p <- sentiments_p %>%
  group_by(date) %>%
  summarize(average_sentiment = mean(sentiment, na.rm = TRUE))

# Plotting the average sentiment over time
ggplot(average_sentiment_by_date_p, aes(x = date, y = average_sentiment)) +
  geom_line() +  # or geom_point() for individual data points
  labs(title = "Average Sentiment Score Over Time", x = "Date", y = "Average Sentiment Score") +
  theme_minimal()

# Extract sentences and get sentiment scores using Bing method
sentiments_bing <- reddit_data_filtered_p %>%
  rowwise() %>%
  mutate(sentences = list(get_sentences(comment))) %>%
  unnest(sentences) %>%
  filter(str_detect(tolower(sentences), paste(target_words_p, collapse = "|"))) %>%
  mutate(sentiment = get_sentiment(sentences, method = "bing"))

# Classify sentiments as positive, negative, or neutral
sentiments_bing <- sentiments_bing %>%
  mutate(sentiment_type = case_when(
    sentiment > 0 ~ "Positive",
    sentiment < 0 ~ "Negative",
    TRUE ~ "Neutral"
  ))

# Group by date and sentiment type, then count
sentiments_over_time_p <- sentiments_bing %>%
  mutate(date = as.Date(date)) %>%
  group_by(date, sentiment_type) %>%
  summarize(count = n(), .groups = 'drop')

# Plotting the sentiment types over time
ggplot(sentiments_over_time_p, aes(x = date, y = count, color = sentiment_type)) +
  geom_line() +
  labs(title = "Sentiment Analysis Over Time", x = "Date", y = "Count of Sentiments") +
  theme_minimal() +
  scale_color_manual(values = c("Positive" = "green", "Negative" = "red", "Neutral" = "blue"))

######################################################
# Filter sentences containing reference to the Israelis
target_words_i <- c("israel", "israelis", "israeli")
reddit_data_filtered_i <- gaza_comments_data %>%
  filter(str_detect(tolower(comment), paste(target_words_i, collapse = "|")))

# Extract sentences and get sentiment scores
sentiments_i <- reddit_data_filtered %>%
  rowwise() %>%
  mutate(sentences = list(get_sentences(comment))) %>%
  unnest(sentences) %>%
  filter(str_detect(tolower(sentences), paste(target_words_i, collapse = "|"))) %>%
  mutate(sentiment = get_sentiment(sentences, method = "syuzhet"))

# View sentiment scores
print(sentiments_i)

# Selecting the last two columns
sentences_sentiment_df_i <- sentiments_i %>%
  select(sentences, sentiment)

#Displaying Sentiment Graphically
# Histogram of sentiment scores
ggplot(sentences_sentiment_df_i, aes(x = sentiment)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  labs(title = "Distribution of Sentiment Scores", x = "Sentiment Score", y = "Frequency") +
  theme_minimal()

# Now for a density plot
ggplot(sentences_sentiment_df_i, aes(x = sentiment)) +
  geom_density(aes(y = after_stat(density)), alpha = 0.5) +
  labs(title = "Density of Sentiment Scores", x = "Sentiment Score", y = "Density") +
  theme_minimal()

sentiments_i$date <- as.Date(sentiments_i$date)

# Calculating the average sentiment score for each date
average_sentiment_by_date_i <- sentiments_i %>%
  group_by(date) %>%
  summarize(average_sentiment_i = mean(sentiment, na.rm = TRUE))

# Plotting the average sentiment over time
ggplot(average_sentiment_by_date_i, aes(x = date, y = average_sentiment_i)) +
  geom_line() +  # or geom_point() for individual data points
  labs(title = "Average Sentiment Score Over Time", x = "Date", y = "Average Sentiment Score") +
  theme_minimal()

# Extract sentences and get sentiment scores using Bing method
sentiments_bing <- reddit_data_filtered_i %>%
  rowwise() %>%
  mutate(sentences = list(get_sentences(comment))) %>%
  unnest(sentences) %>%
  filter(str_detect(tolower(sentences), paste(target_words_i, collapse = "|"))) %>%
  mutate(sentiment = get_sentiment(sentences, method = "bing"))

# Classify sentiments as positive, negative, or neutral
sentiments_bing <- sentiments_bing %>%
  mutate(sentiment_type = case_when(
    sentiment > 0 ~ "Positive",
    sentiment < 0 ~ "Negative",
    TRUE ~ "Neutral"
  ))

# Group by date and sentiment type, then count
sentiments_over_time_i <- sentiments_bing %>%
  mutate(date = as.Date(date)) %>%
  group_by(date, sentiment_type) %>%
  summarize(count = n(), .groups = 'drop')

# Plotting the sentiment types over time
ggplot(sentiments_over_time_i, aes(x = date, y = count, color = sentiment_type)) +
  geom_line() +
  labs(title = "Sentiment Analysis Over Time", x = "Date", y = "Count of Sentiments") +
  theme_minimal() +
  scale_color_manual(values = c("Positive" = "green", "Negative" = "red", "Neutral" = "blue"))
