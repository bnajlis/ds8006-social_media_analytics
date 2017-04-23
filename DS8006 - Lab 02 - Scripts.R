install.packages("twitteR")
install.packages("lubridate")
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")

library(twitteR)
library(httr)
library(rjson)
library(tm)
library(lubridate)
library(stringr)
library(wordcloud)


# Oauth authentication with API key and token
consumer_key <- "pY9vu9ZlKAHIVjO79cNAMH8zy"
consumer_secret <- "0TZZl6UW7UYDrt3sh27wB5qKZArrvhSHmw1aDsIz7xOa9vXCKv"
access_token <- "824392227749298177-UfnBjHAkOaJG6wRIvJye4gN6MOAWp8c"
access_secret <- "Byom7w6p6IUMcrwwKY0jhUCvHGBxMcC5fUZuOash0EHtX"

# Twitter oauth
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#Remove punctuation, numbers, html-links and unecessary spaces:
textScrubber <- function(dataframe) {
  
  dataframe$text <-  gsub("-", " ", dataframe$text)
  dataframe$text <-  gsub("&", " ", dataframe$text)
  dataframe$text <-  gsub("#", "\002", dataframe$text)
  dataframe$text <-  gsub("[[:punct:]]", " ", dataframe$text)   # Task 3: Keep hashtags
  dataframe$text <-  gsub("[[:digit:]]", "", dataframe$text)
  dataframe$text <-  gsub("http\\w+", "", dataframe$text)
  dataframe$text <-  gsub("\n", " ", dataframe$text)
  dataframe$text <-  gsub("^\\s+|\\s+$", "", dataframe$text)
  
  dataframe$text <-  tolower(dataframe$text)
  
  # Additional statements for Lab tasks


  dataframe$text <-  gsub("jfdcxlds", " ", dataframe$text)  # Task 1: remove noise words
  dataframe$text <-  gsub("tkjhkz", " ", dataframe$text)    # Task 1: remove noise words
  dataframe$text <-  gsub("amp", " ", dataframe$text)       # Task 1: remove noise words
  dataframe$text <-  gsub(""", " ", dataframe$text)         # Task 1: remove noise words
  dataframe$text <-  gsub("\002", "#", dataframe$text)      # Task 3: Keep hashtags

  dataframe$text <-  gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", dataframe$text)  # Task 2: remove words shorter than 3 chars
  dataframe$text <-  gsub("[^0-9A-Za-z#///' ]", " ", dataframe$text) # Task 1: remove noise characters    
  return(dataframe)
}

clinton_tweets <- userTimeline(user="@HillaryClinton", n=200, includeRts = FALSE, retryOnRateLimit=2000)
trump_tweets <- userTimeline(user="@realDonaldTrump", n=200, includeRts = FALSE, retryOnRateLimit=2000)

# Convert list to data.frame
clinton_tweets <- twListToDF(clinton_tweets)
trump_tweets <- twListToDF(trump_tweets)

# Scrub the tweets and do Task 1, 2 and part of task 3
clinton_tweets <- textScrubber(clinton_tweets)
trump_tweets <- textScrubber(trump_tweets)

# Task 3: Keep hashtags
hashtag.regex <- perl("(?<=^|\\s)#\\S+")
clinton_tweets_hashtags <- str_extract_all(clinton_tweets$text, hashtag.regex)  # remove everything that is not a hashtag
clinton_tweets_hashtags <- clinton_tweets_hashtags[lapply(clinton_tweets_hashtags,length)>0]  # remove empty rows that don't have any hashtag
clinton_tweets_hashtags <- unlist(clinton_tweets_hashtags, recursive = TRUE)  #uwind items that contain more than one hashtag per tweet into unified list

trump_tweets_hashtags<- str_extract_all(trump_tweets$text, hashtag.regex)
trump_tweets_hashtags <- trump_tweets_hashtags[lapply(trump_tweets_hashtags,length)>0]
trump_tweets_hashtags <- unlist(trump_tweets_hashtags, recursive = TRUE)

tdmCreator <- function(dataframe, stemDoc = T, rmStopwords = T){
  
  tdm <- Corpus(VectorSource(dataframe$text))
  if (isTRUE(rmStopwords)) {
    tdm <- tm_map(tdm, removeWords, stopwords())
  }
  if (isTRUE(stemDoc)) {
    tdm <- tm_map(tdm, stemDocument)
  }
  tdm <- TermDocumentMatrix(tdm,
                            control = list(wordLengths = c(1, Inf)))
  tdm <- rowSums(as.matrix(tdm))
  tdm <- sort(tdm, decreasing = T)
  df <- data.frame(term = names(tdm), freq = tdm)
  return(df)
}

# Task 4: Create word cloud
wordcloud(clinton_tweets$text, min.freq=3)
wordcloud(trump_tweets$text, min.freq=3)



