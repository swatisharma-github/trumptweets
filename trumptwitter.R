library(foreign)
library(RCurl)

# importing dataset
# source: https://github.com/bpb27
# thanks to Brendan Brown for the data 
twitterData <- read.csv("https://raw.githubusercontent.com/bpb27/political_twitter_archive/master/realdonaldtrump/realdonaldtrump.csv")

library(tidyverse) #tidyverse conflicts with RCurl so wait to load
library(lubridate)

# removing retweets
twitterData <- twitterData %>% 
  subset(is_retweet=="False") %>%                                                # removing retweets
  select(-is_retweet, -source, -in_reply_to_screen_name) %>%                     # removing extra columns
  separate(created_at, c("Date", "Year"), "\\+0000", remove=TRUE) %>%
  separate(Date, c("Day", "Month", "Date", "Time"), " ", remove=TRUE) %>% 
  unite("Date", c("Date", "Month", "Year", "Time"), sep = " ", remove=TRUE) %>% 
  mutate(Date = dmy_hms(Date)) %>%                                               # creating dates
  arrange(Date)                                                                  # order by oldest to newest date

beforeRunning <- twitterData %>%                                                 # creating dataset for pre-campaign tweets
  filter(Date < "2015-06-16 00:00:00") %>%
  mutate(id = row_number())

whileRunning <- twitterData %>%                                                  # creating dataset for tweets during campaign
  filter(Date >= "2015-06-16 00:00:00" & Date < "2017-01-20 09:00:00") %>%
  mutate(id = row_number())

postElection <- twitterData %>%                                                  # creating dataset for post-election tweets
  filter(Date >= "2017-01-20 09:00:00") %>%
  mutate(id = row_number())



# aggression level in response to other tweets?

# generating wordclouds

library(tm)           # text mining
library(SnowballC)    # text stemming
library(wordcloud)    # word-cloud generator



tweetsDuring <- Corpus(VectorSource(whileRunning$text))

tweetsAfter <- Corpus(VectorSource(postElection$text))

tweetsBefore <- Corpus(VectorSource(beforeRunning$text))

#tweetsBefore <- tm_map(tweetsBefore, PlainTextDocument)
tweetsBefore <- tm_map(tweetsBefore, removePunctuation)
tweetsBefore <- tm_map(tweetsBefore, removeWords, stopwords('english'))
tweetsBefore <- tm_map(tweetsBefore, removeWords, c("the"))
tweetsBefore <- tm_map(tweetsBefore, stemDocument)

dtm <- TermDocumentMatrix(tweetsBefore)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

wordcloud(words = d$word, freq = d$freq, max.words=100, random.order = FALSE)