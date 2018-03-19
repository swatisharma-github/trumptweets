library(foreign)
library(RCurl)

# importing dataset
# source: https://github.com/bpb27
# thanks to Brendan Brown for the data 
twitterData <- read.csv("https://raw.githubusercontent.com/bpb27/political_twitter_archive/master/realdonaldtrump/realdonaldtrump.csv")

library(tidyverse) #tidyverse conflicts with RCurl so wait to load
library(lubridate)

# removing retweets
twitterData <- twitterData %>% subset(is_retweet=="False") %>% select(-is_retweet, -source, -in_reply_to_screen_name) %>%
  separate(created_at, c("Date", "Year"), "\\+0000", remove=TRUE) %>%
  separate(Date, c("Day", "Month", "Date", "Time"), " ", remove=TRUE) %>% 
  unite("Date", c("Date", "Month", "Year", "Time"), sep = " ", remove=TRUE) %>%
  mutate(Date = dmy_hms(Date)) %>% 
  arrange(Date) 

postElection <- twitterData %>% 
  filter(Date >= "2017-01-20 09:00:00") %>%
  mutate(id = row_number())



# aggression level in response to other tweets?

