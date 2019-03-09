library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)
library(tidytext)
library(stringr)
library(kableExtra)
library(knitr)


#Load the Coulter tweets
Coulter <- rio::import("Coulter.csv")
Coulter <- janitor::clean_names(Coulter)
colnames(Coulter)
str(Coulter)

# formating the Tweets in kable
 Coulter %>%
  select(created_at, text) %>% 
  kable("html") %>%
  kable_styling("striped")

# Tweets by Month
Coulter_y_Month <- Coulter %>%
  mutate(created_at = ymd_hms(created_at)) %>%
  mutate(yearmon= format(created_at, "%Y-%m")) %>%
  group_by(yearmon) %>%
  count(yearmon)
# plot
ggplot(Coulter_y_Month)+
  aes(x = yearmon, y = n, fill =yearmon) +
  theme_bw()+
  geom_col(show.legend = FALSE) +
  labs( y = "Tweets", x = "Year/Month",
        title = "Ann Coulter Twitter Activity",
        caption = "Source: Twitter, 2018") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
#------------------

# most common words
remove_reg <- "&amp;|&lt;|&gt;"
CommonWords <- Coulter %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]")) %>%
  count(word, sort = T) %>%
  filter(!str_detect(word, "^@|ha")) %>% 
  # by this line, I excluded user mentions, 
  #as well as "ha ha ha" because they are not actually words.
  top_n(15) %>%
  arrange(desc(n))
#plot
ggplot(CommonWords, aes(x = reorder(word, -n), y = n, fill = word))  +
  geom_bar(stat = "identity", show.legend = F) +
  coord_flip() +
  labs(title = "Top Words In Ann Coulter's Twitter Feed", 
       subtitle = "Ann Coulter Twitter Feed",
       caption = "Source: Twitter 2019",
       x="Word",
       y="Count of the word usage") +
  theme_bw()

# Hashatags
Hash <- Coulter %>%
  count(hashtags, sort = T) %>%
  arrange(desc(n)) %>%
  na.exclude()
# clean hashtags
Coulter2 <- Coulter %>%
  select(text, hashtags)
Coulter2$hashtag1 <- gsub("\\(", "", Coulter2$hashtags) 
Coulter2$hashtag1 <- gsub ("\\)", "", Coulter2$hashtag1)
Coulter2$hashtag1 <- gsub ("\"", "&", Coulter2$hashtag1)
Coulter2$hashtag1 <- gsub ("c&&", "", Coulter2$hashtag1)
Coulter2$hashtag1 <- gsub ("&&", "", Coulter2$hashtag1)
# seperate hashtags
Coulter2 <- separate(Coulter2, hashtag1, 
         c('hashtag1', 'hashtag2', 'hashtag3', 'hashtag4'), 
         sep=',', remove=TRUE)

#table with only hashtags 
Coulter2 <- select(Coulter2, hashtag1:hashtag4)
count <- table(unlist(Coulter2))
Hashtags <- as.data.frame(count)
#rename columns
colnames(Hashtags)[1] <- "hashtag"
colnames(Hashtags)[2] <- "count"

#Total by Column, Summarize by String#
hash_df <- Hashtags %>%
  separate_rows(hashtag, sep = ' ') %>%
  group_by(hashtag = tolower(hashtag)) %>%
  summarise(Count = n(), 
            ScoreSum = sum(count))

Hashtags <- select(hash_df, hashtag, ScoreSum) %>%
  arrange(desc(ScoreSum)) %>%
  top_n(15)

# omit the first row and avoid redundants 
Hashtags <- Hashtags[2:16,]
# plot
ggplot(Hashtags, aes(x = reorder(hashtag, -ScoreSum), y = ScoreSum, fill=hashtag))  +
  geom_bar(stat = "identity", show.legend = F) +
  coord_flip() +
  labs(title = "Top 15 hashtags In Ann Coulter Twitter Feed", 
       subtitle = "Ann Coulter Twitter Feed",
       caption = "Source: Twitter 2019",
       x="Word",
       y="Count of the hashtag usage") +
  
  theme_bw()
# ----------------------------Analysis------------------
#Couler Twitter activity increased significantly in October and November
# wwhat was she tweeting about in during this period
remove_reg <- "&amp;|&lt;|&gt;"
OctNov <- Coulter %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]")) %>%
  mutate(created_at = month(created_at, label = T)) %>%
  filter(created_at == c("Oct", "Nov")) %>%
  group_by(created_at) %>%
  count(word, sort = T) %>%
  top_n(10) %>%
  arrange(desc(n))
#plot
ggplot(OctNov)+
  aes(x = reorder(word,-n), y= n, fill= created_at)+
  geom_col()+
  theme_bw()+
  coord_flip()+
  labs(
    title = "Ann Coulter top Subjects During October/Novemebr",
    y = "Count",
    x  = "word",
    legend = "Month"
  )
#----
# how often does she retweet others?
#retweet count VS actual tweets
ggplot(Coulter)+
  aes(x = is_retweet, fill = is_retweet)+
  geom_histogram(stat = "count", show.legend = F)+
  theme_bw()+
  labs(title = "Ann Coulter Tweets vs retweets", 
       subtitle = "Ann Coulter Twitter Feed",
       caption = "Source: Twitter 2019",
       x="Retweets",
       y="Tweets") 
#----
# how much engagement (likes) does she get over time?
# favourtie count
Fav <- Coulter %>%
  select(screen_name, favorite_count, created_at) %>%
  filter(favorite_count > 0) %>%
  mutate(created_at = ymd_hms(created_at)) %>%
  mutate(yearmon= format(created_at, "%Y-%m")) %>%
  group_by(yearmon) %>%
  arrange(desc(favorite_count))
#plot
ggplot(Fav)+
  aes(x = yearmon, y = favorite_count, fill = yearmon)+
  geom_col(show.legend = F) +
  theme_bw()+
  labs(title = "Ann Coulter Tweeter engagement", 
       subtitle = "Ann Coulter Twitter Feed",
       caption = "Source: Twitter 2019",
       x="Year/Month",
       y="Tweets Favorites") 

# quoted_names
quotes <- Coulter %>%
  count(quoted_name, sort = T) %>%
  na.exclude() %>%
  top_n(20)
ggplot(quotes)+
  aes(x = reorder(quoted_name,-n), y = n, fill = quoted_name)+
  geom_col(show.legend = F) +
  coord_flip()+
  labs(title = "Ann Coulter Quoted Sources", 
       subtitle = "Ann Coulter Twitter Feed",
       caption = "Source: Twitter 2019",
       x="Quoted Name",
       y="Count") 

#------------------measuring the engagement----------------------

# this code shows people and entities that coulter comment on the most on Twitter
quotes <- Coulter %>%
  count(quoted_name, sort = T) %>%
  na.exclude() %>%
  top_n(20)
ggplot(quotes)+
  aes(x = reorder(quoted_name,-n), y = n, fill = quoted_name)+
  geom_col(show.legend = F) +
  coord_flip()+
  labs(title = "Ann Coulter Quoted Sources", 
       subtitle = "Ann Coulter Twitter Feed",
       caption = "Source: Twitter 2019",
       x="Quoted Name",
       y="Count") 
# this code shows us who is engaging her in their conversations, soupposing she relpies to those who mention her on Twitter
engagement1 <- Coulter %>%
  count(reply_to_screen_name)%>%
  arrange(reply_to_screen_name)

#this code shows who she is trying to intreact with the most (as an ally)
mentions <- Coulter %>%
  count(mentions_screen_name, sort = T) %>%
  arrange(desc(n)) %>%
  na.exclude() %>%
  top_n(10)
ggplot(mentions)+
  aes(x = reorder(mentions_screen_name, -n), y = n, fill = mentions_screen_name)+
  geom_col(show.legend = F)+
  theme_bw()+
  coord_flip()+
  labs(title = "Coulter's Most Common Name Mentions on Twitter", y = "Count", x = "Name"  )
# this code shows  how many times Coulter's Tweets whave been liked by others
engagement2 <- Coulter %>%
  select(favorite_count, created_at) %>%
  filter(favorite_count >0) %>%
  mutate(created_at = ymd_hms(created_at)) %>%
  mutate(yearmon= format(created_at, "%Y-%m")) %>%
  group_by(yearmon) %>%
  arrange(favorite_count)
ggplot(engagement2)+
  aes(x = yearmon, y = favorite_count, fill= yearmon)+
  geom_col(show.legend = F)+
  labs(title = "Times Coulter's Tweets whave been liked by Ohers")
# this code shows many time Coulter's Tweets have been retweeted by others
engagement3 <- Coulter %>%
  select(retweet_count, created_at) %>%
  mutate(created_at = ymd_hms(created_at)) %>%
  mutate(yearmon= format(created_at, "%Y-%m")) %>%
  group_by(yearmon) %>%
  arrange(retweet_count)
ggplot(engagement3)+
  aes(x = yearmon, y = retweet_count, fill= yearmon)+
  geom_col(show.legend = F)+
  labs(title = "Coulter's Retweets Count")
