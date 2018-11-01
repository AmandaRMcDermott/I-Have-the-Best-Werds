#Prerequisites
library(dplyr)
library(tidytext)
library(ggthemes)
get_sentiments("bing")
get_sentiments("afinn")

#

tidy_us_sentiments <- tidy_us_words %>% 
  filter(context == "SOTU") %>% 
  left_join(get_sentiments("afinn"))

tidy_us_sentiments$score[is.na(tidy_us_sentiments$score)] <- 0

tidy_us_sentiments <- tidy_us_sentiments %>% 
  count(index = year, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)
  
  

tidy_us_sotu_sentiments <- tidy_us_words %>% 
  filter(context == "SOTU", year > 1970) %>% 
  inner_join(get_sentiments("afinn"))

tidy_us_sotu_sentiments %>% 
  filter(is.na(score))

tidy_us_sentiments$score[is.na(tidy_us_sentiments$score)] <- 0
  
  count(index = year, score) %>% 
  spread(score, n, fill = 0) %>% 
  mutate(sentiment = -5 + `-4`*-4 + `-3`*-3+`-2`*-2+`-1`+`1`+`2`*2+`3`*3+`4`*4+`5`*5)

tidy_us_ungd_sentiments <- tidy_us_words %>% 
  filter(context == "UNGD") %>% 
  inner_join(get_sentiments("afinn")) %>% 
  count(index = year, score) %>% 
  spread(score, n, fill = 0) %>% 
  mutate(sentiment = -5 + `-4`*-4 + `-3`*-3+`-2`*-2+`-1`+`1`+`2`*2+`3`*3+`4`*4+`5`*5)


ggplot(tidy_us_sotu_sentiments, aes(index, sentiment)) +
  geom_col(show.legend = FALSE) +
  geom_smooth()+ theme_economist() +
  ggtitle("US SOTU Sentiments Over Time")
  
ggplot(tidy_us_ungd_sentiments, aes(index, sentiment)) +
  geom_col(show.legend = FALSE) +
  geom_smooth()+ theme_economist() +
  ggtitle("US UNGD Sentiments Over Time")
