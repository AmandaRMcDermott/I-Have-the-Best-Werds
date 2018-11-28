###Word Frequencies
library(tidyverse)
library(tidytext)
library(ggplot2)
data(stop_words)

#Tidying it up
tidy_words <- speeches %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

tidy_words %>% 
  count(word, sort = TRUE)

##Removing NAs
tidy_words <- tidy_words %>% 
  filter(!is.na(word))

###Graphs
speech_words <- speeches %>%
  unnest_tokens(word, text) %>%
  count(country, year, context, word, sort = TRUE) %>%
  ungroup()

total_words <- speech_words %>% 
  group_by(country, year, context) %>% 
  summarize(total = sum(n))

speech_words <- left_join(speech_words, total_words)
speech_words


###Nasty one
speech_words <- speech_words %>%
  bind_tf_idf(word, country, n)
speech_words


china_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(country) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = country)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~country, ncol = 2, scales = "free") +
  coord_flip()
