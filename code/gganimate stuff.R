library(tidyverse)
library(tidytext)
library(tm)
library(stringr)
library(textmineR)
library(gganimate)
library(tweenr)
library(wordcloud)
library(reshape2)
library(memoise)

speech_words <- clean_speeches%>% 
  unnest_tokens(word, text) %>% 
  count(country, year, context, word, sort = T) %>% 
  ungroup() %>% 
  inner_join(get_sentiments("afinn"), by = c(word = "word"))


total_speech_words <- speech_words %>% 
  group_by(country, year, context, word) %>% 
  summarize(total = sum(n))

country_words <- left_join(speech_words, total_speech_words)

clean_speeches %>% 
  spread(context, key = context1, value = context2)


speech_tf <- country_words  %>% 
  bind_tf_idf(word, country, n) %>% 
  spread(key = context, value = tf)

speech_tf$SOTU[is.na(speech_tf$SOTU)] <- 0
speech_tf$UNGD[is.na(speech_tf$UNGD)] <- 0

speech_tf1 <- speech_tf %>% 
  select(country, year, word, SOTU)

speech_tf2 <- speech_tf %>% 
  select(country, year, word, UNGD)

peak <- speech_tf1 %>% 
  left_join(speech_tf2)

temp <- subset(speech_tf, country == "CHN" & year == "2007" & word == "improve")


p1 <- speech_tf %>% 
  filter(country == "CHN") %>% 
  ggplot(., aes(y = SOTU, x = UNGD, color = country)) +
  #geom_text(aes(label = word), size = 3, hjust = -0.15) +
  geom_point(alpha = 0.5, show.legend = F) +
  geom_jitter() +
  facet_grid(context ~ country) +
  labs(title = 'Year: {frame_time}', x = 'count', y = 'total frequency') +
  transition_time(year) +
  ease_aes('linear')

animate(p1)

p2 <- speech_tf %>% 
  filter(country == "RUS", context == "UNGD") %>% 
  ggplot(., aes(y = tf, x = n, color = country)) 
geom_text(aes(label = word), size = 3, hjust = -0.15) +
  geom_point(alpha = 0.5, show.legend = F) +
  geom_jitter() +
  facet_grid(context ~ country) +
  labs(title = 'Year: {frame_time}', x = 'count', y = 'total frequency') +
  transition_time(year) +
  ease_aes('linear')

animate(p2)

p3 <- speech_tf %>% 
  filter(country == "USA") %>% 
  ggplot(., aes(y = tf, x = n, color = country)) +
  geom_text(aes(label = word), size = 3, hjust = -0.15) +
  geom_point(alpha = 0.5, show.legend = F) +
  geom_jitter() +
  facet_grid(context ~ country) +
  labs(title = 'Year: {frame_time}', x = 'count', y = 'total frequency') +
  transition_time(year) +
  ease_aes('linear')

animate(p3)

p4 <- speech_tf %>% 
  filter(country == "ZAF") %>% 
  ggplot(., aes(y = tf, x = n, color = country)) +
  geom_text(aes(label = word), size = 3, hjust = -0.15) +
  geom_point(alpha = 0.5, show.legend = F) +
  geom_jitter() +
  facet_grid(context ~ country) +
  labs(title = 'Year: {frame_time}', x = 'count', y = 'total frequency') +
  transition_time(year) +
  ease_aes('linear')

animate(p4)

p5 <- speech_tf %>% 
  filter(country == "PHL") %>% 
  ggplot(., aes(y = tf, x = n, color = country)) +
  geom_text(aes(label = word), size = 3, hjust = -0.15) +
  geom_point(alpha = 0.5, show.legend = F) +
  geom_jitter() +
  facet_grid(context ~ country) +
  labs(title = 'Year: {frame_time}', x = 'count', y = 'total frequency') +
  transition_time(year) +
  ease_aes('linear')

animate(p5)

p6 <- speech_tf %>% 
  filter(country == "GHA") %>% 
  ggplot(., aes(y = tf, x = n, color = country)) +
  geom_text(aes(label = word), size = 3, hjust = -0.15) +
  geom_point(alpha = 0.5, show.legend = F) +
  geom_jitter() +
  facet_grid(context ~ country) +
  labs(title = 'Year: {frame_time}', x = 'count', y = 'total frequency') +
  transition_time(year) +
  ease_aes('linear')

animate(p6)