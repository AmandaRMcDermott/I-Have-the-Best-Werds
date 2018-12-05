library(tidyverse)
library(tidytext)
library(tm)
library(stringr)
library(gganimate)


speech_words <- clean_speeches%>% 
  unnest_tokens(word, text) %>% 
  count(country, year, context, word, sort = T)


total_speech_words <- speech_words %>% 
  group_by(country, year, context) %>% 
  summarize(total = sum(n))

country_words <- left_join(speech_words, total_speech_words)

#clean_speeches %>% 
#  spread(context, key = context1, value = context2)


speech_tf <- country_words  %>% 
  bind_tf_idf(word, country, n) %>% 
  spread(key = context, value = tf)

test <-speech_tf %>% 
  gather(context, tf, SOTU:UNGD)
test <- subset(test, !is.na(tf))
test1 <- subset(test, context == "SOTU")
test2 <- subset(test, context == "UNGD")

test3 <- test1 %>% left_join(test2, by = c("country", "year", "word"))
test3 <- subset(test3, !is.na(tf.y))

test4 <- test3 %>% 
  left_join(economic_sentiment)
test4 <- subset(test4, !is.na(sentiment))

test5 <- test3 %>% 
  left_join(economic_sentiment)
test5$sentiment[is.na(test5$sentiment)]<- "none"

p1 <- test4 %>% 
  filter(country == "PHL") %>% 
  ggplot(., aes(y = log(tf.y), x = log(tf.x), color = sentiment)) +
  geom_text(aes(label = word), size = 3, hjust = -0.15) +
  geom_point(alpha = 0.5, show.legend = F) +
  geom_abline(slope = 1, intercept = 0) +
  geom_jitter() +
  ggtitle("Philippines") +
  facet_grid(country ~ .) +
  labs(title = 'Year: {frame_time}', x = 'SOTU Frequency', y = 'UNGD Frequnecy') +
  transition_time(year) +
  ease_aes('linear')

animate(p1, fps = 2)

p1 <- test5 %>% 
  filter(country == "PHL") %>% 
  ggplot(., aes(y = log(tf.y), x = log(tf.x), color = sentiment)) +
  geom_text(aes(label = word), size = 3, hjust = -0.15) +
  geom_point(alpha = 0.5, show.legend = F) +
  geom_abline(slope = 1, intercept = 0) +
  geom_jitter() +
  ggtitle("Philippines") +
  facet_grid(country ~ .) +
  labs(title = 'Year: {frame_time}', x = 'SOTU Frequency', y = 'UNGD Frequnecy') +
  transition_time(year) +
  ease_aes('linear')

animate(p1, fps = 2)

p1 <- test4 %>% 
  filter(country == "CHN") %>% 
  ggplot(., aes(y = log(tf.y), x = log(tf.x), color = sentiment)) +
  geom_text(aes(label = word), size = 3, hjust = -0.15) +
  geom_point(alpha = 0.5, show.legend = F) +
  geom_abline(slope = 1, intercept = 0) +
  geom_jitter() +
  ggtitle("Philippines") +
  facet_grid(country ~ .) +
  labs(title = 'Year: {frame_time}', x = 'SOTU Frequency', y = 'UNGD Frequnecy') +
  transition_time(year) +
  ease_aes('linear')

animate(p1, fps = 2)

p1 <- test4 %>% 
  filter(country == "USA") %>% 
  ggplot(., aes(y = log(tf.y), x = log(tf.x), color = sentiment)) +
  geom_text(aes(label = word), size = 3, hjust = -0.15) +
  geom_point(alpha = 0.5, show.legend = F) +
  geom_abline(slope = 1, intercept = 0) +
  geom_jitter() +
  ggtitle("Philippines") +
  facet_grid(country ~ .) +
  labs(title = 'Year: {frame_time}', x = 'SOTU Frequency', y = 'UNGD Frequnecy') +
  transition_time(year) +
  ease_aes('linear')

animate(p1, fps = 2)

p1 <- test4 %>% 
  filter(country == "ZAF") %>% 
  ggplot(., aes(y = log(tf.y), x = log(tf.x), color = sentiment)) +
  geom_text(aes(label = word), size = 3, hjust = -0.15) +
  geom_point(alpha = 0.5, show.legend = F) +
  geom_abline(slope = 1, intercept = 0) +
  geom_jitter() +
  ggtitle("Philippines") +
  facet_grid(country ~ .) +
  labs(title = 'Year: {frame_time}', x = 'SOTU Frequency', y = 'UNGD Frequnecy') +
  transition_time(year) +
  ease_aes('linear')

animate(p1, fps = 2)

p1 <- test4 %>% 
  filter(country == "GHA") %>% 
  ggplot(., aes(y = log(tf.y), x = log(tf.x), color = sentiment)) +
  geom_text(aes(label = word), size = 3, hjust = -0.15) +
  geom_point(alpha = 0.5, show.legend = F) +
  geom_abline(slope = 1, intercept = 0) +
  geom_jitter() +
  ggtitle("Philippines") +
  facet_grid(country ~ .) +
  labs(title = 'Year: {frame_time}', x = 'SOTU Frequency', y = 'UNGD Frequnecy') +
  transition_time(year) +
  ease_aes('linear')

animate(p1, fps = 2)

p1 <- test4 %>% 
  filter(country == "RUS") %>% 
  ggplot(., aes(y = log(tf.y), x = log(tf.x), color = sentiment)) +
  geom_text(aes(label = word), size = 3, hjust = -0.15) +
  geom_point(alpha = 0.5, show.legend = F) +
  geom_abline(slope = 1, intercept = 0) +
  geom_jitter() +
  ggtitle("Philippines") +
  facet_grid(country ~ .) +
  labs(title = 'Year: {frame_time}', x = 'SOTU Frequency', y = 'UNGD Frequnecy') +
  transition_time(year) +
  ease_aes('linear')

animate(p1, fps = 2)



###Amanda old stuff, don't delete maybe?
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