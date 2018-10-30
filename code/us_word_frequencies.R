#Prerequisites
library(tidyverse)
library(tidytext)
library(ggplot2)
data(stop_words)

#Making a new dataframe to work with to keep the original clean.
speeches <- us_speeches

#Tidying it up
tidy_us_words <- speeches %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

tidy_us_words %>% 
  count(word, sort = TRUE)

##Removing NAs
tidy_us_words <- tidy_us_words %>% 
  filter(!is.na(word))

##Uploading
write_csv(tidy_us_words, "tidy_us_words.csv")

whatsup <- subset(tidy_speeches, is.na(text))
###Graphs
tidy_us_words %>%
  filter(context == "SOTU", year > 1969) %>% 
  count(word, sort = TRUE) %>%
  filter(n > 300) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ggtitle("US SOTU speeches (1969+)")


tidy_us_words %>%
  filter(context == "UNGD") %>% 
  count(word, sort = TRUE) %>%
  filter(n > 200) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ggtitle("US UNGD speeches (1969+)")

