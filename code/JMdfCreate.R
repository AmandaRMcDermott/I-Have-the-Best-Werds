library(tidyverse)
library(dplyr)
library(tidytext)
library(ggthemes)
get_sentiments("bing")
get_sentiments("afinn")

###Sentiment Analysis
words <- clean_speeches %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

words$length <- NA
words$length <- nchar(words$word)

#####Word Counts
counted <- words %>%    
  count(word, sort = TRUE) %>%
  filter(n > 500) %>%
  mutate(word = reorder(word, n))

counted <- words %>%    
  count(word, sort = TRUE) %>%
  filter(n < 500 & n >200) %>%
  mutate(word = reorder(word, n))

table(clean_speeches$country, clean_speeches$context)

###Word Length Comparison
UNGD_wl <- words %>% 
  filter(context == "UNGD") %>% 
  group_by(country, year) %>% 
  summarise(avg_wl = mean(length, na.rm = TRUE))
boxplot(UNGD_wl$avg_wl)

SOTU_wl <- words %>% 
  filter(context == "SOTU") %>% 
  group_by(country, year) %>% 
  summarise(avg_wl = mean(length, na.rm = TRUE))
boxplot(SOTU_wl$avg_wl, UNGD_wl$avg_wl)

###Sentiment Analysis (Pos-Neg afinn)
UNGD_sentiments <- words %>% 
  filter(context == "UNGD") %>% 
  left_join(get_sentiments("afinn"))

SOTU_sentiments <- words %>% 
  filter(context == "SOTU") %>% 
  left_join(get_sentiments("afinn"))

UNGD_sentiments$score[is.na(UNGD_sentiments$score)] <- 0

UNGD_sentiments_pn <- UNGD_sentiments %>% 
  count(index = year, country, score) %>% 
  spread(score, n, fill = 0) %>% 
  mutate(sentiment = (`-4`*-4 + `-3`*-3+`-2`*-2+`-1`*-1+`1`+`2`*2+`3`*3+`4`*4+`5`*5)/(`-4`+`-3`+`-2`+`-1`+`0`+`1`+`2`+`3`+`4`+`5`))

hist(UNGD_sentiments_pn$sentiment)

UNGD_sentiments_abs <- UNGD_sentiments %>% 
  count(index = year, country, score) %>% 
  spread(score, n, fill = 0) %>% 
  mutate(sentiment = (`-4`*4 + `-3`*3+`-2`*2+`-1`+`1`+`2`*2+`3`*3+`4`*4+`5`*5)/(`-4`+`-3`+`-2`+`-1`+`0`+`1`+`2`+`3`+`4`+`5`))

hist(UNGD_sentiments_abs$sentiment)

SOTU_sentiments$score[is.na(SOTU_sentiments$score)] <- 0

SOTU_sentiments_pn <- SOTU_sentiments %>% 
  count(index = year, country, score) %>% 
  spread(score, n, fill = 0) %>% 
  mutate(sentiment = (`-4`*-4 + `-3`*-3+`-2`*-2+`-1`*-1+`1`+`2`*2+`3`*3+`4`*4+`5`*5)/(`-4`+`-3`+`-2`+`-1`+`0`+`1`+`2`+`3`+`4`+`5`))

hist(SOTU_sentiments_pn$sentiment)

SOTU_sentiments_abs <- SOTU_sentiments %>% 
  count(index = year, country, score) %>% 
  spread(score, n, fill = 0) %>% 
  mutate(sentiment = (`-4`*4 + `-3`*3+`-2`*2+`-1`+`1`+`2`*2+`3`*3+`4`*4+`5`*5)/(`-4`+`-3`+`-2`+`-1`+`0`+`1`+`2`+`3`+`4`+`5`))

hist(SOTU_sentiments_abs$sentiment)

boxplot(SOTU_sentiments_pn$sentiment, UNGD_sentiments_pn$sentiment)
boxplot(SOTU_sentiments_abs$sentiment, UNGD_sentiments_abs$sentiment)

###Economic stuff
economic_sentiment$econ <- 0
economic_sentiment$econ[economic_sentiment$sentiment == "economic"] <- 1
economic_sentiment$secu <- 0
economic_sentiment$secu[economic_sentiment$sentiment == "security"] <- 1
economic_sentiment$envi <- 0
economic_sentiment$envi[economic_sentiment$sentiment == "environmental"] <- 1
economic_word <- economic_sentiment[,c(1,3)]
security_word <- economic_sentiment[,c(1,4)]
environment_word <- economic_sentiment[,c(1,5)]
###Security Analysis
UNGD_security <- words %>% 
  filter(context == "UNGD") %>% 
  left_join(security_word)

UNGD_security$secu[is.na(UNGD_security$secu)] <- 0

UNGD_security <- UNGD_security %>% 
  count(index = year, country, secu) %>% 
  spread(secu, n, fill = 0) %>% 
  mutate(security = `1`/(`0`+`1`))
hist(UNGD_security$security)

SOTU_security <- words %>% 
  filter(context == "SOTU") %>% 
  left_join(security_word)

SOTU_security$secu[is.na(SOTU_security$secu)] <- 0
SOTU_security <- SOTU_security %>% 
  count(index = year, country, secu) %>% 
  spread(secu, n, fill = 0) %>% 
  mutate(security = `1`/(`0`+`1`))
hist(SOTU_security$security)

boxplot(SOTU_security$security, UNGD_security$security)

###Economic Analysis
UNGD_economics <- words %>% 
  filter(context == "UNGD") %>% 
  left_join(economic_word)

UNGD_economics$econ[is.na(UNGD_economics$econ)] <- 0

UNGD_economics <- UNGD_economics %>% 
  count(index = year, country, econ) %>% 
  spread(econ, n, fill = 0) %>% 
  mutate(econ = `1`/(`0`+`1`))
hist(UNGD_economics$econ)

SOTU_economics <- words %>% 
  filter(context == "SOTU") %>% 
  left_join(economic_word)

SOTU_economics$econ[is.na(SOTU_economics$econ)] <- 0

SOTU_economics <- SOTU_economics %>% 
  count(index = year, country, econ) %>% 
  spread(econ, n, fill = 0) %>% 
  mutate(econ = `1`/(`0`+`1`))
hist(SOTU_economics$econ)
boxplot(SOTU_economics$econ, UNGD_economics$econ)


####Creating a clean dataframe.
reg.df <- clean_speeches[,c(1,2)]
var1 <- SOTU_sentiments_pn[,c(1,2,14)]
var1 <- var1 %>% rename(year = index, sotu_sent_pn = sentiment)
var2 <- SOTU_sentiments_abs[,c(1,2,14)]
var2 <- var2 %>% rename(year = index, sotu_sent_ab = sentiment)
var3 <- UNGD_sentiments_pn[,c(1,2,14)]
var3 <- var3 %>% rename(year = index, ungd_sent_pn = sentiment)
var4 <- UNGD_sentiments_abs[,c(1,2,14)]
var4 <- var4 %>% rename(year = index, ungd_sent_ab = sentiment)
var5 <- UNGD_wl[,c(1,2,3)]
var5 <- var5 %>% rename(ungd_wl = avg_wl)
var6 <- SOTU_wl[,c(1,2,3)]
var6 <- var6 %>% rename(sotu_wl = avg_wl)
var7 <- SOTU_economics[,c(1,2,5)]
var7 <- var7 %>% rename(year = index, sotu_econ = econ)
var8 <- UNGD_economics[,c(1,2,5)]
var8 <- var8 %>% rename(year = index, ungd_econ = econ)
var9 <- SOTU_security[,c(1,2,5)]
var9 <- var9 %>% rename(year = index, sotu_secu = security)
var10 <- UNGD_security[,c(1,2,5)]
var10 <- var10 %>% rename(year = index, ungd_secu = security)
reg.df <- reg.df %>% 
  left_join(var1) %>% 
  left_join(var2) %>% 
  left_join(var3) %>% 
  left_join(var4) %>% 
  left_join(var5) %>% 
  left_join(var6) %>% 
  left_join(var7) %>% 
  left_join(var8) %>% 
  left_join(var9) %>% 
  left_join(var10)

reg.df <- reg.df %>% 
  arrange(country, year)

reg.df <- unique(reg.df)

write_csv(reg.df, "JM.csv")

