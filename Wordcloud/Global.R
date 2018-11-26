library(shiny)
library(tm)
library(wordcloud)
library(memoise)
library(tidyverse)
library(tidytext)

# Global
speeches <- read_csv("https://raw.githubusercontent.com/Glacieus/GOVT-696-Project-Jang-McDermott/master/data/speeches.csv")
countries <<- list("China" = "CHN", "Ghana" = "GHN", "Phillipines" = "PHL", "Russia" = "RUS", "United States" = "USA", "South Africa" = "ZAF")

# Make each row a word and remove stop words
word_speeches <- speeches %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

# Put word back into speech format - now each row is a speech
clean_speeches <- word_speeches %>% 
  nest(word) %>% 
  mutate(text = map(data, unlist),
         text = map_chr(text, paste, collapse = " "))


# Cache the results
getTermMatrix <- memoise(function(country){
  if (!(country %in% countries))
    stop("Unkown country")
  
  myCorpus = Corpus(VectorSource(clean_speeches))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus)
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = T)
})