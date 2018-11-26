library(shiny)
library(tm)
library(wordcloud)
library(memoise)
library(tidyverse)
library(tidytext)

# Global
speeches <- read_csv("~/Documents/R/speeches.csv")

# Use memoise to cache the results
getTermMatrix <- memoise(function(speech) {
  if (!(speech %in% speeches))
    stop("Unknown country")
  
  myCorpus = speeches %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    count(country, year, context, word, sort = T)
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = T)
})
