library(shiny)
library(tm)
library(wordcloud)
library(memoise)
library(tidyverse)
library(tidytext)
library(shinythemes)
library(readr)

clean_speeches <- read_csv("https://raw.githubusercontent.com/Glacieus/GOVT-696-Project-Jang-McDermott/master/data/clean_speeches.csv")

ctry <<- list("China" = "CHN", "Ghana" = "GHA", "Philippines" = "PHL", "Russia" = "RUS", "United States" = "USA", "South Africa" = "ZAF")
ctry2 <<- list("United States" = "USA", "China" = "CHN", "Ghana" = "GHA", "Philippines" = "PHL", "Russia" = "RUS", "South Africa" = "ZAF")

type_speech <<- list("UN General Debates" = "UNGD", "State of the Union" = "SOTU")
type_speech2 <<- list("UN General Debates" = "UNGD", "State of the Union" = "SOTU")

minyear <<- 1913:2018
maxyear <<- 1913:2018


# Cache the results
getTermMatrix <- memoise(function(ctry, type_speech, minyear = 2000, maxyear = 2018){
  if(missing(type_speech)){
    text <- clean_speeches %>% 
      filter(country == ctry, year >= minyear, year <= maxyear) %>% 
      select(text)
  }
  if(missing(minyear)){
    text <- clean_speeches %>% 
      filter(country == ctry, year >= 2000, year <= maxyear) %>% 
      select(text)
  }
  if(missing(maxyear)){
    text <- clean_speeches %>% 
      filter(country == ctry, year >= minyear, year <= 2018) %>% 
      select(text)
  }
  else{
    text <- clean_speeches %>%
      filter(country == ctry, context == type_speech, year >= minyear, year <= maxyear) %>% 
      select(text)}
  
  myCorpus = Corpus(VectorSource(text))
  
  myDTM = TermDocumentMatrix(
    myCorpus,
    control = list(
      minWordLength = 1,
      wordLengths = c(0, Inf),
      removePunctuation = TRUE,
      removeNumbers = TRUE, stopwords = TRUE
    ))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = T)
})


get_comp_comm <- function(ctry, ctry2, type_speech, type_speech2 = "UNGD", minyear = 2000, maxyear = 2018){
  if(missing(ctry)){
    print("Please enter Country 1")
    if(missing(ctry2))
      print("Please enter Country 2")
  }
  else{
    cloud1 <- clean_speeches %>% 
      filter(country == ctry, context == type_speech, year >= minyear, year <= maxyear) %>% 
      select(text)
    
    cloud2 <- clean_speeches %>% 
      filter(country == ctry2, context == type_speech2, year >= minyear, year <= maxyear) %>% 
      select(text)
    
    pt1 <- Corpus(VectorSource(cloud1))
    pt2 <- Corpus(VectorSource(cloud2))
    
    myDTM1 = TermDocumentMatrix(
      pt1,
      control = list(
        minWordLength = 1,
        wordLengths = c(0, Inf),
        removePunctuation = TRUE,
        removeNumbers = TRUE, stopwords = TRUE
      ))
    
    myDTM2 = TermDocumentMatrix(
      pt2,
      control = list(
        minWordLength = 1,
        wordLengths = c(0, Inf),
        removePunctuation = TRUE,
        removeNumbers = TRUE, stopwords = TRUE
      ))
    
    tdm1 <- myDTM1 %>% 
      as.matrix() 
    
    tdm2 <- myDTM2 %>% 
      as.matrix()
    
    tdm10 <- sort(rowSums(tdm1), decreasing = T)
    tdm20 <- sort(rowSums(tdm2), decreasing = T)
    
    df1 <- rownames_to_column(as.data.frame(tdm10))
    df2 <- rownames_to_column(as.data.frame(tdm20))
    
    full_df <- full_join(df1, df2) %>% 
      mutate(tdm10 = replace_na(tdm10, 0)) %>% 
      mutate(tdm20 = replace_na(tdm20, 0))
    
    tdm <- as.matrix(column_to_rownames(full_df))
    
    name1 <- paste(ctry, type_speech, sep = "-")
    name2 <- paste(ctry2, type_speech2, sep = "-")
    
    colnames(tdm) <- c(name1, name2)
    #par(mfrow = c(1, 2))
    tdm
  }
}