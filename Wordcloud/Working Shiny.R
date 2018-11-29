library(shiny)
library(tm)
library(wordcloud)
library(memoise)
library(tidyverse)
library(tidytext)

# Global
speeches <- read_csv("https://raw.githubusercontent.com/Glacieus/GOVT-696-Project-Jang-McDermott/master/data/speeches.csv")

# Make each row a word and remove stop words
word_speeches <- speeches %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

# Put word back into speech format - now each row is a speech
clean_speeches <- word_speeches %>% 
  nest(word) %>% 
  mutate(text = map(data, unlist),
         text = map_chr(text, paste, collapse = " ")) %>% 
  select(country, year, context, text)

ctry <<- list("China" = "CHN", "Ghana" = "GHA", "Phillipines" = "PHL", "Russia" = "RUS", "United States" = "USA", "South Africa" = "ZAF")

type_speech <<- list("UN General Debates" = "UNGD", "State of the Union" = "SOTU")

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


# Define ui
ui <- fluidPage(
  # App title
  headerPanel("Speeches Wordclouds"),
  
  # Sidebar for inputs
  sidebarPanel(
    selectInput("selection", "Choose a country:",
                choices = ctry),
    actionButton("update", "Change"),
    hr(),
    radioButtons("type", "Type of Speech",
                 choices = type_speech),
    sliderInput("yrs", "Years", 1913, 2018, value = c(2000, 2014)),
    sliderInput(
      "freq",
      "Min Freq:",
      min = 1,
      max = 50,
      value = 15
    ),
    sliderInput(
      "max",
      "Max Number of Words:",
      min = 1,
      max = 300,
      value = 100
    ),
    h3("Available Years"),
    
    h5(helpText("Years avaiable for China:")),
    h6(
      helpText("UNGD: 1971-2017"),
      helpText("SOTU: 1956 1969 1973 1977 1982 1987 1992 1997 2002 2007 2012 2017")),
    
    h5(helpText("Years available for Ghana:")),
    h6(helpText("UNGD: 1970-2017"),
       helpText("SOTU: 2008, 2011-2018")), 
    
    h5(helpText("Years available for the Phillipines:")),
    h6(helpText("UNGD: 1970-2017"),
       helpText("SOTU: 1935-2018")),
    
    h5(helpText("Years avaiable for Russia:")),
    h6(helpText("UNGD: 1971-2017"),
       helpText("SOTU: 2000-2018")),
    
    h5(helpText("Year available for the United States:")),
    h6(helpText("UNGD: 1970-2017"),
       helpText("SOTU: 1913-2018")),
    
    h5(helpText("Year available for South Africa:")),
    h6(helpText("UNGD: 1970-2017"),
       helpText("SOTU: 1990, 1994-2018")),
    
    # Main panel for displaying outputs
    mainPanel(plotOutput("plot"),
              textOutput("type"),
              textOutput("dates"))
  )
)


server <- function(input, output, session) {
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    minyear <- input$yrs[1]
    maxyear <- input$yrs[2]
    req(input$selection)
    req(input$yrs[1])
    req(input$yrs[2])
    input$update
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection, input$type, minyear, maxyear)
      })
    })
  })
  
  
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  }, height = 500, width = 600)
}


shinyApp(ui, server)

