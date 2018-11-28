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

countries <<- list("China" = "CHN", "Ghana" = "GHN", "Phillipines" = "PHL", "Russia" = "RUS", "United States" = "USA", "South Africa" = "ZAF")

type_speech <<- list("UN General Debates" = "UNGD", "State of the Union" = "SOTU")

# Cache the results
getTermMatrix <- memoise(function(ctry, type) {
  #if (!(country %in% countries))
  #stop("Unknown country")
  if(missing(type)){
    text <- clean_speeches %>% 
      filter(country == ctry) %>% 
      select(text)
  }
  else{
    text <- clean_speeches %>%
      filter(country == ctry, context == type) %>%
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
                choices = countries),
    actionButton("update", "Change"),
    hr(),
    selectInput("type", "Type of Speech",
                       choices = type_speech),
    sliderInput("freq",
                "Min Freq:",
                min = 1, max = 50, value = 15),
    sliderInput("max",
                "Max Number of Words:",
                min = 1, max = 300, value = 100)
  ),
  
  # Main panel for displaying outputs
  mainPanel(
    plotOutput("plot"),
    textOutput("type"))
)


# Server
server <- function(input, output, session) {
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    req(input$selection)
    input$update
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        if(!is.null(input$type))
          getTermMatrix(input$selection)
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
  })
  #output$value <- renderText({input$checkGroup})
}

shinyApp(ui, server)

