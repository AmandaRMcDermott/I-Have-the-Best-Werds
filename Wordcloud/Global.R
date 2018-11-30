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
         text = map_chr(text, paste, collapse = " ")) %>% 
  select(country, year, context, text)


#countries <<- list("China" = clean_speeches$country[clean_speeches$country == "CHN"], "Ghana" = clean_speeches$country[clean_speeches$country == "GHN"],"Phillipines" = clean_speeches$country[clean_speeches$country == "PHL"],"Russia" = clean_speeches$country[clean_speeches$country == "RUS"],"United States" = clean_speeches$country[clean_speeches$country == "USA"],"South Africa" = clean_speeches$country[clean_speeches$country == "ZAF"])


# Cache the results
getTermMatrix <- memoise(function(ctry) {
  #if (!(country %in% countries))
  #stop("Unkown country")
  myCorpus = Corpus(VectorSource(clean_speeches$text[clean_speeches$country == ctry]))
  #myCorpus = tm_map(myCorpus, content_transformer(tolower))
  #myCorpus = tm_map(myCorpus, removePunctuation)
  #myCorpus = tm_map(myCorpus, removeNumbers)
  #myCorpus = tm_map(myCorpus, stripWhitespace)
  
  myDTM = TermDocumentMatrix(
    myCorpus,
    control = list(
      minWordLength = 1,
      wordLengths = c(0, Inf),
      removePunctuation = TRUE,
      removeNumbers = TRUE
    ))
    #myDTM = removeSparseTerms(myDTM, 0.9)
    
    m = as.matrix(myDTM)
    
    sort(rowSums(m), decreasing = T)
})


# Define ui
ui <- fluidPage(
  # App title
  headerPanel("Speeches Wordclouds"),
  
  # Sidebar for inputs
  sidebarPanel(
    selectInput("select", "Choose a country:",
                choices = countries),
    actionButton("update", "Change"),
    hr(),
    sliderInput("freq",
                "Min Freq:",
                min = 1, max = 50, value = 15),
    sliderInput("max",
                "Max Number of Words:",
                min = 1, max = 300, value = 100)
  ),
  
  # Main panel for displaying outputs
  mainPanel(
    plotOutput("plot")
  )
)

# Server
server <- function(input, output) {
  terms <- reactive({
    input$update
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(countries[input$selection])
        #req(countries[input$selection])
      })
    })
  })
  
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale = c(4, 0.5),
                  min.freq = input$freq, max.words = input$max, 
                  colors = brewer.pal(8, "Dark2"))
  })
}

shinyApp(ui, server)