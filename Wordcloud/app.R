library(shiny)
library(tm)
library(wordcloud)
library(memoise)
library(tidyverse)
library(tidytext)

# Global
speeches <- read_csv("https://raw.githubusercontent.com/Glacieus/GOVT-696-Project-Jang-McDermott/master/data/speeches.csv")
countries <<- list("China" = "CHN", "Ghana" = "GHN", "Phillipines" = "PHL", "Russia" = "RUS", "United States" = "USA", "South Africa" = "ZAF")

# Cache the results
getTermMatrix <- memoise(function(country){
  if (!(country %in% countries))
    stop("Unkown country")
  
  myCorpus = Corpus(VectorSource(speeches))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus)
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
                min = 1, max = 50, value = 15)
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
        getTermMatrix(input$selection)
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
