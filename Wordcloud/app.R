#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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

ui <- pageWithSidebar(
  # App Title ---
  headerPanel("Speeches Wordclouds"),
  
  # Sidebar panel for inputs ---
  sidebarPanel(
    selectPanel(
      selectInput("selection", "Choose a country:",
                  choices = countries),
      actionButton("update", "Change"),
      hr(),
      sliderInput(
        "freq",
        "Minimum Frequency:",
        min = 1,
        max = 50,
        value = 15
      ),
      sliderInput(
        "max",
        "Maximum Number of Words:",
        min = 1,
        max = 300,
        value = 100
      )
    )
  ),
  
  # Show Word Cloud
  mainPanel(plotOutput("plot"))
)


# Define server logic to plot various variables 
# This needs to be able to create the wordclouds???

server <- function(input, output){
  terms <- reactive({
    input$update
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection)
      })
    })
  })
}

# Make the wordcloud drawing predictable during a session
wordcloud_rep <- repeatable(wordcloud)

output$plot <- renderPlot({
  v <- terms()
  wordcloud_rep(names(v), v, scale = c(4, 0.5),
                min.freq = input$freq, max.words = input$max,
                colors = brewer.pal(8, "Dark2"))
})


# Run the application 
shinyApp(ui = ui, server = server)

runApp("~/shinyapp")
