library(shiny)
library(tm)
library(wordcloud)
library(memoise)
library(tidyverse)
library(tidytext)


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



shinyApp(ui, server)
