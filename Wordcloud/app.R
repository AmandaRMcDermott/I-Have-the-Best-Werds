library(shiny)
library(tm)
library(wordcloud)
library(memoise)
library(tidyverse)
library(tidytext)
library(shinythemes)
library(readr)

# Global
clean_speeches <- read_csv("https://raw.githubusercontent.com/Glacieus/GOVT-696-Project-Jang-McDermott/master/data/clean_speeches.csv")

ctry <<- list("China" = "CHN", "Ghana" = "GHA", "Philipines" = "PHL", "Russia" = "RUS", "United States" = "USA", "South Africa" = "ZAF")

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



ui <- navbarPage(
  "Speeches Wordclouds",
  theme = shinytheme("superhero"),
  tabPanel(
    "Wordcloud",
    plotOutput('plot', width = "auto", height = "auto"),
    fluidRow(
      column(
        3,
        selectInput("selection", "Choose a country:",
                    choices = ctry),
        actionButton("update", "Change"),
        hr(),
        radioButtons("type", "Type of Speech",
                     choices = type_speech)
      ),
      column(
        4,
        offset = 1,
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
        )
      ),
      column(
        4,
        h3("Available Years"),
        
        h5(helpText("Years avaiable for China:")),
        h6(
          helpText("UNGD: 1971-2017"),
          helpText("SOTU: 1956 1969 1973 1977 1982 1987 1992 1997 2002 2007 2012 2017")
        ),
        
        h5(helpText("Years available for Ghana:")),
        h6(helpText("UNGD: 1970-2017"),
           helpText("SOTU: 2008, 2011-2018")),
        
        h5(helpText("Years available for the Phillipines:")),
        h6(helpText("UNGD: 1970-2017"),
           helpText("SOTU: 1935-2018")),
        
        h5(helpText("Years avaiable for Russia:")),
        h6(helpText("UNGD: 1971-2017"),
           helpText("SOTU: 2000-2018")),
        
        h5(helpText("Years available for the United States:")),
        h6(helpText("UNGD: 1970-2017"),
           helpText("SOTU: 1913-2018")),
        
        h5(helpText("Years available for South Africa:")),
        h6(helpText("UNGD: 1970-2017"),
           helpText("SOTU: 1990, 1994-2018"))
      )
    )
  ),
  tabPanel(
    "Comparison Wordclouds",
    plotOutput('plot2', width = "auto", height = "600px"),
    fluidRow(
      column(
        3,
        selectInput("selection3", "Choose Country 1",
                    choices = ctry),
        hr(),
        radioButtons("type3", "Type of Speech for Country 1",
                     choices = type_speech),
        hr(),
        selectInput("selection4", "Choose Country 2",
                    choices = ctry2),
        hr(),
        radioButtons("type4", "Type of Speech for Country 2",
                     choices = type_speech),
        actionButton("update", "Change")
      ),
      column(
        4,
        offset = 1,
        sliderInput("yrs2", "Years", 1913, 2018, value = c(2000, 2014)),
        sliderInput(
          "freq2",
          "Min Freq:",
          min = 1,
          max = 50,
          value = 15
        ),
        sliderInput(
          "max2",
          "Max Number of Words:",
          min = 1,
          max = 300,
          value = 100
        )
      ),
      column(
        4,
        h3("Available Years"),
        
        h5(helpText("Years avaiable for China:")),
        h6(
          helpText("UNGD: 1971-2017"),
          helpText("SOTU: 1956 1969 1973 1977 1982 1987 1992 1997 2002 2007 2012 2017")
        ),
        
        h5(helpText("Years available for Ghana:")),
        h6(helpText("UNGD: 1970-2017"),
           helpText("SOTU: 2008, 2011-2018")),
        
        h5(helpText("Years available for the Phillipines:")),
        h6(helpText("UNGD: 1970-2017"),
           helpText("SOTU: 1935-2018")),
        
        h5(helpText("Years avaiable for Russia:")),
        h6(helpText("UNGD: 1971-2017"),
           helpText("SOTU: 2000-2018")),
        
        h5(helpText("Years available for the United States:")),
        h6(helpText("UNGD: 1970-2017"),
           helpText("SOTU: 1913-2018")),
        
        h5(helpText("Years available for South Africa:")),
        h6(helpText("UNGD: 1970-2017"),
           helpText("SOTU: 1990, 1994-2018"))
      )
    )
  )
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
  
  terms2 <- reactive({
    req(input$selection3)
    req(input$selection4)
    minyear <- input$yrs2[1]
    maxyear <- input$yrs2[2]
    req(input$type3)
    req(input$type4)
    req(input$yrs2[1])
    req(input$yrs2[2])
    input$update
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        get_comp_comm(input$selection3, input$selection4, input$type3, input$type4, minyear, maxyear)
      })
    })
  })
  
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  wordcloud_comp_rep <- repeatable(comparison.cloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(
      names(v),
      v,
      scale = c(5, 1),
      min.freq = input$freq,
      max.words = input$max,
      colors = brewer.pal(8, "Set1")
    )
  }, height = 700)
  
  
  output$plot2 <- renderPlot({
    v2 <- terms2()
    wordcloud_comp_rep(
      v2,
      scale = c(3, 1),
      min.freq = input$freq2,
      max.words = input$max2,
      #title.size = 1.5,
      colors = c("#6600CC", "red")
    )
  }, height = 525)
}

runGitHub("Glacieus/GOVT-696-Project-Jang-McDermott/Wordcloud", "Glacieus")

shinyApp(ui, server)
runApp("app.R")



