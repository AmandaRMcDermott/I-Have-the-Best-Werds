navbarPage(
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
