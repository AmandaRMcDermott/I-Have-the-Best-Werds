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