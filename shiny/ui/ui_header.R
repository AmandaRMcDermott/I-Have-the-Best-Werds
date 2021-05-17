shinydashboard::dashboardHeader(
  #use_cicerone(), # include dependencies
    title = shiny::div('Corpus exploration tool', class = "tittel"),
    shinydashboard::dropdownMenuOutput("dropdownmenu"),
    tags$li(class="dropdown",
    shinydashboard::dropdownMenu(
                 type="messages",icon=icon("question-sign", lib = "glyphicon"),headerText = "",
                 #tags$li(class="dropdown",actionButton(inputId = "click-help",label="Replay")))
                                   messageItem(
                                   from = "Help",
                                   message = "Replay tutorial",
                                   icon = icon("question")
                                 ))
    )
)
