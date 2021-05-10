shinydashboard::dashboardHeader(
  #use_cicerone(), # include dependencies
    title = shiny::div('Corpus exploration tool', class = "tittel"),
    shinydashboard::dropdownMenuOutput("dropdownmenu")
)
