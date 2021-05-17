#=============================================================================#
####================================SERVER=================================####
#=============================================================================#

server <- function(input, output, session) {
  
  # initialise then start the guide
  main_guide$init()$start()
  
  # Session scope function files --------------------------------------------
  source("./shiny/server/functions_collect_input_terms.R", local = TRUE)
  source("./shiny/server/functions_checking_input.R", local = TRUE)
  source("./shiny/server/functions_ui_management.R", local = TRUE)
  source("./shiny/server/function_collect_edited_info_plot_legend_keys.R", local = TRUE)
  
  # Conditional and customised sidebar UI elements --------------------------
  source("./shiny/ui/render_ui_sidebar_checkbox_filtering.R", local = TRUE)
  source("./shiny/ui/render_ui_sidebar_date_filtering.R", local = TRUE)
  source("./shiny/ui/hide_ui_sidebar_plot_mode.R", local = TRUE)
  source("./shiny/ui/set_colours_in_search_fields.R", local = TRUE)
  
  # output$dropdownmenu<- renderMenu({
  #   shinyjs::onclick(id="dropdown",expr = replay_guide$start())
  # })
  
  observeEvent(input$click_help,{
    replay_guide$start()
  })
  
  # Session variables -------------------------------------------------------
  source("./shiny/server/session_variables.R", local = TRUE)
  
  # For use with potential "extra" plugins ----------------------------------
  if (INCLUDE_EXTRA == TRUE) {
    source("./shiny/extra/extra_render_ui_sidebar_magic_filtering.R", local = TRUE)
    source("./shiny/extra/extra_tab_content.R", local = TRUE)
    source("./shiny/extra/extra_ui_management_functions.R", local = TRUE)
    source("./shiny/extra/extra_session_variables.R", local = TRUE)
  }
  
  # Corpus info tab ---------------------------------------------------------
  source("./shiny/server/corpus_info_tab.R", local = TRUE)
  
  
  # 1. Startup actions ------------------------------------------------------
  source("./shiny/server/1_startup_actions.R", local = TRUE)
  
  # 2. Event: search button -------------------------------------------------
  source("./shiny/server/2_event_search_button.R", local = TRUE)
  
  # 3. Event: click in corpus map -------------------------------------------
  source("./shiny/server/3_event_corpus_map_click.R", local = TRUE)
  
  # 4. Event: click in day map ----------------------------------------------
  source("./shiny/server/4_event_day_map_click.R", local = TRUE)
  
  # 5. Event: click in document visualisation -------------------------------
  source("./shiny/server/5_event_document_visualisation_click.R", local = TRUE)
  
  # 6. Event: hovering in corpus map ----------------------------------------
  source("./shiny/server/6_event_hover_corpus_map.R", local = TRUE)
  
  # 7. Event: update plot size ----------------------------------------------
  source("./shiny/server/7_event_plot_size_button.R", local = TRUE)
  
  # Cleaning up the session -------------------------------------------------
  shiny::onSessionEnded(function() {
    shiny::shinyOptions("corporaexplorer_data" = NULL)
    shiny::shinyOptions("corporaexplorer_search_options" = NULL)
    shiny::shinyOptions("corporaexplorer_ui_options" = NULL)
    shiny::shinyOptions("corporaexplorer_input_arguments" = NULL)
    shiny::shinyOptions("corporaexplorer_plot_options" = NULL)
    shiny::shinyOptions("corporaexplorer_extra" = NULL)
  })
}