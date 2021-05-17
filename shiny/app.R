# Libraries ---------------------------------------------------------------
require(shiny)
require(tidyverse)
require(cicerone)
require(data.table)


# Function files ----------------------------------------------------------
source("./shiny/global/function_display_document.R", local = TRUE)
source("./shiny/global/function_display_document_info.R", local = TRUE)
source("./shiny/global/function_filter_corpus.R", local = TRUE)
source("./shiny/global/function_find_row_in_df.R", local = TRUE)
source("./shiny/global/function_format_date.R", local = TRUE)
source("./shiny/global/function_insert_doc_links.R", local = TRUE)
source("./shiny/global/function_plot_size.R", local = TRUE)
source("./shiny/global/function_visualise_corpus.R", local = TRUE)
source("./shiny/global/function_visualise_document.R", local = TRUE)
source("./shiny/global/functions_info.R", local = TRUE)
source("./shiny/global/functions_main_search_engine.R", local = TRUE)
source('./shiny/global/function_app_guide.R', local = TRUE)

# Setting up data and config ----------------------------------------------
if (!is.null(getOption("shiny.testmode"))) {
  if (getOption("shiny.testmode") == TRUE) {
    source("./shiny/config/config_tests.R", local = TRUE)
  }
} else {
  source("./shiny/config/config.R", local = TRUE)
}

#=============================================================================#
####================================  UI  =================================####
#=============================================================================#

ui <- function(id) {

div(id="test",
  shinydashboard::dashboardPage(
    
    title = 'Corpus exploration',
    
    # Header --------------------------------------------------------------
    source("./shiny/ui/ui_header.R", local = TRUE)$value,
    
    # Sidebar -------------------------------------------------------------
    source("./shiny/ui/ui_sidebar.R", local = TRUE)$value,
    
    # Body ----------------------------------------------------------------
    
    shinydashboard::dashboardBody(
      
     use_cicerone(),
      
      # CSS and JS files --------------------------------------------------
      source("./shiny/ui/css_js_import.R", local = TRUE)$value,
      source("./shiny/ui/css_from_arguments.R", local = TRUE)$value,
      
      # Fluid row ---------------------------------------------------------
      
      shiny::fluidRow(

        # Corpus map/corpus info box -------------------------------
        source("./shiny/ui/ui_corpus_box.R", local = TRUE)$value,
        
        # A day in the corpus box (for data_365) -------------------
        source("./shiny/ui/ui_day_in_corpus_box.R", local = TRUE)$value,
        
        # Document box ---------------------------------------------
        source("./shiny/ui/ui_document_box.R", local = TRUE)$value
        
        # Fluid row ends
      )
      
      # shinyjs
      ,
      shinyjs::useShinyjs(),
      shinyWidgets::useSweetAlert()
      
      # Body ends
    )
    # Page ends
  ),
)
}

#=============================================================================#
####================================SERVER=================================####
#=============================================================================#

server <- function(input, output, session) {
  
  # initialise then start the guide
  guide$init()$start()
  
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
  output$dropdownmenu<- renderMenu({
    shinyjs::onclick(id="dropdown",guide$init()$start())
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

# Run app -----------------------------------------------------------------
shiny::shinyApp(ui, server)

