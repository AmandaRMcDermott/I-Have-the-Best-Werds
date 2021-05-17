main_guide <- Cicerone$
  new(id="main-tutorial",
    padding=0,
    opacity = 1
  )$ 
  step(
    el="test",
    title = "Welcome to the text tool.",
    description = "This intro provides a short guide to help orient you around the tool and how to use it.",
    is_id = T,
    position="mid-center"
  )$
  step(
    el = "korpuskart",
    title = "Corpus chart",
    description = paste0("The corpus chart gives you a basic overview of documents. 
                        Each cell represents a document. 
                        Wider cells are longer documents, while narrower ones are shorter.",
                        br(),
                        "Click a cell to make the corresponding document pop up."),
    position="right-center"
  )$
  step(
    el="antall_linjer",
    title="Highlighting terms",
    description="Select the number of terms to search the corpus for."
  )$
  step(
    el="search_text_1",
    title="Entering terms",
    description="Type in a term or phrase to search for and then press ENTER on your keyboard. A new chart with the search terms will appear."
  )$
  step(
    el="more_terms_button",
    title="Additional terms",
    description="If you need to search for more terms, you may enter them here but they will not appear in the legend."
  )$
  step(
    el="subset_corpus",
    title="Filtering the corpus",
    description="To only include documents that contain certain words, enter them here where each is separated by a new line. Click the SEARCH button to then filter."
  )$
  step(
    el="adjust_plotsize",
    title="Changing the plot size",
    description="Click this button and then use the slider to change the width of the plot by clicking ADJUST SIZE."
  )


# Replay guide ------------------------------------------------------------

replay_guide <- Cicerone$
  new(id="replay-tutorial",
      padding=0,
      opacity = 1
  )$
  step(
    el = "korpuskart",
    title = "Corpus chart",
    description = paste0("The corpus chart gives you a basic overview of documents. 
                        Each cell represents a document. 
                        Wider cells are longer documents, while narrower ones are shorter.",
                         br(),
                         "Click a cell to make the corresponding document pop up."),
    position="right-center"
  )$
  step(
    el="antall_linjer",
    title="Highlighting terms",
    description="Select the number of terms to search the corpus for."
  )$
  step(
    el="search_text_1",
    title="Entering terms",
    description="Type in a term or phrase to search for and then press ENTER on your keyboard. A new chart with the search terms will appear."
  )$
  step(
    el="additional_search_terms",
    title="Additional terms",
    description="If you need to search for more terms, you may enter them here but they will not appear in the legend."
  )