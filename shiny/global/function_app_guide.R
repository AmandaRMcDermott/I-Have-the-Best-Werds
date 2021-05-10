guide <- Cicerone$
  new()$ 
  step(
    el = "corpus_box",
    title = "Text Input",
    description = "This is where you enter the text you want to print."
  )$
  step(
    el = "antall_linjer",
    title = "Text Input",
    description = "This is where you enter the text you want to print."
  )$
  step(
    "search_text_1",
    "Send the Text",
    "Send the text to the server for printing"
  )