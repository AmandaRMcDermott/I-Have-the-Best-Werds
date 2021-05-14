guide <- Cicerone$
  new()$ 
  step(
    ".content-wrapper",
    title = "Text Input",
    description = "This is where you enter the text you want to print.",
    is_id = F
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