library(tidyverse)
###Reading in the files
files = list.files(pattern="*.txt")
fls <- NULL
lns <- NULL
for (file in files) {
  my_lines <- readLines(file)
  print(file)
  for (line in my_lines) {
    fls <- c(fls, file)
    lns <- c(lns, line)
  }
}

###Dataframe stuff.
df <- data.frame(file=fls, text =lns)
new_df <- df %>% 
  separate(file, into = c("country", "delete_me", "year"), sep = "_") 
newer_df <- new_df %>% 
  mutate(year = gsub(".txt", "", year))
newer_df$delete_me <-NULL 
newer_df$context <- "UNGD"

write_csv(newer_df, "USUNGD.csv")
