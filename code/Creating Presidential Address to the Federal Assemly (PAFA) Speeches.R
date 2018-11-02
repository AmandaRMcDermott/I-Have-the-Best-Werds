
# Prereqs -----------------------------------------------------------------

#Load libraries

packages <- c("XML", "tidyverse", "RCurl", "xtable", "rvest", "stringr", "hms", "lubridate", "tidytext")
lapply(packages, library, character.only = T)

# Webscraping -------------------------------------------------------------

url <- 'http://en.kremlin.ru/events/president/transcripts/messages'

link <- read_html(url)


# Dates for Speeches ------------------------------------------------------

#Read in Dates
vec <- link %>% 
  html_nodes("span") %>% 
  html_nodes("time") %>% 
  html_text()

# Transform into a df
vec <- as.data.frame(vec)

# Take only the needed dates of the speeches
vec <- vec %>% 
  slice(c(2, 4, 8, 13, 15, 19, 21, 23, 25))

# Parse date and time
vec$vec <- parse_date_time(vec$vec, "b! d!, Y!, H!:M!")

# Remove time since it is unnecessary
r_date <- vec %>% 
  separate(vec, into = c("date", "delete"), sep = " ") %>% 
  select(date)


# Text from Speeches ------------------------------------------------------

#Get unique html links
html_codes <- link %>% 
  html_nodes("h3") %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  gsub("[//.eventspridacmag]", "", .)

# Get only the html codes for pafa speeches
(html_codes <- html_codes[c(1, 2, 4, 6, 7, 8, 9, 10, 11, 12)])

# Put all URLs in one object
urls <- c("http://en.kremlin.ru/events/president/transcripts/copy/56957", 
         "http://en.kremlin.ru/events/president/transcripts/copy/53379", 
         "http://en.kremlin.ru/events/president/transcripts/copy/50864",
         "http://en.kremlin.ru/events/president/transcripts/copy/47173",
         "http://en.kremlin.ru/events/president/transcripts/copy/19825",
         "http://en.kremlin.ru/events/president/transcripts/copy/17118",
         "http://en.kremlin.ru/events/president/transcripts/copy/14088",
         "http://en.kremlin.ru/events/president/transcripts/copy/9637",
         "http://en.kremlin.ru/events/president/transcripts/copy/5979",
         "http://en.kremlin.ru/events/president/transcripts/copy/1968")

# Create a function that takes urls and scraps out the text
scrap_it <- function(url){
  read_url <- read_html(url) 
  read_url %>% 
    html_node("textarea#special_textarea") %>% 
    html_text() %>% 
    gsub("[\n<>]", "", .) %>% 
    print() # Shows which speech its on
}

# Scrap all pafa speeches and output a list to be converted into a df
pafa <- lapply(urls, scrap_it)

# Clean up for Putin ------------------------------------------------------

# Convert into a df to prepare for separation of introductory text (not part of pafa) in speeches
pafa <- data.frame(matrix(unlist(pafa), nrow = 10, byrow = T), stringsAsFactors = F)

# Rename column
pafa <- pafa %>% 
  rename(text = "matrix.unlist.pafa...nrow...10..byrow...T.")

# Split text
split_pafa <- strsplit(pafa$text, "Putin:", fixed = T)

# Convert to a dataframe again and rename
better_split_pafa <- data.frame(matrix(unlist(split_pafa), nrow = 16, byrow = T), stringsAsFactors = F)

final_split_pafa <- better_split_pafa %>% 
  rename(text = "matrix.unlist.split_pafa...nrow...16..byrow...T.")

# Take rows that now only contain the speeches and no introdutory manner
clean_putin_pafa <- final_split_pafa %>% 
  slice(c(2, 4, 6, 8, 10, 12, 13, 14, 15, 16))

# Clean up for Medvedev ------------------------------------------------------

# Split text
t1 <- strsplit(clean_putin_pafa$text, "Medvedev:", fixed = T)

# Convert to a dataframe again and rename
t2 <- data.frame(matrix(unlist(t1), nrow = 14, byrow = T), stringsAsFactors = F)

t3 <- t2 %>% 
  rename(text = "matrix.unlist.t1...nrow...14..byrow...T.")

# Take rows that now only contain the speeches and no introdutory manner
t4 <- t3 %>%
  slice(c(1, 2, 3, 4, 5, 6, 8, 10, 12, 14))

# Add column and entries for years speeches were give
t4$year <- c(2018, 2016, 2015, 2014, 2013, 2012, 2011, 2010, 2009, 2008)

# Add country column and Russia label
t4$country <- "Russia"

# Add PAFA context column
t4$context <- "PAFA"

# Rename dataframe as "pafa"
pafa <- t4

# Separate sentences from each other in each speech for each speaker
pafa_tidy <- pafa %>% 
  unnest_tokens(text, text, token = "sentences") %>%
  select(country, year, text, context)

# Write csv
write_csv(pafa_tidy, "pafa.csv")

