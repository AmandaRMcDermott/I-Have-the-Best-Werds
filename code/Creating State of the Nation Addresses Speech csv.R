# Load Prereq's -----------------------------------------------------------

packages <- c("XML", "tidyverse", "RCurl", "xtable", "rvest", "stringr", "hms", "lubridate", "tidytext", "qdapTools")

lapply(packages, library, character.only = T)


# Grab Dates for the Speeches -----------------------------------------------------------

url <- "https://www.officialgazette.gov.ph/past-sona-speeches/"

url <- read_html(url)

sona_table <- url %>%
  html_nodes("td") %>% 
  html_text()

# Convert to tibble
sona_tb <- as.tibble(sona_table)

# Parse dates
sona_dates <- parse_date(sona_tb$value, "%B %d, %Y")

# Parsing missed a date so manually entering it in
sona_dates[328] <- as.Date("2016-07-25")

#Convert to a dataframe; separate year from month and day; select only the year column
sona_dates <- as.data.frame(sona_dates)
sona_dates <- sona_dates %>% 
  na.omit() %>% 
  separate(sona_dates, into = c("year", "month", "day"), sep = "-", convert = F) %>%
  select(year)

# Get Unique HTML Links ---------------------------------------------------


urls <- url %>% 
  html_nodes("td+ td a") %>% 
  html_attr("href")

# Specify Duterte Link since original one didn't go directly to the speech
urls[78] <- "http://www.officialgazette.gov.ph/2016/07/26/the-2016-state-of-the-nation-address/"

# Function to read in speeches
scrape_it <- function(url){
  read_url <- read_html(url)
  read_url %>% 
    html_nodes("p~ p+ p") %>% 
    html_text() %>% 
    gsub("[\n<>\t]", "", .) %>% 
    print()
}

# Test - Incomplete 
for(i in 1:length(urls)){
  test_test <- scrape_it(urls[i])
  #indic<- as.character(i + 1934)
  write.csv(test_test, paste("PHL_", sona_dates[i,1], ".txt", sep = ""))
}

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
