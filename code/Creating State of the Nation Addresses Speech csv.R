# Load Prereq's -----------------------------------------------------------

packages <- c("XML", "tidyverse", "RCurl", "xtable", "rvest", "stringr", "hms", "lubridate", "tidytext", "qdapTools")

lapply(packages, library, character.only = T)


# Grab Dates for the Speeches -----------------------------------------------------------

sona_table <- url %>%
  html_nodes("td") %>% 
  html_text()

# Convert to tibble
sona_tb <- as.tibble(sona_table)

# Parse dates
sona_dates <- parse_date(sona_tb$value, "%B %d, %Y")

# Parsing missed a date so manually entering it in
sona_dates[328] <- as.Date("2016-07-25")
sona_dates <- sona_dates %>% 
  na.omit()

# Get Unique HTML Links ---------------------------------------------------

url <- "https://www.officialgazette.gov.ph/past-sona-speeches/"

url <- read_html(url)

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
t1 <- urls[1:3]

for(i in t1[1]){
  test_test <- scrape_it(i)
  test_test <- as.data.frame(test_test)
  for(d in sona_dates)
    for(l in length(i))
}
