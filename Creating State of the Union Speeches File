#Load libraries
library(XML)
library(tidyverse)
library(tm)
library(corpus)
library(document)
library(xts)
library(infer)
library(RCurl)
library(xtable)

# Parsing of HTML/XML files
library(rvest)

# Verbose regular expression
library(rebus)

#String manipulation
library(stringr)


# Find links to each speech

url <- 'http://stateoftheunion.onetwothree.net/texts/index.html'

first_page <- read_html(url)

# Get unique speech names and dates
html_names_date <- first_page%>% 
  html_nodes("div#content") %>% 
  html_nodes("div#text") %>% 
  html_nodes("ul") %>% 
  html_nodes("a") %>% 
  html_text()

# Get unique html links
html_codes <- first_page%>% 
  html_nodes("div#content") %>% 
  html_nodes("div#text") %>% 
  html_nodes("ul") %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  gsub("[//.html]", "", .)

#Loop
sotu <- data.frame(matrix(nrow=1,ncol=3))
colnames(sotu) = c("speechtext","year","date")

speechtext <- function(ymd){
  for(i in 1:length(ymd)){
    year <- substr(ymd[i],1,4)
    url <- paste0('http://stateoftheunion.onetwothree.net/texts/',ymd[i],'.html')
    doc.html = htmlTreeParse(url, useInternal = TRUE)
    
    doc.text = unlist(xpathApply(doc.html, '//p', xmlValue))
    
    # Replace all newline notation with spaces and all quotes with nothing
    doc.text = gsub('\\n', ' ', doc.text)
    doc.text = gsub('\\"', '', doc.text)
    
    doc.text = paste(doc.text, collapse = ' ')
    
    x <- data.frame(doc.text, year, ymd[i], stringsAsFactors = FALSE)
    names(x) <- c("speechtext","year","date")
    sotu <- rbind(sotu, x)
    sotu <- sotu[!is.na(sotu$speechtext), ]
  }
  return(sotu)
}

# Make speech text
sotu <- speechtext(html_codes)

# Make html_names_date rownames of sotu
row.names(sotu_full) <- html_names_date

# Delete html codes from 1790 to 1913 (starting at Woodrow Wilson's first speech)
sotu <- sotu[-c(1:124),]

# Full speech document
sotu_full <- data.frame(matrix(nrow=1,ncol=3))
colnames(sotu_full) = c("speechtext","year","date")
sotu_full <- speechtext(html_codes)
sotu_full <- sotu_full[-c(1:108),]

# Write CSV files
write.csv(sotu, "sotu.csv")
write.csv(sotu_full, "sotu_full.csv")
