# Reading in the files
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

# Convert to a data frame
df <- data.frame(file = fls, text = lns)


#---------- Do the above for all speeches --------------------------------


# Clean phl_sotu df
phl_sotu <- df %>% 
  mutate(text = gsub("\"[0-9]+\",", "", text)) %>% 
  separate(file, into = c("country", "year"), sep = "_") %>% 
  mutate(year = gsub(".txt", "", year))

phl_sotu$context <- "SOTU"

# Clean phl_ungd df
phl_ungd <- df %>%
  mutate(text = gsub("\\d+\\.", "", text)) %>% 
  separate(file, into = c("country", "delete", "year"), sep = "_") %>% 
  mutate(year = gsub(".txt", "", year)) %>% 
  select(country, year, text)

phl_ungd$context <- "UNGD"

# Clean chn_sotu df
chn_sotu <- df %>% 
  separate(file, into = c("country", "year"), sep = " ") %>% 
  mutate(year = gsub(".txt", "", year)) %>% 
  mutate(text = str_squish(text))

chn_sotu$country <- "CHN"
chn_sotu$context <- "SOTU"

# Clean chn_ungd df
chn_ungd <- df %>% 
  mutate(text = gsub("\\d+\\.", "", text)) %>% 
  separate(file, into = c("country", "delete", "year"), sep = "_") %>% 
  mutate(year = gsub(".txt", "", year)) %>% 
  select(country, year, text)

chn_ungd$context <- "UNGD"

# Clean zaf_sotu
zaf_sotu <- df %>% 
  separate(file,into = c("year", "delete"), sep = "_") %>% 
  select(year, text)

zaf_sotu$country <- "ZAF"
zaf_sotu$context <- "SOTU"
  
# Clean zaf_ungd
zaf_ungd <- df %>% 
  mutate(text = gsub("\\d+\\.", "", text)) %>% 
  separate(file, into = c("country", "delete", "year"), sep = "_") %>% 
  mutate(year = gsub(".txt", "", year)) %>% 
  select(country, year, text)

zaf_ungd$context <- "UNGD"

# Clean gha_sotu
gha_sotu <- df %>% 
  separate(file, into = c("delete", "year"), sep = "_") %>% 
  mutate(year = gsub(".txt", "", year)) %>% 
  select(year, text)

gha_sotu$country <- "GHA"
gha_sotu$context <- "SOTU"

# Clean gha_ungd
gha_ungd <- df %>% 
  mutate(text = gsub("\\d+\\.", "", text)) %>% 
  separate(file, into = c("country", "delete", "year"), sep = "_") %>% 
  mutate(year = gsub(".txt", "", year)) %>% 
  select(country, year, text)
  
gha_ungd$context <- "UNGD"  

# Rbind all the speeches
speeches <- rbind(chn_sotu, chn_ungd, gha_sotu, gha_ungd, phl_sotu, phl_ungd, rus_speeches, us_speeches, zaf_sotu, zaf_ungd)
speeches <- speeches %>% 
  na.omit()

# Clean and add on rus_00_07
rus_00_07$country <- "RUS"
rus_00_07$context <- "SOTU"
speeches <- rbind(speeches, rus_00_07)

# Since some of the SOTU speeches from the Phillipines were in Phillipino, these were translated into English and need to be recombined with the speeches.csv
# Remove SOTU Phillipines observations
speeches <- speeches %>% 
  filter(country != "PHL", context != "SOTU")

phl_sotu <- df %>% 
  mutate(text = gsub("\"[0-9]+\",", "", text)) %>% 
  separate(file, into = c("country", "year"), sep = "_") %>% 
  mutate(year = gsub(".txt", "", year))
  
phl_sotu$context <- "SOTU"

speeches <- rbind(speeches, phl_sotu)
speeches <- speeches %>% na.omit

# Write csv
write_csv(speeches, "speeches.csv")
